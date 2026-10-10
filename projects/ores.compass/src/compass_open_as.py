"""compass open-as -- open the web in an isolated Chrome, signed in as an Acme user.

Testing the web as different Acme staff should not add profiles to the
developer's own Chrome. Each persona gets its own --user-data-dir under
~/.cache/ores-personas, which Chrome treats as a separate instance: it never
appears in the profile picker and never syncs. The sign-in form is filled
through the DevTools protocol, so only Sign in is left to press.

Acme is a fixed seeded tenant, so the tenant, the shared password, the
locations and the titles are hard coded. The staff come from the seeder
dataset, the same file the provisioning loads.

Stdlib only: the DevTools socket speaks a small subset of RFC 6455.
"""

import argparse
import base64
import difflib
import json
import os
import re
import shutil
import socket
import struct
import subprocess
import sys
import time
import urllib.request
from pathlib import Path

TENANT_HOSTNAME = "acme_corporation"
PASSWORD = "Secure-Password-123"
DEFAULT_LOCATION = "london"

ACCOUNTS_REL = Path("projects/ores.seeder/datasets/acme_corporation/accounts.json")

# Each office is a business unit prefix in the dataset.
LOCATIONS = {
    "london": "acme_uk", "uk": "acme_uk", "ldn": "acme_uk",
    "new york": "acme_us", "ny": "acme_us", "nyc": "acme_us", "us": "acme_us",
    "hong kong": "acme_hk", "hk": "acme_hk", "hkg": "acme_hk",
}

ROLES = {"trading": "Trading", "trader": "Trading", "operations": "Operations",
         "ops": "Operations", "viewer": "Viewer"}

# A short word picks every title that contains it, so "trader" is both the
# senior and the junior one. The group head has no office and ignores location.
TITLES = [
    "Group Chief Executive Officer", "Country Head", "COO", "Head of Trading",
    "Head of Risk", "Head of Market Risk", "Head of Middle Office",
    "Head of Desk", "Senior Trader", "Junior Trader", "Senior Analyst",
    "Junior Analyst",
]

PERSONAS_ROOT = Path.home() / ".cache" / "ores-personas"
FILL_TIMEOUT_SECONDS = 30
RESTORED_FILL_TIMEOUT_SECONDS = 6
RESTORE_WAIT_SECONDS = 8


def load_staff(project_root):
    path = Path(project_root) / ACCOUNTS_REL
    return json.loads(path.read_text(encoding="utf-8"))


def location_prefix(text):
    key = text.strip().lower()
    if key not in LOCATIONS:
        raise SystemExit(
            f"open-as: unknown location '{text}'. Known: "
            + ", ".join(sorted(LOCATIONS)))
    return LOCATIONS[key]


def in_location(person, prefix):
    unit = person.get("business_unit_code") or ""
    return unit.split(".")[0] == prefix


def by_name(staff, text):
    """People whose name or username matches, best match first.

    A substring match wins. Failing that, the closest spelling does, so a
    near miss such as "adrian vince" still finds Adrian Vance.
    """
    wanted = " ".join(text.lower().replace(".", " ").split())
    exact = [p for p in staff
             if wanted in " ".join(p["full_name"].lower().split())
             or wanted in p["username"].replace(".", " ")]
    if exact:
        return sorted(exact, key=lambda p: p["full_name"])
    names = {p["full_name"].lower(): p for p in staff}
    close = difflib.get_close_matches(wanted, list(names), n=3, cutoff=0.6)
    return [names[n] for n in close]


def by_title(staff, text, location):
    wanted = text.strip().lower()
    titles = [t for t in TITLES if wanted in t.lower()]
    if not titles:
        raise SystemExit(f"open-as: unknown job title '{text}'. Known: "
                         + ", ".join(TITLES))
    prefix = location_prefix(location)
    found = [p for p in staff if p["job_title"] in titles
             and (in_location(p, prefix) or not p.get("business_unit_code"))]
    return sorted(found, key=lambda p: (p["job_title"], p["full_name"]))


def by_role(staff, text, location):
    role = ROLES.get(text.strip().lower())
    if role is None:
        raise SystemExit(f"open-as: unknown role '{text}'. Known: "
                         + ", ".join(sorted(set(ROLES.values()))))
    prefix = location_prefix(location)
    found = [p for p in staff if p["role"] == role and in_location(p, prefix)]
    return sorted(found, key=lambda p: (p["job_title"], p["full_name"]))


def principal(person):
    return f"{person['username']}@{TENANT_HOSTNAME}"


def web_url(project_root, env_file=None):
    dotenv = env_file if env_file is not None else Path(project_root) / ".env"
    port = "8080"
    if Path(dotenv).is_file():
        for line in Path(dotenv).read_text(encoding="utf-8").splitlines():
            if line.startswith("ORES_WEB_PORT="):
                port = line.partition("=")[2].strip().strip("'\"")
    return f"http://localhost:{port}/"


def profile_path(project_root, person, workspace=None):
    """The persona's data directory, one per workspace so each keeps its pages."""
    base = PERSONAS_ROOT / env_name(project_root)
    if workspace is not None:
        base = base / f"workspace-{workspace}"
    return base / person["username"]


def env_name(project_root):
    return Path(project_root).name


# --- DevTools socket -------------------------------------------------------

def encode_frame(payload: bytes, mask_key: bytes) -> bytes:
    """A masked text frame, as a client must send it."""
    head = bytearray([0x81])
    size = len(payload)
    if size < 126:
        head.append(0x80 | size)
    elif size < 65536:
        head.append(0x80 | 126)
        head += struct.pack(">H", size)
    else:
        head.append(0x80 | 127)
        head += struct.pack(">Q", size)
    masked = bytes(b ^ mask_key[i % 4] for i, b in enumerate(payload))
    return bytes(head) + mask_key + masked


def read_exact(sock, count):
    data = b""
    while len(data) < count:
        chunk = sock.recv(count - len(data))
        if not chunk:
            raise ConnectionError("DevTools socket closed")
        data += chunk
    return data


def read_frame(sock):
    """The next text frame's payload, skipping control frames."""
    while True:
        first, second = read_exact(sock, 2)
        size = second & 0x7F
        if size == 126:
            size = struct.unpack(">H", read_exact(sock, 2))[0]
        elif size == 127:
            size = struct.unpack(">Q", read_exact(sock, 8))[0]
        payload = read_exact(sock, size)
        if first & 0x0F == 0x1:
            return payload


class DevTools:
    def __init__(self, ws_url):
        host_port, _, path = ws_url[len("ws://"):].partition("/")
        host, _, port = host_port.partition(":")
        self.sock = socket.create_connection((host, int(port)), timeout=10)
        key = base64.b64encode(os.urandom(16)).decode()
        self.sock.sendall((
            f"GET /{path} HTTP/1.1\r\nHost: {host_port}\r\n"
            "Upgrade: websocket\r\nConnection: Upgrade\r\n"
            f"Sec-WebSocket-Key: {key}\r\nSec-WebSocket-Version: 13\r\n\r\n"
        ).encode())
        reply = b""
        while b"\r\n\r\n" not in reply:
            reply += self.sock.recv(1024)
        if b" 101 " not in reply.split(b"\r\n", 1)[0]:
            raise ConnectionError("DevTools refused the socket")
        self.next_id = 0

    def evaluate(self, expression):
        self.next_id += 1
        message = json.dumps({"id": self.next_id, "method": "Runtime.evaluate",
                              "params": {"expression": expression,
                                         "returnByValue": True}})
        self.sock.sendall(encode_frame(message.encode(), os.urandom(4)))
        while True:
            answer = json.loads(read_frame(self.sock))
            if answer.get("id") == self.next_id:
                return answer.get("result", {}).get("result", {}).get("value")

    def close(self):
        self.sock.close()


FILL_SCRIPT = """
(() => {
  const user = document.querySelector('input[autocomplete="username"]');
  const pass = document.querySelector('input[autocomplete="current-password"]');
  if (!user || !pass) return false;
  const set = (el, value) => {
    const setter = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value').set;
    setter.call(el, value);
    el.dispatchEvent(new Event('input', { bubbles: true }));
  };
  set(user, %s);
  set(pass, %s);
  pass.focus();
  return true;
})()
"""


def browser_gone(browser, profile_dir):
    """Whether Chrome failed to start.

    The process started here can exit cleanly after handing over to another
    Chrome process on the same data directory, which is not a failure.
    """
    if browser is None or browser.poll() is None:
        return False
    return browser.returncode != 0 or not profile_in_use(profile_dir)


def page_socket(profile_dir, browser=None):
    """The DevTools socket URL of the first page, once Chrome has written its port."""
    port_file = Path(profile_dir) / "DevToolsActivePort"
    deadline = time.time() + FILL_TIMEOUT_SECONDS
    while time.time() < deadline:
        if browser_gone(browser, profile_dir):
            return None
        if port_file.is_file():
            port = port_file.read_text().splitlines()[0]
            try:
                with urllib.request.urlopen(
                        f"http://127.0.0.1:{port}/json/list", timeout=2) as r:
                    pages = [t for t in json.load(r) if t.get("type") == "page"]
                if pages:
                    return pages[0]["webSocketDebuggerUrl"]
            except OSError:
                pass
        time.sleep(0.5)
    return None


def fill_sign_in(profile_dir, person, browser=None, timeout=FILL_TIMEOUT_SECONDS):
    """Fill the form, trying again when the page Chrome showed first goes away.

    A restored session can open and close a page while Chrome starts, which
    closes the socket under the filler. Each attempt asks for the pages again.
    """
    script = FILL_SCRIPT % (json.dumps(principal(person)), json.dumps(PASSWORD))
    deadline = time.time() + timeout
    while time.time() < deadline:
        ws_url = page_socket(profile_dir, browser)
        if ws_url is None:
            return False
        try:
            tools = DevTools(ws_url)
            try:
                if tools.evaluate(script):
                    return True
            finally:
                tools.close()
        except OSError:
            pass
        time.sleep(0.5)
    return False


# --- launch ----------------------------------------------------------------

def chrome_binary():
    for name in ("google-chrome", "google-chrome-stable", "chromium",
                 "chromium-browser"):
        found = shutil.which(name)
        if found:
            return found
    raise SystemExit("open-as: no Chrome or Chromium on PATH")


def mentions_profile(cmdline, profile_dir):
    """Whether a process command line names this data directory.

    Chrome rewrites its own command line with spaces in place of the NUL
    separators, so the argument is looked for as a word in either form.
    """
    wanted = re.escape(f"--user-data-dir={profile_dir}".encode())
    return re.search(wanted + rb"(\s|\0|$)", cmdline) is not None


def profile_in_use(profile_dir):
    """Whether a running Chrome was started on this data directory."""
    for cmdline in Path("/proc").glob("[0-9]*/cmdline"):
        try:
            if mentions_profile(cmdline.read_bytes(), profile_dir):
                return True
        except OSError:
            continue
    return False


def merge_json(path, patch):
    """Merge patch into the JSON file at path, creating it when absent."""
    data = json.loads(path.read_text()) if path.is_file() else {}

    def merge(into, new):
        for key, value in new.items():
            if isinstance(value, dict):
                merge(into.setdefault(key, {}), value)
            else:
                into[key] = value

    merge(data, patch)
    path.write_text(json.dumps(data))


def name_profile(profile_dir, full_name):
    """Show the person's name where Chrome shows the profile's name."""
    merge_json(profile_dir / "Local State", {"profile": {"info_cache": {
        "Default": {"name": full_name, "is_using_default_name": False}}}})
    merge_json(profile_dir / "Default" / "Preferences",
               {"profile": {"name": full_name}})


def prepare_profile(profile_dir, fresh):
    if fresh and profile_dir.exists():
        shutil.rmtree(profile_dir)
    if profile_dir.exists() and not profile_in_use(profile_dir):
        # A killed Chrome leaves these behind and the next one refuses to start.
        for name in ("SingletonLock", "SingletonSocket", "SingletonCookie"):
            (profile_dir / name).unlink(missing_ok=True)
    (profile_dir / "Default").mkdir(parents=True, exist_ok=True)
    stale = profile_dir / "DevToolsActivePort"
    if stale.exists():
        stale.unlink()
    # "Continue where you left off": the pages left open, and the sign-in,
    # come back the next time this person is opened.
    merge_json(profile_dir / "Default" / "Preferences", {
        "credentials_enable_service": False,
        "profile": {"password_manager_enabled": False},
        "session": {"restore_on_startup": 1},
    })


def graphical_environment(environ=None, runtime_dir=None):
    """The environment Chrome needs to reach the desktop.

    A shell started outside the desktop, such as a coding session, has no
    display variables. The desktop's sockets are still in the user's runtime
    directory, so they are found there. Variables the shell already has win.
    """
    environ = dict(os.environ if environ is None else environ)
    if environ.get("DISPLAY") or environ.get("WAYLAND_DISPLAY"):
        return environ
    runtime = Path(runtime_dir or f"/run/user/{os.getuid()}")
    sockets = sorted(p.name for p in runtime.glob("wayland-*")
                     if not p.name.endswith(".lock"))
    if sockets:
        environ["WAYLAND_DISPLAY"] = sockets[0]
        environ.setdefault("XDG_RUNTIME_DIR", str(runtime))
    x_sockets = sorted(Path("/tmp/.X11-unix").glob("X[0-9]*"))
    if x_sockets:
        environ["DISPLAY"] = ":" + x_sockets[0].name[1:]
        auth = sorted(runtime.glob(".mutter-Xwaylandauth.*"))
        if auth:
            environ.setdefault("XAUTHORITY", str(auth[0]))
    return environ


def window_class(person):
    return f"ores-persona-{person['username']}"


def move_to_workspace(person, workspace, environ, timeout=FILL_TIMEOUT_SECONDS):
    """Move the persona's window to the 1-based workspace.

    Only X11 windows can be moved from outside on GNOME, so the window is
    found by the class Chrome was started with. Returns whether it moved.
    """
    wanted = window_class(person).lower()
    deadline = time.time() + timeout
    while time.time() < deadline:
        listing = subprocess.run(["wmctrl", "-lx"], env=environ, text=True,
                                 capture_output=True).stdout
        for line in listing.splitlines():
            # The class field holds spaces, so the whole line is searched.
            if wanted in line.lower():
                moved = subprocess.run(
                    ["wmctrl", "-i", "-r", line.split()[0], "-t", str(workspace - 1)],
                    env=environ)
                return moved.returncode == 0
        time.sleep(0.5)
    return False


def has_page(profile_dir):
    """Whether Chrome on this data directory has a page open."""
    port_file = Path(profile_dir) / "DevToolsActivePort"
    if not port_file.is_file():
        return False
    port = port_file.read_text().splitlines()[0]
    try:
        with urllib.request.urlopen(f"http://127.0.0.1:{port}/json/list",
                                    timeout=2) as r:
            return any(t.get("type") == "page" for t in json.load(r))
    except OSError:
        return False


def wait_for_profile_free(profile_dir, seconds=15):
    """Wait for every Chrome process on the data directory to end."""
    deadline = time.time() + seconds
    while time.time() < deadline and profile_in_use(profile_dir):
        time.sleep(0.3)


def wait_for_pages_or_retry(browser, profile_dir, start, attempts=3):
    """Wait for a restored session to show a page, else open the sign-in page.

    A session that holds no pages makes Chrome open no window and quit. The
    next start waits until that Chrome has fully gone, because a start made
    while it is still shutting down hands over to it and exits at once.
    Returns the browser and whether the saved session was used.
    """
    deadline = time.time() + RESTORE_WAIT_SECONDS
    while time.time() < deadline and browser.poll() is None \
            and not has_page(profile_dir):
        time.sleep(0.3)
    if browser.poll() is None:
        return browser, True
    for _ in range(attempts):
        wait_for_profile_free(profile_dir)
        browser = start(True)
        wait_end = time.time() + RESTORE_WAIT_SECONDS
        while time.time() < wait_end and browser.poll() is None \
                and not has_page(profile_dir):
            time.sleep(0.3)
        if browser.poll() is None:
            return browser, False
    return browser, False


def launch(person, url, profile_dir, fill, headless=False, workspace=None,
           restored=False):
    """Start Chrome and fill the form. Returns (browser, filled, restored).

    A profile with a saved session is started with no address, so its pages
    come back. When that session holds no pages Chrome opens no window and
    quits, so it is started again on the sign-in address.
    """
    flags = ["--headless=new"] if headless else []
    if workspace is not None:
        flags += ["--ozone-platform=x11", f"--class={window_class(person)}"]
    else:
        flags += ["--ozone-platform-hint=auto"]
    environ = graphical_environment()

    def start(with_address):
        stale = Path(profile_dir) / "DevToolsActivePort"
        stale.unlink(missing_ok=True)
        log = open(Path(profile_dir) / "chrome.log", "wb")
        return subprocess.Popen(
            [chrome_binary(), f"--user-data-dir={profile_dir}",
             "--remote-debugging-port=0", "--no-first-run",
             "--no-default-browser-check", "--password-store=basic",
             *flags, *(["--new-window", url] if with_address else [])],
            stdout=log, stderr=subprocess.STDOUT, env=environ,
            start_new_session=True)

    browser = start(not restored)
    if restored:
        browser, restored = wait_for_pages_or_retry(browser, profile_dir, start)
    if workspace is not None and not headless:
        if not move_to_workspace(person, workspace, environ):
            print(f"open-as: could not move the window to workspace {workspace}.",
                  file=sys.stderr)
    if not fill:
        return browser, True, restored
    # A restored session is usually past the form, so the wait is short.
    timeout = RESTORED_FILL_TIMEOUT_SECONDS if restored else FILL_TIMEOUT_SECONDS
    return browser, fill_sign_in(profile_dir, person, browser, timeout), restored


def describe(person):
    unit = person.get("business_unit_code") or "group"
    return (f"{person['full_name']} ({person['job_title']}, "
            f"{person['role']}, {unit}) as {principal(person)}")


def run(argv, project_root=None, env_file=None) -> int:
    parser = argparse.ArgumentParser(
        prog="compass open-as",
        description="Open the web in an isolated Chrome signed in as an Acme "
                    "user. Press Sign in; the form is already filled.")
    parser.add_argument("by", choices=["name", "title", "role"],
                        help="how to choose the person")
    parser.add_argument("text", help="the name, job title or role to match")
    parser.add_argument("--location", default=DEFAULT_LOCATION,
                        help="office for title and role: london (default), "
                             "new york, hong kong (aliases: uk, ny, us, hk)")
    parser.add_argument("--pick", type=int, default=1,
                        help="which match to open when several match (default 1)")
    parser.add_argument("--list", action="store_true", dest="list_only",
                        help="list the matches and open nothing")
    parser.add_argument("--fresh", action="store_true",
                        help="discard the persona's browser data first")
    parser.add_argument("--workspace", type=int, metavar="N",
                        help="open the window on workspace N (1 is the first). "
                             "Runs Chrome under X11, because GNOME on Wayland "
                             "cannot move a native window from outside")
    parser.add_argument("--headless", action="store_true",
                        help="run Chrome without a window, for checks")
    parser.add_argument("--no-fill", action="store_true",
                        help="open the browser but leave the form empty")
    args = parser.parse_args(argv)

    staff = load_staff(project_root)
    if args.by == "name":
        found = by_name(staff, args.text)
    elif args.by == "title":
        found = by_title(staff, args.text, args.location)
    else:
        found = by_role(staff, args.text, args.location)

    if not found:
        print("open-as: nobody matches.", file=sys.stderr)
        return 1
    for index, person in enumerate(found, 1):
        marker = "*" if index == args.pick else " "
        print(f"{marker} {index}. {describe(person)}")
    if args.list_only:
        return 0
    if not 1 <= args.pick <= len(found):
        print(f"open-as: --pick must be between 1 and {len(found)}.",
              file=sys.stderr)
        return 1

    person = found[args.pick - 1]
    profile_dir = profile_path(project_root, person, args.workspace)
    if profile_in_use(profile_dir):
        print(f"{person['full_name']} is already open. Close that window to "
              "open a new one.")
        return 0
    prepare_profile(profile_dir, args.fresh)
    name_profile(profile_dir, person["full_name"])
    restored = (profile_dir / "Default" / "Sessions").is_dir()
    browser, filled, restored = launch(
        person, web_url(project_root, env_file), profile_dir,
        not args.no_fill, args.headless, args.workspace, restored)
    if browser_gone(browser, profile_dir):
        tail = (profile_dir / "chrome.log").read_text(errors="replace")[-600:]
        print(f"open-as: Chrome exited at start (code {browser.returncode}, "
              f"saved session used: {restored}, profile still in use: "
              f"{profile_in_use(profile_dir)}).\n{tail}", file=sys.stderr)
        return 1
    if args.no_fill:
        print(f"Opened. Sign in as {principal(person)}.")
    elif restored and not filled:
        print("Reopened with the pages left open last time.")
    elif filled:
        print("Opened with the form filled. Press Sign in.")
    else:
        print(f"Opened, but the form was not found (already signed in?). "
              f"Sign in as {principal(person)} with the Acme password.")
    return 0
