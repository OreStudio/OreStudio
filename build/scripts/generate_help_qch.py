#!/usr/bin/env python3
"""Compile the help HTML export into a Qt help collection (.qch).

Reads the self-contained manual produced by ores-build-help.el
(build/output/help/user_manual.html + images/), derives a Qt Help
Project (.qhp) — contents tree and keyword index from the HTML's
table of contents — bundles every file, and runs qhelpgenerator to
produce user_manual.qch.

The .qch is the input the in-app QHelpEngine viewer loads. Run after
deploy_help; wired as the deploy_help_qch CMake target.

Usage: python3 build/scripts/generate_help_qch.py
(paths are derived from this script's location).
"""
import html
import re
import shutil
import subprocess
import sys
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
REPO_ROOT = SCRIPT_DIR.parent.parent
HELP_DIR = REPO_ROOT / "build" / "output" / "help"
HTML_FILE = HELP_DIR / "user_manual.html"
QHP_FILE = HELP_DIR / "user_manual.qhp"
QCH_FILE = HELP_DIR / "user_manual.qch"

NAMESPACE = "org.orestudio.usermanual"
VIRTUAL_FOLDER = "manual"

# A table-of-contents entry: section number → depth, anchor, title.
TOC_RE = re.compile(
    r'<a href="#(?P<anchor>[^"]+)">(?P<num>[0-9.]+)\.\s+(?P<title>[^<]*)</a>')


def find_qhelpgenerator():
    """Locate qhelpgenerator across PATH and the usual Qt install dirs."""
    for name in ("qhelpgenerator", "qhelpgenerator-qt6"):
        found = shutil.which(name)
        if found:
            return found
    for base in ("/usr/lib/qt6/libexec", "/usr/lib/qt6/bin",
                 "/usr/lib/x86_64-linux-gnu/qt6/libexec"):
        cand = Path(base) / "qhelpgenerator"
        if cand.exists():
            return str(cand)
    return None


def parse_toc(text):
    """Return [(depth, anchor, title)] from the manual's contents list.

    Depth comes from the section number (1 → 0, 1.1 → 1, …) so the
    nesting is reconstructed without parsing the HTML list structure.
    """
    toc = []
    # Restrict to the TOC block so body links are not mistaken for entries.
    m = re.search(r'id="text-table-of-contents".*?</ul>\s*</div>',
                  text, re.S)
    block = m.group(0) if m else text
    for entry in TOC_RE.finditer(block):
        depth = entry.group("num").count(".")
        title = html.unescape(entry.group("title").strip())
        toc.append((depth, entry.group("anchor"), title))
    return toc


def build_toc_xml(toc):
    """Nest the flat TOC into Qt Help <section> elements."""
    lines = []
    stack = []  # open depths
    indent = "        "
    for depth, anchor, title in toc:
        while stack and stack[-1] >= depth:
            lines.append(indent + "  " * len(stack) + "</section>")
            stack.pop()
        ref = f"user_manual.html#{anchor}"
        attrs = f'title="{html.escape(title)}" ref="{html.escape(ref)}"'
        # Open (children may follow); close lazily when depth recedes.
        lines.append(indent + "  " * (len(stack) + 1) + f"<section {attrs}>")
        stack.append(depth)
    while stack:
        lines.append(indent + "  " * len(stack) + "</section>")
        stack.pop()
    return "\n".join(lines)


def build_keywords_xml(toc):
    """Index every section title as a keyword for the help index."""
    seen = set()
    out = []
    for _depth, anchor, title in toc:
        if title in seen:
            continue
        seen.add(title)
        ref = f"user_manual.html#{anchor}"
        out.append(
            f'        <keyword name="{html.escape(title)}" '
            f'ref="{html.escape(ref)}"/>')
    return "\n".join(out)


def build_files_xml():
    """List the HTML and every bundled image."""
    files = ["user_manual.html"]
    images = HELP_DIR / "images"
    if images.is_dir():
        files += sorted(f"images/{p.name}" for p in images.iterdir())
    return "\n".join(f"        <file>{html.escape(f)}</file>" for f in files)


def main():
    if not HTML_FILE.exists():
        sys.exit(f"error: {HTML_FILE} not found — run deploy_help first.")
    gen = find_qhelpgenerator()
    if not gen:
        sys.exit("error: qhelpgenerator not found (install qt6-tools-dev-tools "
                 "or add it to PATH).")

    toc = parse_toc(HTML_FILE.read_text(encoding="utf-8"))
    if not toc:
        sys.exit("error: no table-of-contents entries parsed from the HTML.")

    qhp = f"""<?xml version="1.0" encoding="UTF-8"?>
<QtHelpProject version="1.0">
    <namespace>{NAMESPACE}</namespace>
    <virtualFolder>{VIRTUAL_FOLDER}</virtualFolder>
    <filterSection>
        <toc>
{build_toc_xml(toc)}
        </toc>
        <keywords>
{build_keywords_xml(toc)}
        </keywords>
        <files>
{build_files_xml()}
        </files>
    </filterSection>
</QtHelpProject>
"""
    QHP_FILE.write_text(qhp, encoding="utf-8")
    print(f"wrote {QHP_FILE.relative_to(REPO_ROOT)} "
          f"({len(toc)} sections)")

    result = subprocess.run(
        [gen, str(QHP_FILE), "-o", str(QCH_FILE)],
        cwd=HELP_DIR, capture_output=True, text=True)
    if result.returncode != 0:
        sys.stdout.write(result.stdout)
        sys.stderr.write(result.stderr)
        sys.exit(f"error: qhelpgenerator failed (exit {result.returncode}).")
    print(result.stdout.strip())
    print(f"wrote {QCH_FILE.relative_to(REPO_ROOT)} "
          f"({QCH_FILE.stat().st_size // 1024} KB)")


if __name__ == "__main__":
    main()
