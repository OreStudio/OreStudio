#!/usr/bin/env python3
"""Build a report's ORE packages through the shell and check what came out.

A one-off proof rather than a framework. It exists because the durable form of
this check -- the package-generation suite -- belongs to the Ready for business
story, and until that lands there is no way to answer the question this asks:
does a package built off the compute grid hold the documents its run document
names, and does the run document validate against the ORE schema?

It drives the real client. The build itself is done by `ore build-packages`,
run through `ores.shell --load`, so what is exercised is the same path a
developer takes by hand; the script adds only the checks on the result.

Usage::

    ./scripts/validate_ore_package.py --definition <name> \\
        --user <user> --password <password>

    # A package already on disk, with no build:
    ./scripts/validate_ore_package.py --package-dir /tmp/pkgs --skip-build

    # Also hand the package to the ORE engine, when one is available:
    ./scripts/validate_ore_package.py --definition <name> --user u --password p \\
        --ore-binary ~/ORE/App/OREMain

The ORE engine run is opt-in because the engine is not part of this repo; when
no binary is named, that step is reported as skipped rather than passed.

Inputs and outputs are told apart by evidence, not by name: see
``output_parameters``. Note that ORE's own samples under ``external/ore/examples``
are not all self-contained -- some name inputs they do not ship -- so a run
against one of those reports real absences, which is not the same as this tool
being wrong about a package the product built.
"""

from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
import tempfile
import xml.etree.ElementTree as ET
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_SHELL = REPO_ROOT / "build" / "output" / "linux-clang-release-ninja" / "publish" / "bin" / "ores.shell"
ORE_XSD = REPO_ROOT / "external" / "ore" / "xsd" / "input.xsd"
ORE_EXAMPLES = REPO_ROOT / "external" / "ore" / "examples"
RUN_DOCUMENT = Path("Input") / "ore.xml"

# The ORE run document names its inputs with parameters ending in "File". The
# paths in them are relative, and the engine resolves them against the run
# directory, so a check that only tries one base reports a missing file that is
# in fact present.
FILE_PARAMETER_SUFFIX = "File"

# How many sample run documents to classify against. The corpus is large and the
# answer stops changing long before the end of it.
OUTPUT_SCAN_LIMIT = 60


class Failure(Exception):
    """A check that did not hold."""


def load_env() -> None:
    """Export the checkout's .env, so the shell reads the same settings as compass."""
    env_file = REPO_ROOT / ".env"
    if not env_file.is_file():
        return
    for line in env_file.read_text().splitlines():
        line = line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, _, value = line.partition("=")
        os.environ.setdefault(key.strip(), value.strip())


def find_shell(explicit: str | None) -> Path:
    if explicit:
        shell = Path(explicit).expanduser()
    elif os.environ.get("ORES_SHELL"):
        shell = Path(os.environ["ORES_SHELL"]).expanduser()
    else:
        shell = DEFAULT_SHELL
    if not shell.is_file():
        raise Failure(
            f"ores.shell not found at {shell}. Build it, or pass --shell <path>."
        )
    return shell


def build_packages(shell: Path, definition: str, out_dir: Path,
                   user: str, password: str, timeout: int) -> None:
    """Run ore build-packages through a generated .ores script.

    @p definition is the report definition's name, which the command resolves
    within the party the session is signed in to.
    """
    out_dir.mkdir(parents=True, exist_ok=True)
    # A report definition name is prose -- "Board Risk Dashboard" -- so it is
    # quoted for the shell. The quotes are the shell's, not the command's: the
    # name arrives as one argument whatever it holds.
    quoted = "'" + definition.replace("'", "'\\''") + "'"
    script = (
        f"login {user} {password}\n"
        f"ore build-packages {quoted} {out_dir}\n"
    )

    with tempfile.NamedTemporaryFile("w", suffix=".ores", delete=False) as handle:
        handle.write(script)
        script_path = Path(handle.name)

    try:
        print(f"Running: {shell.name} --load <generated .ores>")
        completed = subprocess.run(
            [str(shell), "--load", str(script_path)],
            capture_output=True, text=True, timeout=timeout, cwd=REPO_ROOT,
        )
    except subprocess.TimeoutExpired as exc:
        raise Failure(
            f"the shell did not finish within {timeout}s; the package build may "
            f"still be running on the grid"
        ) from exc
    finally:
        script_path.unlink(missing_ok=True)

    if completed.returncode != 0:
        raise Failure(
            "the shell exited non-zero, so the build failed.\n"
            f"--- stdout ---\n{completed.stdout}\n--- stderr ---\n{completed.stderr}"
        )
    if "did not finish within" in completed.stdout or "refused:" in completed.stdout:
        raise Failure(f"the build reported a problem:\n{completed.stdout}")


def run_document_of(package_dir: Path) -> Path:
    candidates = sorted(package_dir.rglob(str(RUN_DOCUMENT)))
    if not candidates:
        raise Failure(
            f"no {RUN_DOCUMENT} anywhere under {package_dir}; the package has no "
            f"run document, so there is nothing to validate"
        )
    if len(candidates) > 1:
        raise Failure(
            "more than one run document found, so the package is not one report's:\n  "
            + "\n  ".join(str(c) for c in candidates)
        )
    return candidates[0]


def named_files(document: Path) -> list[tuple[str, str]]:
    """Every file the run document names, as (parameter, value), in document order."""
    root = ET.parse(document).getroot()
    names: list[tuple[str, str]] = []
    for parameter in root.iter("Parameter"):
        name = parameter.get("name", "")
        value = (parameter.text or "").strip()
        if name.endswith(FILE_PARAMETER_SUFFIX) and value:
            names.append((name, value))
    # The newer form names its inputs as File elements rather than parameters.
    for element in root.iter("File"):
        value = (element.text or "").strip()
        if value:
            names.append(("File", value))
    return names


def output_parameters() -> set[str]:
    """The parameter names ORE writes rather than reads, from the sample corpus.

    The run document gives every file a ``*File`` parameter and never says which
    the package must supply and which the engine will produce. The names cannot
    be trusted to say: ``cubeFile`` and ``scenarioFile`` are outputs, and
    ``sensitivityOutputFile`` is an input in at least one sample. So the corpus
    decides -- a parameter that resolves to a file in no sample is one the
    engine writes -- which keeps the classification evidence rather than a
    guess that rots.
    """
    resolved: set[str] = set()
    seen: set[str] = set()
    if not ORE_EXAMPLES.is_dir():
        return set()

    for document in sorted(ORE_EXAMPLES.rglob("ore.xml"))[:OUTPUT_SCAN_LIMIT]:
        try:
            pairs = named_files(document)
        except ET.ParseError:
            continue
        for parameter, value in pairs:
            seen.add(parameter)
            if resolve(document.parent, value, "Input") is not None:
                resolved.add(parameter)
    return seen - resolved


def resolve(base: Path, name: str, input_path: str) -> Path | None:
    """The first base the name resolves against, or None.

    ORE's paths are relative and the convention is not stated in the document,
    so the bases the engine itself uses are tried in turn rather than guessed at
    once and reported as a missing file.
    """
    for candidate in (
        base / name,
        base / input_path / name,
        base.parent / name,
        base.parent / input_path / name,
    ):
        if candidate.is_file():
            return candidate.resolve()
    return None


def check_layout(document: Path) -> tuple[int, list[str]]:
    """Every input the package must supply resolves, or a failure names the rest.

    @return the number of inputs that resolved, and the parameter names left out
    because the engine produces them rather than reads them.
    """
    root = ET.parse(document).getroot()
    input_path = ""
    for parameter in root.iter("Parameter"):
        if parameter.get("name") == "inputPath":
            input_path = (parameter.text or "").strip()

    pairs = named_files(document)
    if not pairs:
        raise Failure(
            f"{document} names no files, which is not a run document that can be "
            f"validated"
        )

    outputs = output_parameters()
    missing: list[str] = []
    present = 0
    excluded: set[str] = set()
    for parameter, value in pairs:
        if parameter in outputs:
            excluded.add(parameter)
            continue
        if resolve(document.parent, value, input_path) is None:
            missing.append(f"{value}  ({parameter})")
        else:
            present += 1

    if missing:
        raise Failure(
            f"{len(missing)} named input(s) are not in the package, relative to "
            f"{document.parent}:\n  " + "\n  ".join(missing)
        )
    return present, sorted(excluded)


def check_schema(document: Path) -> str:
    """The run document validates against the ORE schema, or a failure says why."""
    if not ORE_XSD.is_file():
        raise Failure(f"the ORE schema is not at {ORE_XSD}")
    xmllint = shutil.which("xmllint")
    if xmllint is None:
        raise Failure(
            "xmllint is not installed, so the schema cannot be checked. "
            "Install libxml2-utils."
        )

    completed = subprocess.run(
        [xmllint, "--schema", str(ORE_XSD), "--path", str(ORE_XSD.parent),
         "--noout", str(document)],
        capture_output=True, text=True,
    )
    if completed.returncode != 0:
        raise Failure(
            f"{document.name} does not validate against {ORE_XSD.name}:\n"
            f"{completed.stderr.strip()}"
        )
    return xmllint


def run_ore_engine(binary: Path, document: Path) -> str:
    """Hand the package to the ORE engine and report what it said."""
    if not binary.is_file():
        raise Failure(f"the ORE binary is not at {binary}")
    completed = subprocess.run(
        [str(binary), str(document)], capture_output=True, text=True, cwd=document.parent,
    )
    if completed.returncode != 0:
        raise Failure(
            f"the ORE engine exited {completed.returncode}.\n"
            f"--- stdout ---\n{completed.stdout}\n--- stderr ---\n{completed.stderr}"
        )
    return binary.name


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--definition", help="report definition name to build")
    parser.add_argument("--user", help="shell login user")
    parser.add_argument("--password", help="shell login password")
    parser.add_argument("--out-dir", default=None, help="where the package is written")
    parser.add_argument("--shell", default=None, help="path to ores.shell")
    parser.add_argument("--package-dir", default=None,
                        help="validate a package already on disk instead of building one")
    parser.add_argument("--skip-build", action="store_true",
                        help="do not build; validate --out-dir as it stands")
    parser.add_argument("--ore-binary", default=None,
                        help="run the ORE engine against the package (opt-in)")
    parser.add_argument("--timeout", type=int, default=900,
                        help="seconds to wait for the build (default: 900)")
    args = parser.parse_args()

    load_env()

    try:
        if args.skip_build or args.package_dir:
            package_dir = Path(args.package_dir or args.out_dir or "").expanduser()
            if not str(package_dir):
                raise Failure("--skip-build needs --out-dir or --package-dir")
            if not package_dir.is_dir():
                raise Failure(f"{package_dir} is not a directory")
        else:
            if not (args.definition and args.user and args.password):
                raise Failure(
                    "--definition, --user and --password are required to build a "
                    "package; use --skip-build to validate one already on disk"
                )
            package_dir = Path(
                args.out_dir or tempfile.mkdtemp(prefix="ore-package-")
            ).expanduser()
            build_packages(find_shell(args.shell), args.definition, package_dir,
                           args.user, args.password, args.timeout)

        document = run_document_of(package_dir)
        print(f"Run document : {document}")

        present, excluded = check_layout(document)
        print(f"Layout       : {present} named input(s) present")
        if excluded:
            print(f"               {len(excluded)} output parameter(s) not required: "
                  + ", ".join(excluded))

        check_schema(document)
        print(f"Schema       : {document.name} validates against {ORE_XSD.name}")

        if args.ore_binary:
            name = run_ore_engine(Path(args.ore_binary).expanduser(), document)
            print(f"ORE engine   : {name} ran the package")
        else:
            print("ORE engine   : skipped (pass --ore-binary to run it)")

        print(f"\nPASS: {package_dir}")
        return 0
    except Failure as failure:
        print(f"\nFAIL: {failure}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
