#!/usr/bin/env python3
"""
Print the clean-standard baseline inventory for one component as Markdown.

Items B02, B03 and B04 of doc/knowledge/architecture/component_clean_standard.org
all ask for the same thing before any edit is made: a census of what the
component is made of. This script takes that census and prints it, so the
task that follows works from a table instead of from memory.

B02 counts every org model file under the component's modeling directory
by its frontmatter metatype, and marks the metatypes that carry no
variability. A component, module or field-group file describes a
structure rather than a wire type, so no profile can vary it and the
profiles sweep excludes it. The exclusion is the table's last column, so
the reader applies it rather than trusting the script's arithmetic.

Directory discovery has a deliberate asymmetry. A catalogue component is
read through codegen.manifest, which resolves its modeling_dir. A
component with no catalogue entry yet, ores.assets being the one in
flight, has no resolved directory and is read through --project, whose
modeling directory is <project>/modeling by definition. Files under
<project>/*/modeling/ are the sub-component overviews and protocol
references, never codegen models, so B02 does not count them. B04 still
reads them: a sub-component protocol reference names subjects without
declaring them, and reporting those separately is the point of the B04
cross-reference.

B03 splits every C++ file under the project into generated and
hand-written by the AUTO-GENERATED FILE marker, then classifies each
hand-written file against the families the generator knows. Files that
are plausibly hand-written are looked at before they are called
generatable: a test, an export header or a service application is
hand-written even in a directory whose generated neighbours exist. A
file that no family claims is reported as unclassified. That label is
deliberate and never a synonym for generatable: the survey exists to be
actionable, and a guess would turn into deleted code.

B04 lists the protocol headers, every raw subject literal in the
project's own sources with its file and line, and every subject a model
declares. The interesting row is a subject that appears in code and
carries no model source, or the reverse. Reading the two lists side by
side is how item P02 is satisfied.

Read-only. Nothing is written, no temporary file is created, and no
model, catalogue entry or generated file is touched.

Usage:
  survey_component.py --component iam
  survey_component.py --component iam-cpp
  survey_component.py --project projects/ores.assets
  survey_component.py --component iam -v
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
CODEGEN_DIR = REPO_ROOT / "projects" / "ores.codegen"
sys.path.insert(0, str(CODEGEN_DIR / "src"))

from codegen.manifest import get_component  # noqa: E402

GENERATED_MARKER = "AUTO-GENERATED FILE"

# Metatypes no profile can vary: they describe a structure (a component, a
# module index, a field-group assembly) rather than a wire type, so B02's
# profile sweep excludes them by definition.
NO_VARIABILITY = frozenset({
    "component",
    "component_catalogue",
    "field_group",
    "module",
    "oresmd_quote_type",
    "service_registry",
})

CXX_SUFFIXES = (".hpp", ".cpp")

# Path components that name a build container rather than a functional
# area, so the area label reads api/domain instead of api/include.
CONTAINERS = frozenset({"include", "src", "tests", "modeling", "generated"})

# A test, an export header and the service application are generated for a
# stub and then owned by hand, so their names decide against their
# neighbours: a hand-written test sits beside a generated eventing test.
HAND_WRITTEN_SUFFIXES = (
    "export.hpp",
    "application.hpp",
    "application.cpp",
    "application_exception.hpp",
    "host.hpp",
    "host.cpp",
    "main.cpp",
    "options.hpp",
    "options.cpp",
    "parser.hpp",
    "parser.cpp",
    "parser_exception.hpp",
)

GENERATABLE_SUFFIXES = (
    "_json_io.hpp",
    "_json_io.cpp",
    "_json.hpp",
    "_json.cpp",
    "_table_io.hpp",
    "_table_io.cpp",
    "_table.hpp",
    "_table.cpp",
    "_entity.hpp",
    "_entity.cpp",
    "_mapper.hpp",
    "_mapper.cpp",
    "_repository.hpp",
    "_repository.cpp",
    "_service.hpp",
    "_service.cpp",
    "_handler.hpp",
    "_handler.cpp",
    "_registrar.hpp",
    "_registrar.cpp",
    "_protocol.hpp",
    "_generator.hpp",
    "_generator.cpp",
)

SUBJECT_NAME_TEMPLATE = r'{token}\.v\d+\.[A-Za-z0-9_*][A-Za-z0-9_.*-]*'
SUBJECT_RE_TEMPLATE = '"' + SUBJECT_NAME_TEMPLATE + '"'
PROPERTY_SUBJECT_RE = re.compile(r"^:subject:\s*(\S+)\s*$")
TABLE_ROW_RE = re.compile(r"^\|(?![-\s|])[^|]*\|")
ORG_TYPE_RE = re.compile(r"^#\+type:\s*(\S+)\s*$", re.IGNORECASE)


def relative(path: Path) -> str:
    """A path as the reader sees it: relative to the repository root."""
    try:
        return str(path.relative_to(REPO_ROOT))
    except ValueError:
        return str(path)


def modelling_org_files(modeling_dir: Path) -> list:
    if not modeling_dir.is_dir():
        return []
    return sorted(
        path for path in (
            list(modeling_dir.glob("*.org")) + list(modeling_dir.glob("*/*.org"))
        )
        if path.is_file()
    )


def org_metatype(path: Path) -> str:
    """The file's metatype, the short name after the ores.codegen prefix."""
    with path.open(encoding="utf-8", errors="replace") as handle:
        head = handle.read(4096)
    for line in head.splitlines():
        match = ORG_TYPE_RE.match(line)
        if not match:
            continue
        value = match.group(1)
        if value == "ores.codegen":
            return "(none)"
        return value.removeprefix("ores.codegen.")
    return "(none)"


def project_cxx_files(project_dir: Path) -> list:
    return sorted(
        path for path in project_dir.rglob("*")
        if path.is_file() and path.suffix in CXX_SUFFIXES
    )


def is_generated(path: Path) -> bool:
    with path.open(encoding="utf-8", errors="replace") as handle:
        head = handle.read(4096)
    return GENERATED_MARKER in head


def area_of(path: Path, project_dir: Path) -> str:
    """The functional area: the first two non-container path components.

    api/include/ores.iam.api/domain/account.hpp -> api/domain
    core/src/repository/account_entity.cpp     -> core/repository
    core/include/ores.iam.core/ores.iam.hpp    -> core
    service/src/main.cpp                       -> service
    """
    parts = [
        part for part in path.relative_to(project_dir).parts[:-1]
        if part not in CONTAINERS and not part.startswith("ores.")
    ]
    return "/".join(parts[:2]) or "(project root)"


def is_umbrella_header(path: Path, project_dir: Path) -> bool:
    """True for the include aggregator of a component layer.

    ores.iam/modeling gives the names ores.iam.hpp (the component root
    header) and ores.iam.<layer>.hpp (a layer's umbrella), where <layer>
    is the directory the header sits in: ores.iam.service.hpp is the
    service layer's umbrella. The generator writes these as include lists
    and never revisits them, so they stay hand-written however they were
    first produced.
    """
    name = path.name
    if not name.endswith(".hpp"):
        return False
    component = project_dir.name
    if name == f"{component}.hpp":
        return True
    directory = path.parent.name
    return name == f"{component}.{directory}.hpp"


def classify(path: Path, project_dir: Path) -> str:
    """generatable, infrastructure or unclassified, from the path alone.

    The path parts rather than the printed area carry the decision: the
    area drops the tests container, and a test is hand-written whether or
    not the family beside it generates one.
    """
    name = path.name
    parts = path.relative_to(project_dir).parts[:-1]
    tokens = set(parts)

    if "tests" in tokens and name.endswith(("_tests.cpp", "main.cpp")):
        return "infrastructure"
    if name.endswith(HAND_WRITTEN_SUFFIXES):
        return "infrastructure"
    if is_umbrella_header(path, project_dir):
        return "infrastructure"

    if name.endswith(GENERATABLE_SUFFIXES):
        return "generatable"
    if tokens & {"messaging", "service"}:
        return "generatable"

    # A domain header with no family of its own is a domain type whose
    # family the json I/O file carries.
    if "domain" in tokens and name.endswith(".hpp"):
        return "generatable"

    return "unclassified"


def project_org_files(project_dir: Path) -> list:
    """Every org file under the project, two levels down.

    modelling_org_files stops at the modeling directory, which is where
    the models are. A project also documents its protocol in a
    component's own modeling directory, one level further in, and that is
    where a subject is written down before it is modelled.
    """
    return sorted(
        path for path in (
            list(project_dir.glob("*.org"))
            + list(project_dir.glob("*/*.org"))
            + list(project_dir.glob("*/*/*.org"))
        )
        if path.is_file()
    )


def subject_token(project_dir: Path) -> str:
    """The component's subject namespace, ores.iam -> iam."""
    token = project_dir.name
    if token.startswith("ores."):
        token = token[len("ores."):]
    return token


def read_lines(path: Path) -> list:
    with path.open(encoding="utf-8", errors="replace") as handle:
        return handle.read().splitlines()


def find_matches(path: Path, pattern: re.Pattern) -> list:
    """Every (line number, matched text) in the file, in line order."""
    found = []
    for number, line in enumerate(read_lines(path), start=1):
        for match in pattern.finditer(line):
            found.append((number, match.group(0)))
    return found


def model_subjects(project_dir: Path) -> tuple:
    """Subjects a model declares, and subjects a modelling doc names.

    A subject reaches a modelling file in one of three shapes: the
    ``:subject:`` property drawer an operation or message carries, a
    protocol reference table whose first column names the subject, or a
    subject held in org emphasis inside prose or a bullet. The property
    drawer is the codegen declaration, which decides whether a subject has
    a model source. The other two are how a protocol reference documents
    its subjects, and are kept apart so the reader can see a subject that
    is documented but not yet declared.

    Only this component's namespace is documented here. A protocol
    reference also names the subjects of the components it talks to, and
    another component's subject is not this survey's to report.
    """
    declared, documented = [], []
    own = re.compile(
        SUBJECT_NAME_TEMPLATE.format(token=re.escape(subject_token(project_dir))))
    property_re = re.compile(SUBJECT_RE_TEMPLATE.format(token=r"[a-z_]+"))
    prose_re = re.compile(
        r"=(?P<subject>[a-z_]+\.v\d+\.[A-Za-z0-9_*][A-Za-z0-9_.*-]*)=")
    for path in project_org_files(project_dir):
        for number, line in enumerate(read_lines(path), start=1):
            stripped = line.strip()
            prop = PROPERTY_SUBJECT_RE.match(stripped)
            if prop:
                declared.append((path, number, prop.group(1)))
                continue
            if TABLE_ROW_RE.match(stripped):
                first = stripped.strip("|").split("|")[0]
                for match in property_re.finditer(first):
                    subject = match.group(0).strip('"')
                    if own.fullmatch(subject):
                        documented.append((path, number, subject))
                continue
            for match in prose_re.finditer(stripped):
                if own.fullmatch(match.group("subject")):
                    documented.append((path, number, match.group("subject")))
    return (
        sorted(declared, key=lambda entry: (str(entry[0]), entry[1], entry[2])),
        sorted(documented, key=lambda entry: (str(entry[0]), entry[1], entry[2])),
    )


def component_subjects(project_dir: Path) -> list:
    """Raw subject literals in the project's own sources.

    The namespace is read from the project directory name (ores.iam ->
    iam.v1.), so a literal naming another component is not claimed by
    this survey. The version is any v<digit>, in case a subject family
    moves on from v1.
    """
    token = project_dir.name
    if token.startswith("ores."):
        token = token[len("ores."):]
    pattern = re.compile(SUBJECT_RE_TEMPLATE.format(token=re.escape(token)))
    found = []
    for path in project_cxx_files(project_dir):
        for number, text in find_matches(path, pattern):
            found.append((path, number, text.strip('"')))
    return sorted(found, key=lambda entry: (str(entry[0]), entry[1], entry[2]))


def protocol_headers(project_dir: Path) -> list:
    return sorted(
        path for path in project_cxx_files(project_dir)
        if path.name.endswith("_protocol.hpp")
    )


def markdown_table(headers: list, rows: list) -> list:
    lines = ["| " + " | ".join(headers) + " |"]
    lines.append("|" + "|".join("-" * (len(header) + 2) for header in headers) + "|")
    for row in rows:
        lines.append("| " + " | ".join(str(cell) for cell in row) + " |")
    return lines


def report_models(component: str, modeling_dir: Path) -> list:
    files = modelling_org_files(modeling_dir)
    lines = ["## B02 Models by metatype", ""]
    if not files:
        lines.append(
            f"No org model files under `{relative(modeling_dir)}`"
            " (the directory does not exist for this component)."
        )
        lines.append("")
        lines.append("Total models: 0. Variability-carrying models: 0.")
        return lines

    counts: dict = {}
    for path in files:
        counts.setdefault(org_metatype(path), []).append(relative(path))

    rows = []
    for metatype, paths in sorted(counts.items()):
        carries = "no" if metatype in NO_VARIABILITY else "yes"
        rows.append((metatype, len(paths), carries, ", ".join(paths)))
    lines += markdown_table(
        ["metatype", "models", "variability", "files"], rows)
    lines.append("")
    variable = sum(
        len(paths) for metatype, paths in counts.items()
        if metatype not in NO_VARIABILITY
    )
    excluded = sorted(
        metatype for metatype in counts if metatype in NO_VARIABILITY)
    lines.append(
        f"Total models: {len(files)}. Variability-carrying models: {variable}."
    )
    lines.append("")
    if excluded:
        lines.append(
            "Excluded as variability-free: "
            + ", ".join(f"`{metatype}`" for metatype in excluded) + "."
        )
    else:
        lines.append("Excluded as variability-free: none.")
    lines.append("")
    lines.append(
        f"Modeling directory: `{relative(modeling_dir)}`"
        f" ({len(files)} of {len(files)} counted from"
        " `*.org` and `*/*.org`)."
    )
    return lines


def report_cxx(project_dir: Path) -> list:
    files = project_cxx_files(project_dir)
    generated = [path for path in files if is_generated(path)]
    hand_written = [path for path in files if not is_generated(path)]

    rows = []
    counts = {"generatable": 0, "infrastructure": 0, "dead": 0, "unclassified": 0}
    for path in hand_written:
        area = area_of(path, project_dir)
        kind = classify(path, project_dir)
        counts[kind] = counts.get(kind, 0) + 1
        rows.append((relative(path), area, kind))

    lines = ["## B03 Hand-written C++ files", ""]
    lines += markdown_table(["file", "area", "classification"], rows)
    lines.append("")
    percentage = (100.0 * len(generated) / len(files)) if files else 0.0
    lines.append(
        f"{len(generated)} of {len(files)} C++ files are generated"
        f" ({percentage:.1f}%); {len(hand_written)} are hand-written."
    )
    lines.append("")
    lines.append(
        "Hand-written by classification: "
        + ", ".join(
            f"{kind} {counts[kind]}" for kind in
            ("generatable", "infrastructure", "dead", "unclassified")
        )
        + "."
    )
    dead = [row for row in rows if row[2] == "dead"]
    lines.append("")
    lines.append(
        f"Dead: {len(dead)} file(s). This survey reports no file as dead"
        " from its path alone; a dead file is found by checking references."
    )
    return lines


def report_protocol(project_dir: Path, modeling_dir: Path) -> list:
    headers = protocol_headers(project_dir)
    literals = component_subjects(project_dir)
    declared, documented = model_subjects(project_dir)

    lines = ["## B04 Protocol inventory", ""]

    lines.append("### Protocol headers")
    lines.append("")
    rows = []
    for path in headers:
        matches = find_matches(
            path, re.compile(SUBJECT_RE_TEMPLATE.format(token=r"[a-z_]+")))
        rows.append((relative(path), "generated" if is_generated(path)
                     else "hand-written", len(matches)))
    if rows:
        lines += markdown_table(["file", "marker", "subjects"], rows)
    else:
        lines.append("None.")
    lines.append("")
    lines.append(
        f"{len(headers)} protocol header(s): "
        f"{sum(1 for row in rows if row[1] == 'generated')} generated, "
        f"{sum(1 for row in rows if row[1] == 'hand-written')} hand-written."
    )

    lines.append("")
    lines.append("### Subject literals in sources")
    lines.append("")
    if literals:
        lines += markdown_table(
            ["file:line", "subject", "in protocol header"],
            [
                (f"{relative(path)}:{number}", subject,
                 "yes" if path.name.endswith("_protocol.hpp") else "no")
                for path, number, subject in literals
            ],
        )
    else:
        lines.append("None.")
    lines.append("")
    raw = [
        (path, number, subject) for path, number, subject in literals
        if not path.name.endswith("_protocol.hpp")
    ]
    lines.append(
        f"{len(literals)} subject literal(s) in sources;"
        f" {len(raw)} of them sit outside a protocol header."
    )

    lines.append("")
    lines.append("### Subjects declared in models")
    lines.append("")
    if declared:
        lines += markdown_table(
            ["model", "line", "subject"],
            [(relative(path), number, subject)
             for path, number, subject in declared],
        )
    else:
        lines.append(
            f"None. No model under `{relative(modeling_dir)}` declares a"
            " subject in a property drawer."
        )

    lines.append("")
    lines.append("### Subjects named in modelling docs")
    lines.append("")
    if documented:
        lines += markdown_table(
            ["model", "line", "subject"],
            [(relative(path), number, subject)
             for path, number, subject in documented],
        )
    else:
        lines.append("None.")
    lines.append("")
    lines.append(
        "These are named by a table row or by org emphasis, so they document"
        " a subject without declaring it. A subject that reaches P02 needs"
        " a model source, not a reference."
    )

    by_subject: dict = {}
    for path, number, subject in literals:
        entry = by_subject.setdefault(
            subject, {"code": set(), "model": set(), "doc": set()})
        entry["code"].add(f"{relative(path)}:{number}")
    for path, number, subject in declared:
        entry = by_subject.setdefault(
            subject, {"code": set(), "model": set(), "doc": set()})
        entry["model"].add(f"{relative(path)}:{number}")
    for path, number, subject in documented:
        entry = by_subject.setdefault(
            subject, {"code": set(), "model": set(), "doc": set()})
        entry["doc"].add(f"{relative(path)}:{number}")

    lines.append("")
    lines.append("### Subject cross-reference")
    lines.append("")
    if by_subject:
        lines += markdown_table(
            ["subject", "code source", "model source", "docs"],
            [
                (subject,
                 ", ".join(sorted(entry["code"])) or "none",
                 ", ".join(sorted(entry["model"])) or "none",
                 ", ".join(sorted(entry["doc"])) or "none")
                for subject, entry in sorted(by_subject.items())
            ],
        )
    else:
        lines.append("None.")
    lines.append("")
    code_only = sorted(
        subject for subject, entry in by_subject.items()
        if entry["code"] and not entry["model"])
    model_only = sorted(
        subject for subject, entry in by_subject.items()
        if entry["model"] and not entry["code"])
    lines.append(f"Distinct subjects: {len(by_subject)}.")
    lines.append("")
    lines.append("In code with no model source: "
                 + (", ".join(f"`{subject}`" for subject in code_only)
                    or "none") + ".")
    lines.append("")
    lines.append("Declared in a model with no code literal: "
                 + (", ".join(f"`{subject}`" for subject in model_only)
                    or "none") + ".")
    return lines


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    selectors = ap.add_mutually_exclusive_group(required=True)
    selectors.add_argument(
        "--component",
        metavar="NAME",
        help="one component by catalogue slug (e.g. iam); its modeling "
        "directory is resolved through codegen.manifest",
    )
    selectors.add_argument(
        "--project",
        metavar="PATH",
        help="a project directory by path (e.g. projects/ores.assets), for "
        "a component with no catalogue entry yet; its modeling directory "
        "is <project>/modeling",
    )
    ap.add_argument("-v", "--verbose", action="store_true")
    args = ap.parse_args()

    if args.component:
        try:
            comp = get_component(args.component)
        except ValueError as error:
            print(error, file=sys.stderr)
            return 2
        if not comp.modeling_dir:
            print(f"Component {args.component!r} declares no modeling_dir.",
                  file=sys.stderr)
            return 2
        modeling_dir = REPO_ROOT / comp.modeling_dir
        project_dir = modeling_dir.parent
    else:
        project_dir = Path(args.project)
        if not project_dir.is_absolute():
            project_dir = REPO_ROOT / project_dir
        project_dir = project_dir.resolve()
        if not project_dir.is_dir():
            print(f"No such project directory: {project_dir}", file=sys.stderr)
            return 2
        modeling_dir = project_dir / "modeling"

    if args.verbose:
        print(f"# Survey of {relative(project_dir)}", file=sys.stderr)
        print(f"# Modeling directory: {relative(modeling_dir)}", file=sys.stderr)

    print(f"# Clean-standard survey: {relative(project_dir)}")
    print()
    omitted = [
        path for path in sorted(project_dir.glob("*/modeling"))
        if path.is_dir() and path != modeling_dir
    ]
    if omitted:
        print(f"Not surveyed as models: "
              + ", ".join(f"`{relative(path)}`" for path in omitted) + ".")
        print()

    for section in (
        report_models(args.component or project_dir.name, modeling_dir),
        report_cxx(project_dir),
        report_protocol(project_dir, modeling_dir),
    ):
        print("\n".join(section))
        print()

    return 0


if __name__ == "__main__":
    sys.exit(main())
