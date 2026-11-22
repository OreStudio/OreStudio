#!/usr/bin/env python3
"""Inventory the Qt DQ surface from the last commit that held it.

The Qt tree is no longer in the checkout: it went in af67db3062. The evidence
the DQ journeys are argued from therefore lives in git, and this script reads it
from there rather than from a vault that no longer exists.

The script counts and names, it does not judge. For every client model it reads
the column enum from the header and the column labels from the model's
headerData; for every .ui file it counts the labelled fields. A reviewer reruns
it against the commit and compares the tables on the page.

    ./qt_dq_inventory.py --ref 'af67db3062^'
"""

import argparse
import re
import subprocess
import sys


def git_show(ref, path):
    result = subprocess.run(
        ["git", "show", f"{ref}:{path}"],
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        raise SystemExit(f"cannot read {path} at {ref}: {result.stderr.strip()}")
    return result.stdout


def git_ls(ref, path):
    result = subprocess.run(
        ["git", "ls-tree", "-r", "--name-only", ref, "--", path],
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        raise SystemExit(f"cannot list {path} at {ref}: {result.stderr.strip()}")
    return [line for line in result.stdout.splitlines() if line]


def column_enum(header_source):
    match = re.search(r"enum\s+Column\s*\{(.*?)\}", header_source, re.DOTALL)
    if not match:
        return []
    names = []
    for entry in match.group(1).split(","):
        name = entry.split("=")[0].strip()
        if name and name != "ColumnCount":
            names.append(name)
    return names


def column_labels(source):
    labels = {}
    for name, label in re.findall(
        r"case\s+(\w+)\s*:\s*return\s+tr\(\"([^\"]*)\"\)", source
    ):
        labels[name] = label
    return labels


def ui_field_labels(ui_source):
    """The text of every QLabel whose name says it labels a field."""
    labels = []
    for block in re.findall(r"<widget class=\"QLabel\".*?</widget>", ui_source, re.DOTALL):
        name_match = re.search(r'name="([^"]+)"', block)
        text_match = re.search(r'<string>([^<]*)</string>', block)
        if not name_match or not text_match:
            continue
        name, text = name_match.group(1), text_match.group(1)
        lowered = name.lower()
        is_field = lowered.startswith("label") or lowered.endswith("label")
        if text and is_field and not lowered.endswith("titlelabel") and not text.endswith(" Details"):
            labels.append(text)
    return labels


def table(headers, rows):
    out = ["| " + " | ".join(headers) + " |"]
    out.append("|" + "|".join("-" * (len(h) + 2) for h in headers) + "|")
    for row in rows:
        out.append("| " + " | ".join(row) + " |")
    return "\n".join(out)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--ref", default="af67db3062^")
    parser.add_argument("--path", default="projects/ores.qt/dq")
    args = parser.parse_args()

    files = git_ls(args.ref, args.path)
    headers = [f for f in files if f.endswith(".hpp") and "/include/" in f]
    sources = [f for f in files if f.endswith(".cpp") and "/src/" in f]
    uis = [f for f in files if f.endswith(".ui")]

    basenames = {f.rsplit("/", 1)[-1] for f in headers}
    window_entities = sorted(
        b[len("") : -len("MdiWindow.hpp")]
        for b in basenames
        if b.endswith("MdiWindow.hpp")
    )
    detail_entities = sorted(
        b[: -len("DetailDialog.ui")]
        for b in (f.rsplit("/", 1)[-1] for f in uis)
        if b.endswith("DetailDialog.ui")
    )
    history_entities = sorted(
        b[: -len("HistoryDialog.ui")]
        for b in (f.rsplit("/", 1)[-1] for f in uis)
        if b.endswith("HistoryDialog.ui")
    )

    models = {}
    for header in headers:
        base = header.rsplit("/", 1)[-1]
        match = re.fullmatch(r"Client(\w+)Model\.hpp", base)
        if not match:
            continue
        entity = match.group(1)
        source_path = f"{args.path}/src/Client{entity}Model.cpp"
        if source_path not in sources:
            continue
        labels = column_labels(git_show(args.ref, source_path))
        columns = [
            labels.get(name, name) for name in column_enum(git_show(args.ref, header))
        ]
        models[entity] = columns

    print(f"# Qt DQ inventory at {args.ref}:{args.path}\n")
    print(
        f"- translation units: "
        f"{len([f for f in files if f.endswith('.cpp')])} cpp, "
        f"{len([f for f in files if f.endswith('.hpp')])} hpp"
    )
    print(f"- ui files: {len(uis)}")
    print(f"- list windows: {len(window_entities)}")
    print(f"- detail dialogs: {len(detail_entities)}")
    print(f"- history dialogs: {len(history_entities)}\n")

    rows = []
    for entity in window_entities:
        columns = models.get(entity)
        detail = "yes" if entity in detail_entities else "—"
        history = "yes" if entity in history_entities else "—"
        shown = ", ".join(columns) if columns else "(no columns read)"
        rows.append([entity, shown, detail, history])
    print(table(["Entity", "List columns", "Detail", "History"], rows))

    print("\n## Fields a detail dialog asks for\n")
    for entity in detail_entities:
        ui = next(f for f in uis if f.endswith(f"/{entity}DetailDialog.ui"))
        labels = ui_field_labels(git_show(args.ref, ui))
        print(f"- *{entity}* ({len(labels)}): {', '.join(labels)}")

    only_ui = sorted({e for e in detail_entities + history_entities} - set(window_entities))
    if only_ui:
        print(f"\nDialogs with no list window of their own: {', '.join(only_ui)}")

    print("\n## Publishing surface classes\n")
    for base in sorted(b for b in basenames if "Dialog" in b or "Window" in b or "Wizard" in b):
        if base.endswith(("MdiWindow.hpp", "DetailDialog.hpp", "HistoryDialog.hpp")):
            continue
        print(f"- ={base[:-len('.hpp')]}=")

    return 0


if __name__ == "__main__":
    sys.exit(main())
