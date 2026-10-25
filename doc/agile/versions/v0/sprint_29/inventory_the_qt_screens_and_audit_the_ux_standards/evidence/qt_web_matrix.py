#!/usr/bin/env python3
"""Compare the Qt reference data screens with the ores.web screens.

Reads the Qt client sources (the detail dialogs, the list models and the MDI
windows) and the web registries (the record resources, the classification
lists, the convention kinds and the field tables), and writes two files
beside this script: qt_web_matrix.json with the raw per-entity data, and
qt_web_matrix.org with the tables the knowledge page carries.

Usage: qt_web_matrix.py --qt <path to ores.qt> [--repo <path to the checkout>]
"""
import argparse
import json
import pathlib
import re
import xml.etree.ElementTree as ET

HERE = pathlib.Path(__file__).resolve().parent

# The provenance fields every versioned record carries. The web reads them and
# never asks for them, so they are not a field gap.
AUDIT_FIELDS = {
    "version", "modified_by", "performed_by", "recorded_at",
    "change_reason_code", "change_commentary", "tenant_id", "id",
}

# Qt widget name suffixes, longest first, so "PlainTextEdit" wins over "Edit".
WIDGET_SUFFIXES = [
    "PlainTextEdit", "TextEdit", "DateTimeEdit", "DateEdit", "TimeEdit",
    "SpinBox", "ComboBox", "CheckBox", "Combo", "Edit", "Spin", "Check",
]
INPUT_CLASSES = {
    "QLineEdit", "QSpinBox", "QDoubleSpinBox", "QPlainTextEdit", "QTextEdit",
    "QComboBox", "QCheckBox", "QDateEdit", "QDateTimeEdit", "QTimeEdit",
}

# Each web field table and the record kind it drives.
FIELD_TABLES = {
    "CURRENCY_FIELDS": "currencies",
    "GROUP_FIELDS": "currency-groups",
    "CALENDAR_FIELDS": "calendars",
    "RULE_FIELDS": "calendar-rules",
    "EXCEPTION_FIELDS": "calendar-exceptions",
    "EVENT_FIELDS": "calendar-events",
    "PAIR_FIELDS": "currency-pairs",
    "CONVENTION_FIELDS": "currency-pair-conventions",
    "PARTY_FIELDS": "parties",
    "IDENTIFIER_FIELDS": "party-identifiers",
    "CONTACT_FIELDS": "party-contacts",
    "UNIT_FIELDS": "business-units",
}


def snake(name: str) -> str:
    name = re.sub(r"(.)([A-Z][a-z]+)", r"\1_\2", name)
    name = re.sub(r"([a-z0-9])([A-Z])", r"\1_\2", name)
    return name.lower()


def field_of_widget(name: str) -> str:
    for suffix in WIDGET_SUFFIXES:
        if name.endswith(suffix) and len(name) > len(suffix):
            return snake(name[: -len(suffix)])
    return snake(name)


def qt_dialog(ui_path: pathlib.Path) -> dict:
    """The tabs, groups and input fields of one detail dialog."""
    root = ET.parse(ui_path).getroot()
    tabs: dict[str, list[str]] = {}
    fields: list[dict] = []
    buttons: list[str] = []

    def walk(node, tab, group):
        for child in node:
            if child.tag == "widget":
                cls = child.get("class", "")
                name = child.get("name", "")
                next_tab, next_group = tab, group
                if node.tag == "widget" and node.get("class") == "QTabWidget":
                    next_tab = name
                    tabs.setdefault(name, [])
                elif cls == "QGroupBox":
                    next_group = name
                if cls in INPUT_CLASSES:
                    field = field_of_widget(name)
                    fields.append({
                        "field": field, "widget": cls, "name": name,
                        "tab": tab, "group": group,
                    })
                    if tab:
                        tabs.setdefault(tab, []).append(field)
                elif cls == "QPushButton":
                    buttons.append(field_of_widget(name).removesuffix("_button"))
                walk(child, next_tab, next_group)
            else:
                walk(child, tab, group)

    walk(root, None, None)
    return {"tabs": tabs, "fields": fields, "buttons": buttons}


def qt_dialog_fields(dialog_cpp: pathlib.Path) -> list[str]:
    """The wire fields the dialog reads into widgets and writes back from them.

    The widget names are not the wire names, and some dialogs build their
    combos in code, so the dialog source is the authority: the member of the
    entity variable (the one ending in an underscore that is not ui_) that
    the dialog touches most.
    """
    text = dialog_cpp.read_text()
    uses: dict[str, list[str]] = {}
    for var, field in re.findall(r"\b([a-zA-Z][a-zA-Z0-9_]*_)\.([a-z][a-z0-9_]*)\b", text):
        if var in ("ui_", "clientManager_", "badgeCache_", "model_"):
            continue
        uses.setdefault(var, []).append(field)
    if not uses:
        return []
    entity_var = max(uses, key=lambda v: len(uses[v]))
    fields: list[str] = []
    for field in uses[entity_var]:
        if field not in fields and field not in AUDIT_FIELDS:
            fields.append(field)
    return fields


def qt_columns(model_cpp: pathlib.Path) -> list[str]:
    """The struct fields the list model shows, in column order."""
    text = model_cpp.read_text()
    match = re.search(r"QVariant \w+::data\(.*?\n\}", text, re.S)
    if not match:
        return []
    body = match.group(0)
    display = re.search(r"role == Qt::DisplayRole\) \{(.*?)\n    \}", body, re.S)
    if not display:
        return []
    columns = []
    for case in re.finditer(r"case (\w+):\s*\n?\s*return (.*?);", display.group(1), re.S):
        fields = re.findall(r"\b\w+\.(\w+)\b", case.group(2))
        columns.append(fields[0] if fields else snake(case.group(1)))
    return columns


def qt_actions(window_cpp: pathlib.Path) -> list[str]:
    text = window_cpp.read_text()

    def words(name: str) -> str:
        text = snake(name).replace("_", " ")
        return re.sub(r"\b(xml|csv)\b", lambda m: m.group(1).upper(), text)

    actions = [words(m.removesuffix("Action")) for m in
               re.findall(r"(\w+Action)_? = toolbar_->addAction\(", text)]
    actions += [f"show {words(m)}" for m in re.findall(r"emit show(\w+)Requested\(\)", text)]
    standard = {"reload", "add", "edit", "delete", "history"}
    actions = [a if a in standard or a.split()[0] in ("show", "import", "export")
               else f"show {a}" for a in actions]
    seen: list[str] = []
    for action in actions:
        if action not in seen:
            seen.append(action)
    return seen


def qt_entities(qt_root: pathlib.Path) -> dict[str, dict]:
    src = qt_root / "refdata" / "src"
    ui = qt_root / "refdata" / "ui"
    names: set[str] = set()
    for path in src.glob("*.cpp"):
        stem = path.stem
        for suffix in ("DetailDialog", "MdiWindow", "Controller"):
            if stem.endswith(suffix):
                names.add(stem[: -len(suffix)])
        if stem.startswith("Client") and stem.endswith("Model"):
            names.add(stem[len("Client"):-len("Model")])
    entities = {}
    for name in sorted(names):
        parts = []
        entity: dict = {"snake": snake(name), "parts": parts}
        dialog = ui / f"{name}DetailDialog.ui"
        if dialog.exists():
            parts.append("detail dialog")
            entity["dialog"] = qt_dialog(dialog)
            entity["fields"] = qt_dialog_fields(src / f"{name}DetailDialog.cpp")
        model = src / f"Client{name}Model.cpp"
        if model.exists():
            parts.append("list model")
            entity["columns"] = qt_columns(model)
        if "fields" not in entity:
            entity["fields"] = [c for c in entity.get("columns", []) if c not in AUDIT_FIELDS]
        window = src / f"{name}MdiWindow.cpp"
        if window.exists():
            parts.append("mdi window")
            entity["actions"] = qt_actions(window)
            text = window.read_text()
            if "HistoryDialog" in text or "history" in text.lower():
                parts.append("history")
        for extra in ("ChildEntityTables", "HierarchyTab"):
            if (src / f"{name}{extra}.cpp").exists():
                parts.append(snake(extra).replace("_", " "))
        entities[name] = entity
    return entities


def web_records(packages: pathlib.Path) -> dict[str, dict]:
    text = (packages / "wire-protocol" / "src" / "records.ts").read_text()
    records = {}
    for block in re.finditer(r"\{\s*key: '([a-z-]+)',(.*?)\n    \},", text, re.S):
        key, body = block.group(1), block.group(2)
        entity = re.search(r"entityType: '([\w.]+)'", body)
        key_fields = re.search(r"keyFields: \[(.*?)\]", body)
        records[key] = {
            "entityType": entity.group(1) if entity else "",
            "keyFields": re.findall(r"'(\w+)'", key_fields.group(1)) if key_fields else [],
            "versioned": "versioned: true" in body,
            "writable": "writable: true" in body,
            "listBy": (re.search(r"listBy: '(\w+)'", body) or [None, None])[1],
            "fields": [],
        }
    for tsx in (packages / "web" / "src" / "refdata").glob("*.tsx"):
        if tsx.name.endswith(".test.tsx"):
            continue
        source = tsx.read_text()
        for table in re.finditer(r"const (\w+_FIELDS): readonly FieldSpec\[\] = \[(.*?)\n\];", source, re.S):
            key = FIELD_TABLES.get(table.group(1))
            if key is None or key not in records:
                print(f"unmapped field table {table.group(1)} in {tsx.name}")
                continue
            records[key]["fields"] = re.findall(r"field: '(\w+)'", table.group(2))
    return records


def web_classifications(packages: pathlib.Path) -> dict[str, dict]:
    text = (packages / "wire-protocol" / "src" / "classifications.ts").read_text()
    lists = {}
    for block in re.finditer(r"\{\s*key: '([a-z-]+)',(.*?)\n    \},", text, re.S):
        key, body = block.group(1), block.group(2)
        entity = re.search(r"entityType: '([\w.]+)'", body)
        shape = re.search(r"shape: '(\w+)'", body)
        lists[key] = {
            "entityType": entity.group(1) if entity else "",
            "shape": shape.group(1) if shape else "",
            "editable": "editable: true" in body,
        }
    return lists


def web_conventions(packages: pathlib.Path) -> dict[str, dict]:
    kinds_text = (packages / "web" / "src" / "refdata" / "conventionKinds.ts").read_text()
    kinds = {}
    for key, asset in re.findall(r"^\s+'?([a-z-]+)'?: '([a-z-]+)',$", kinds_text, re.M):
        kinds[key] = {"assetClass": asset, "terms": []}
    state = (packages / "web" / "src" / "journeys" / "conventionsState.ts").read_text()
    specs = re.search(r"export const TERM_SPECS[^=]*= \{(.*?)\n\};", state, re.S)
    if specs:
        for family in re.finditer(r"\n    '?([a-z-]+)'?: \[(.*?)\n    \],", specs.group(1), re.S):
            kinds.setdefault(family.group(1), {"assetClass": "", "terms": []})
            kinds[family.group(1)]["terms"] = re.findall(r"column: '(\w+)'", family.group(2))
    return kinds


# Qt entities the web reaches outside the reference data registries. Each
# names the surface that covers it, found by reading the web sources.
ELSEWHERE = {
    "Book": "Book structure journey (trading area), =api/books.ts=",
    "Portfolio": "Book structure journey (trading area), =api/books.ts=",
    "Tenor": "Tenor resolution page, =api/tenors.ts= writes tenors and memberships",
    "Counterparty": "Counterparties tab of the party page, =api/counterparties.ts= composite write",
    "CounterpartyIdentifier": "part of the counterparty composite, =api/counterparties.ts=",
    "CounterpartyContactInformation": "part of the counterparty composite, =api/counterparties.ts=",
    "IrCurveBootstrapConfig": "synthetic IR feeds (market data area), =marketdata/synthetic/irFeeds.tsx=",
}

# Why a field the Qt dialog has is not a gap in the web, by field name.
# A field not named here, and not in the web, is a gap.
NOT_A_GAP = {
    "calendar_code": "parent key; the web lists a part under its parent (RSS Addresses, Words: Part)",
    "pair_code": "parent key; the web lists a part under its parent (RSS Addresses, Words: Part)",
    "party_id": "party scope; the web lists the record under the party (RSS What a record kind declares: parent field)",
    "id": "a UUID id is never shown (RSS The table)",
}


def coverage(entity: dict, records, lists, kinds) -> dict:
    """Where the web shows this Qt entity, found by its entity type."""
    wanted = f"ores.refdata.{entity['snake']}"
    for key, record in records.items():
        if record["entityType"] == wanted:
            kind = "record" if record["fields"] else "registry only"
            return {"kind": kind, "key": key, "fields": record["fields"],
                    "writable": record["writable"]}
    for key, item in lists.items():
        if item["entityType"] == wanted:
            return {"kind": "classification", "key": key,
                    "fields": ["code", "name", "description", "display_order"],
                    "writable": item["editable"]}
    base = entity["snake"].removesuffix("_convention").replace("_", "-")
    if entity["snake"].endswith("_convention") and base in kinds:
        terms = kinds[base]["terms"]
        return {"kind": "convention", "key": base, "fields": ["id", *terms],
                "writable": bool(terms)}
    name = next((n for n, e in QT_BY_NAME.items() if e is entity), "")
    if name in ELSEWHERE:
        return {"kind": "elsewhere", "key": ELSEWHERE[name], "fields": [], "writable": False}
    return {"kind": "none", "key": "", "fields": [], "writable": False}


QT_BY_NAME: dict[str, dict] = {}


def compare(entity: dict, cover: dict) -> dict:
    qt_fields = entity["fields"]
    web_fields = [f for f in cover["fields"] if f not in AUDIT_FIELDS]
    qt_only = [f for f in qt_fields if f not in web_fields]
    return {
        "qt_only": [f for f in qt_only if f not in NOT_A_GAP],
        "explained": {f: NOT_A_GAP[f] for f in qt_only if f in NOT_A_GAP},
        "web_only": [f for f in web_fields if f not in qt_fields],
        "qt_count": len(qt_fields),
        "web_count": len(web_fields),
    }


def org_actions_table(rows: list[dict]) -> str:
    """The entities whose Qt window had more than the standard toolbar, or child parts."""
    standard = ["reload", "add", "edit", "delete", "history"]
    out = ["| Qt entity | Beyond the standard toolbar | Child parts |",
           "|-----------+-----------------------------+-------------|"]
    for row in rows:
        extra = [a for a in row["qt"].get("actions", []) if a not in standard]
        missing = [a for a in standard if a not in row["qt"].get("actions", [])]
        children = [p for p in row["parts"] if p in ("child entity tables", "hierarchy tab")]
        if not extra and not missing and not children:
            continue
        note = ", ".join(extra)
        if missing:
            note += (", " if note else "") + "without " + ", ".join(missing)
        out.append(f"| {row['name']} | {note} | {', '.join(children)} |")
    return "\n".join(out)


def splice(page: pathlib.Path, marker: str, body: str) -> None:
    """Replace the text between the begin and end comments of one marker."""
    text = page.read_text()
    begin, end = f"# begin {marker}", f"# end {marker}"
    head, rest = text.split(begin, 1)
    _, tail = rest.split(end, 1)
    page.write_text(f"{head}{begin}\n{body}\n{end}{tail}")


def org_table(rows: list[dict]) -> str:
    out = ["| Qt entity | Qt parts | Web coverage | Qt fields | Web fields | Fields only in Qt | Fields only in the web |",
           "|-----------+----------+--------------+-----------+------------+-------------------+------------------------|"]
    for row in rows:
        cover = row["web"]
        compared = cover["kind"] in ("record", "classification", "convention")
        if cover["kind"] == "none":
            where = "none"
        elif cover["kind"] == "elsewhere":
            where = cover["key"]
        else:
            where = f"{cover['kind']} ={cover['key']}="
            if not cover["writable"]:
                where += " (read only)"
        diff = row["diff"]
        out.append("| {} | {} | {} | {} | {} | {} | {} |".format(
            row["name"], ", ".join(row["parts"]), where, diff["qt_count"],
            diff["web_count"] if compared else "",
            ", ".join(f"={f}=" for f in diff["qt_only"]) if compared else "",
            ", ".join(f"={f}=" for f in diff["web_only"])))
    return "\n".join(out)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--qt", required=True, type=pathlib.Path)
    parser.add_argument("--repo", type=pathlib.Path, default=HERE.parents[6])
    args = parser.parse_args()
    packages = args.repo / "projects" / "ores.web" / "packages"

    entities = qt_entities(args.qt)
    QT_BY_NAME.update(entities)
    records = web_records(packages)
    lists = web_classifications(packages)
    kinds = web_conventions(packages)

    rows = []
    for name, entity in entities.items():
        cover = coverage(entity, records, lists, kinds)
        rows.append({"name": name, "parts": entity["parts"], "qt": entity,
                     "web": cover, "diff": compare(entity, cover)})

    (HERE / "qt_web_matrix.json").write_text(json.dumps({
        "qt": entities, "web": {"records": records, "classifications": lists,
                                "conventions": kinds}, "rows": rows},
        indent=2, sort_keys=True) + "\n")
    (HERE / "qt_web_matrix.org").write_text(org_table(rows) + "\n")
    page = args.repo / "doc" / "knowledge" / "ui" / "qt_web_reference_data_matrix.org"
    if page.exists() and "# begin matrix" in page.read_text():
        splice(page, "matrix", org_table(rows))
        splice(page, "actions", org_actions_table(rows))

    kinds_of = {}
    for row in rows:
        kinds_of.setdefault(row["web"]["kind"], []).append(row["name"])
    for kind, names in sorted(kinds_of.items()):
        print(f"{kind:15s} {len(names):3d}  {' '.join(names)}")


if __name__ == "__main__":
    main()
