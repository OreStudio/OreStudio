#!/usr/bin/env python3
"""The three leaf maps, and what each one names.

Reads the seed scripts rather than a database, so the numbers do not
depend on which checkout's schema the database happens to hold:

    refdata_curve_configuration_columns_populate.sql
    refdata_convention_kind_terms_populate.sql
    reporting_ore_parameter_vocabularies_populate.sql
    reporting_ore_parameters_artefact_populate.sql   (the contract)
    reporting_ore_parameter_value_sets_populate.sql  (the setting links)

A leaf is (schema, element). A curve column or a convention term carries
one in its xpath, so the element is the last path segment with any
[named] predicate removed. The contract carries the same pair, which is
what makes the guards' join on (schema_name, xpath) work.

Prints: how many leaves each map names, the overlap between them, and
whether a leaf the guards check resolves to a value set today.

Usage: leaf_maps_overlap.py [--root PATH]
"""
import argparse
import pathlib
import re
import sys

def repository_root():
    """The checkout, found by walking up to the one that holds the tree."""
    for parent in pathlib.Path(__file__).resolve().parents:
        if (parent / "projects" / "ores.sql").is_dir():
            return parent
    raise SystemExit("not inside the repository")


ROOT = repository_root()

SEED = "projects/ores.sql/populate"

COLUMNS = f"{SEED}/refdata/refdata_curve_configuration_columns_populate.sql"
TERMS = f"{SEED}/refdata/refdata_convention_kind_terms_populate.sql"
VOCABULARIES = f"{SEED}/reporting/reporting_ore_parameter_vocabularies_populate.sql"
CONTRACT = f"{SEED}/reporting/reporting_ore_parameters_artefact_populate.sql"
LINKS = f"{SEED}/reporting/reporting_ore_parameter_value_sets_populate.sql"

ELEMENT = re.compile(r"/([^/\[]+)(?:\[[^\]]*\])?$")


def split_values(body):
    """The tuples of an insert's values clause, as lists of strings.

    Stops at the statement's semicolon, so the `on conflict` clause that
    follows is not read as more tuples.
    """
    rows, field, depth, quoted, i = [], [], 0, False, 0
    while i < len(body):
        ch = body[i]
        if quoted:
            if ch == "'" and i + 1 < len(body) and body[i + 1] == "'":
                field.append("'")
                i += 2
                continue
            if ch == "'":
                quoted = False
            else:
                field.append(ch)
        elif ch == "'":
            quoted = True
        elif ch == "(":
            depth += 1
            if depth == 1:
                field = []
        elif ch == ")":
            depth -= 1
            if depth == 0:
                rows.append(field)
                field = []
                rest = body[i + 1:].lstrip().lower()
                if rest.startswith((";", "on ", "returning", "do ")):
                    break
        elif ch == ";" and depth == 0:
            break
        elif ch == "," and depth == 1:
            field.append("\x00")
        else:
            if depth == 1:
                field.append(ch)
        i += 1
    out = []
    for row in rows:
        text = "".join(row)
        out.append([part.strip() for part in text.split("\x00")])
    return out


def table(root, path, table_name):
    """The rows of `insert into <table_name>` as dicts keyed by column."""
    text = (root / path).read_text(encoding="utf-8")
    header = re.search(
        r"insert into\s+" + table_name + r"\s*\(([^)]*)\)\s*values", text, re.I)
    if not header:
        raise SystemExit(f"no insert for {table_name} in {path}")
    names = [n.strip().strip('"') for n in header.group(1).split(",")]
    return [dict(zip(names, values))
            for values in split_values(text[header.end():])]


def element_of(xpath):
    match = ELEMENT.search(xpath.strip())
    return match.group(1) if match else None


def leaves(rows, schema_key="schema_name", xpath_key="xpath"):
    out = set()
    for row in rows:
        element = element_of(row.get(xpath_key, ""))
        if element:
            out.add((row[schema_key], element))
    return out


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", default=ROOT)
    args = parser.parse_args()
    root = pathlib.Path(args.root)

    curve = table(root, COLUMNS, "ores_refdata_curve_configuration_columns_tbl")
    terms = table(root, TERMS, "ores_refdata_convention_kind_terms_tbl")
    vocab = table(root, VOCABULARIES,
                  "ores_reporting_ore_parameter_vocabularies_tbl")
    contract = table(root, CONTRACT, "ores_dq_ore_parameters_artefact_tbl")
    links = table(root, LINKS,
                  "ores_reporting_ore_parameter_setting_value_sets_tbl")

    curve_leaves = leaves(curve)
    term_leaves = leaves(terms)
    vocab_leaves = {(r["schema_name"], r["element_name"]) for r in vocab}

    set_of_parameter = {(r["scope"], r["name"]): r["value_set_code"]
                        for r in links}
    contract_by_leaf = {}
    for row in contract:
        element = element_of(row.get("xpath", ""))
        if element:
            contract_by_leaf.setdefault(
                (row["schema_name"], element), row)

    leaf_via_contract = {}
    for leaf, row in contract_by_leaf.items():
        code = set_of_parameter.get((row["scope"], row["name"]))
        if code:
            leaf_via_contract[leaf] = code

    guard_leaves = curve_leaves | term_leaves
    resolved = guard_leaves & set(leaf_via_contract)
    unresolved = guard_leaves - set(leaf_via_contract)

    print("rows in each source")
    print(f"  curve configuration columns      {len(curve):5d}"
          f"  ({len(curve_leaves)} distinct leaves)")
    print(f"  convention kind terms            {len(terms):5d}"
          f"  ({len(term_leaves)} distinct leaves)")
    print(f"  vocabularies                     {len(vocab):5d}"
          f"  ({len(vocab_leaves)} distinct leaves)")
    print(f"  the parameter contract           {len(contract):5d}"
          f"  ({len(contract_by_leaf)} distinct leaves)")
    print(f"  setting links to a value set     {len(links):5d}")
    print()

    print("overlap of the three leaf maps")
    print(f"  curve columns and vocabularies   "
          f"{len(curve_leaves & vocab_leaves):5d}")
    print(f"  convention terms and vocabularies"
          f"{len(term_leaves & vocab_leaves):5d}")
    print(f"  curve columns and convention terms"
          f"{len(curve_leaves & term_leaves):5d}")
    print(f"  in all three                     "
          f"{len(curve_leaves & term_leaves & vocab_leaves):5d}")
    print()

    print("the vocabularies rows, leaf by leaf")
    for row in vocab:
        leaf = (row["schema_name"], row["element_name"])
        guarded = "yes" if leaf in guard_leaves else "no "
        code = leaf_via_contract.get(leaf)
        print(f"  {leaf[0]}|{leaf[1]:26s} set={row['value_set_code']:34s}"
              f" guarded={guarded}"
              f" via the contract={code or 'none'}")
    print()

    print("the leaves the two guards check")
    print(f"  distinct leaves                  {len(guard_leaves):5d}")
    print(f"  resolve to a set via the contract{len(resolved):5d}")
    print(f"  resolve to no set                {len(unresolved):5d}")
    in_contract = {leaf for leaf in unresolved if leaf in contract_by_leaf}
    missing_contract = unresolved - in_contract
    reachable_by_vocab = unresolved & vocab_leaves
    print(f"    in the contract, with no set   {len(in_contract):5d}"
          f"  (a parameter, and no rule)")
    print(f"    not in the contract at all     {len(missing_contract):5d}"
          f"  (the map names a leaf the document does not)")
    print(f"    named in vocabularies          {len(reachable_by_vocab):5d}")
    print()

    if unresolved:
        print("unresolved leaves, by element")
        by_element = {}
        for schema, element in sorted(unresolved):
            by_element.setdefault(element, []).append(schema)
        for element, schemas in sorted(by_element.items()):
            print(f"  {element:32s} {', '.join(sorted(set(schemas)))}")
        print()

    mismatched = [(leaf, leaf_via_contract[leaf], vocab_code)
                  for leaf, vocab_code in
                  ((l, r["value_set_code"]) for l in vocab_leaves
                   for r in vocab
                   if (r["schema_name"], r["element_name"]) == l)
                  if leaf in leaf_via_contract
                  and leaf_via_contract[leaf] != vocab_code]
    if mismatched:
        print("leaves where the two paths name different sets")
        for leaf, via_contract, via_vocab in mismatched:
            print(f"  {leaf[0]}|{leaf[1]:28s} contract={via_contract}"
                  f"  vocabularies={via_vocab}")
        print()

    return 0


if __name__ == "__main__":
    sys.exit(main())
