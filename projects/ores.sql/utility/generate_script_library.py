#!/usr/bin/env python3
"""Seed ORE's script library as reference data, mined from the library itself.

Every script of ORE's library becomes a row, with each variant's code and NPV
variable, the results it reports, the calibrations it asks for and the
schedules it may coarsen. Its inputs are mined from the code rather than typed
by hand: an input is a variable the code reads and does not declare, its type
is what the code does with it, it is an array when the code indexes it or asks
its size, and its value rule is the script's REQUIRE statement about it. The
library's comment block adds a description where it names the input.

Run with no argument to write the seed, with --check to fail when the committed
seed is not what the library gives, so a later ORE library that adds, removes or
changes an input fails the check, and with --compare to measure the mined types
against the scripted trades ORE's examples state.
"""
import argparse
import glob
import re
import sys
import xml.etree.ElementTree as ET
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
LIBRARY = ROOT / "external/ore/examples/Products/Input/scriptlibrary.xml"
SEED = ROOT / "projects/ores.sql/populate/trading/trading_script_library_populate.sql"

KEYWORDS = {"IF", "THEN", "ELSE", "END", "FOR", "IN", "DO", "NUMBER", "REQUIRE", "AND", "OR",
            "NOT", "TODAY", "GEQ", "GT", "LEQ", "LT", "EQ"}
FUNCTIONS = {"PAY", "LOGPAY", "NPV", "NPVMEM", "HISTFIXING", "DISCOUNT", "FWDCOMP", "FWDAVG",
             "ABOVEPROB", "BELOWPROB", "SIZE", "DATEINDEX", "SORT", "PERMUTE", "abs", "exp",
             "ln", "log", "sqrt", "pow", "max", "min", "normalCdf", "normalPdf", "black", "dcf",
             "days", "RESETNPVMEM"}
IDENT = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")


def tokens(code):
    return re.findall(r"[A-Za-z_][A-Za-z0-9_]*|\d+\.?\d*(?:[eE][-+]?\d+)?|==|!=|<=|>=|[^\s]",
                      code)


def declared(code):
    names = set()
    for block in re.findall(r"\bNUMBER\b([^;]*);", code):
        for part in block.split(","):
            match = IDENT.match(part.strip())
            if match:
                names.add(match.group(0))
    return names


def assigned(code):
    """The variables a statement of the code assigns to."""
    pattern = re.compile(r"(?:^|;|\bTHEN\b|\bELSE\b|\bDO\b)\s*([A-Za-z_][A-Za-z0-9_]*)"
                         r"\s*(?:\[[^\]]*\])?\s*=(?!=)")
    return set(pattern.findall(code))


def call_arguments(toks, start):
    """The argument token lists of the call whose '(' is at toks[start]."""
    args, current, depth = [], [], 0
    for i in range(start, len(toks)):
        tok = toks[i]
        if tok == "(":
            depth += 1
            if depth == 1:
                continue
        elif tok == ")":
            depth -= 1
            if depth == 0:
                args.append(current)
                return args
        elif tok == "," and depth == 1:
            args.append(current)
            current = []
            continue
        current.append(tok)
    return args


def bare(arg):
    """The variable an argument is, when it is one variable, possibly indexed."""
    if arg and IDENT.fullmatch(arg[0]) and (len(arg) == 1 or arg[1] == "["):
        return arg[0]
    return None


def infer(code):
    """The inputs a script reads: name to (type, is_array), in first-use order."""
    toks = tokens(code)
    local = declared(code)
    order, kind, arrays = [], {}, set()

    def note(name, typ=None):
        if name in local or name in KEYWORDS or name in FUNCTIONS:
            return
        if name not in kind:
            order.append(name)
            kind[name] = None
        if typ and kind[name] in (None, "Number"):
            kind[name] = typ

    for i, tok in enumerate(toks):
        if not IDENT.fullmatch(tok) or tok in KEYWORDS:
            continue
        nxt = toks[i + 1] if i + 1 < len(toks) else ""
        if tok in FUNCTIONS:
            if nxt != "(":
                continue
            args = call_arguments(toks, i + 1)
            names = [bare(a) for a in args]
            if tok in ("PAY", "LOGPAY"):
                for pos, typ in ((1, "Event"), (2, "Event"), (3, "Currency")):
                    if pos < len(names) and names[pos]:
                        note(names[pos], typ)
            elif tok in ("ABOVEPROB", "BELOWPROB"):
                for pos, typ in ((0, "Index"), (1, "Event"), (2, "Event")):
                    if pos < len(names) and names[pos]:
                        note(names[pos], typ)
            elif tok in ("dcf", "days"):
                for pos, typ in ((0, "Daycounter"), (1, "Event"), (2, "Event")):
                    if pos < len(names) and names[pos]:
                        note(names[pos], typ)
            elif tok == "NPV" and len(names) > 1 and names[1]:
                note(names[1], "Event")
            elif tok == "DATEINDEX":
                if names and names[0]:
                    note(names[0], "Event")
                if len(names) > 1 and names[1]:
                    note(names[1], "Event")
                    arrays.add(names[1])
            elif tok == "SIZE" and names and names[0]:
                arrays.add(names[0])
            elif tok in ("FWDCOMP", "FWDAVG", "HISTFIXING") and names and names[0]:
                note(names[0], "Index")
            continue
        if tok in local:
            continue
        note(tok)
        if nxt == "[":
            arrays.add(tok)
            close = toks.index("]", i + 1)
            if close + 1 < len(toks) and toks[close + 1] == "(":
                note(tok, "Index")
                for name in (bare(a) for a in call_arguments(toks, close + 1)):
                    if name:
                        note(name, "Event")
        elif nxt == "(":
            note(tok, "Index")
            for name in (bare(a) for a in call_arguments(toks, i + 1)):
                if name:
                    note(name, "Event")
    return [(name, kind[name] or "Number", name in arrays) for name in order]


def value_rules(code):
    """The REQUIRE statements of a script, by the input each names first."""
    rules = {}
    for statement in re.findall(r"\bREQUIRE\b([^;]*);", code):
        text = " ".join(statement.split())
        first = IDENT.search(text)
        if first:
            rules.setdefault(first.group(0), text)
    return rules


def comment_lines(root_text):
    """Each script's comment block, as name to {variable: description}."""
    blocks = {}
    pattern = re.compile(r"<!--(.*?)-->\s*<Script>\s*<Name>([^<]+)</Name>", re.S)
    for comment, name in pattern.findall(root_text):
        lines = comment.strip().splitlines()
        title = lines[0].strip() if lines else ""
        described = {}
        for line in lines[1:]:
            match = re.match(r"\s*([A-Za-z_][A-Za-z0-9_]*)\s{2,}(.+)", line)
            if match:
                described.setdefault(match.group(1), match.group(2).strip())
        blocks[name.strip()] = (title, described)
    return blocks


def load(path):
    text = Path(path).read_text(encoding="utf-8")
    root = ET.fromstring(text)
    comments = comment_lines(text)
    scripts = []
    for script in root.findall("Script"):
        name = script.findtext("Name").strip()
        title, described = comments.get(name, ("", {}))
        variants = []
        for variant in script.findall("Script"):
            variants.append({
                "purpose": variant.get("purpose") or "default",
                "code": variant.findtext("Code") or "",
                "npv": (variant.findtext("NPV") or "").strip(),
                "results": [(r.text.strip(), r.get("rename"))
                            for r in variant.findall("Results/Result")],
                "calibrations": [(c.findtext("Index").strip(),
                                  [s.text.strip() for s in c.findall("Strikes/Strike")])
                                 for c in variant.findall("CalibrationSpec/Calibration")],
                "coarsenings": [e.text.strip()
                                for e in variant.findall("ScheduleCoarsening/EligibleSchedule")],
            })
        # A trade states the inputs of every variant it may run, so the inputs
        # are the union over the variants, the default's first. A variable the
        # code assigns, such as the one its NPV is read from, is the script's to
        # write and not the trade's to state; one it only reads, even when it is
        # reported as a result, is an input.
        outputs = set().union(*(assigned(v["code"]) for v in variants))
        rules, seen, inputs = {}, set(), []
        for variant in sorted(variants, key=lambda v: v["purpose"] != "default"):
            for key, rule in value_rules(variant["code"]).items():
                rules.setdefault(key, rule)
            for n, t, a in infer(variant["code"]):
                if n not in seen and n not in outputs:
                    seen.add(n)
                    inputs.append((n, t, a))
        inputs = [(n, t, a, rules.get(n), described.get(n)) for n, t, a in inputs]
        scripts.append({"name": name, "tag": (script.findtext("ProductTag") or "").strip() or None,
                        "title": title or None, "variants": variants, "inputs": inputs})
    return scripts


def literal(value):
    if value is None:
        return "null"
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, int):
        return str(value)
    if "\n" in value or "'" in value:
        tag = "$code$"
        while tag in value:
            tag = tag.replace("$", "$x", 1)
        return f"{tag}{value}{tag}"
    return "'" + value + "'"


AUDIT = "current_user, 'system.initial_load', 'Seed the script library'"


def insert(table, columns, rows, conflict):
    if not rows:
        return ""
    body = ",\n".join(
        "    (" + ", ".join(literal(v) for v in row[:1]) + ", ores_utility_system_tenant_id_fn(), 0, "
        + ", ".join(literal(v) for v in row[1:]) + ", " + AUDIT + ")" for row in rows)
    head = columns[0] + ", tenant_id, version, " + ", ".join(columns[1:])
    return (f"insert into {table} (\n    {head},\n    modified_by, change_reason_code, "
            f"change_commentary\n) values\n{body}\non conflict ({conflict})\n"
            f"where valid_to = ores_utility_infinity_timestamp_fn()\ndo nothing;\n\n")


def seed(scripts):
    out = ["/* Generated by projects/ores.sql/utility/generate_script_library.py from ORE's\n"
           " * script library. Do not edit: run the script, which --check verifies. */\n\n"
           "\\echo '--- Script library ---'\n\n"]
    s_rows, v_rows, r_rows, c_rows, k_rows, o_rows, i_rows = [], [], [], [], [], [], []
    for script in scripts:
        n = script["name"]
        s_rows.append((n, script["tag"], script["title"]))
        for v in script["variants"]:
            p = v["purpose"]
            v_rows.append((n, p, v["code"], v["npv"]))
            for seq, (var, rename) in enumerate(v["results"], 1):
                r_rows.append((n, p, seq, var, rename))
            for cal, (index, strikes) in enumerate(v["calibrations"], 1):
                c_rows.append((n, p, cal, index))
                for seq, strike in enumerate(strikes, 1):
                    k_rows.append((n, p, cal, seq, strike))
            for seq, schedule in enumerate(v["coarsenings"], 1):
                o_rows.append((n, p, seq, schedule))
        for seq, (name, typ, array, rule, desc) in enumerate(script["inputs"], 1):
            i_rows.append((n, seq, name, typ, array, rule, desc))
    out.append(insert("ores_trading_scripts_tbl", ["name", "product_tag", "description"],
                      s_rows, "tenant_id, name"))
    out.append(insert("ores_trading_script_variants_tbl",
                      ["script_name", "purpose", "code", "npv_variable"], v_rows,
                      "tenant_id, script_name, purpose"))
    out.append(insert("ores_trading_script_results_tbl",
                      ["script_name", "purpose", "sequence_number", "variable", "rename"], r_rows,
                      "tenant_id, script_name, purpose, sequence_number"))
    out.append(insert("ores_trading_script_calibrations_tbl",
                      ["script_name", "purpose", "sequence_number", "index_variable"], c_rows,
                      "tenant_id, script_name, purpose, sequence_number"))
    out.append(insert("ores_trading_script_calibration_strikes_tbl",
                      ["script_name", "purpose", "calibration_number", "sequence_number",
                       "strike_variable"], k_rows,
                      "tenant_id, script_name, purpose, calibration_number, sequence_number"))
    out.append(insert("ores_trading_script_coarsenings_tbl",
                      ["script_name", "purpose", "sequence_number", "eligible_schedule"], o_rows,
                      "tenant_id, script_name, purpose, sequence_number"))
    out.append(insert("ores_trading_script_inputs_tbl",
                      ["script_name", "sequence_number", "input_name", "data_type", "is_array",
                       "value_rule", "description"], i_rows,
                      "tenant_id, script_name, sequence_number"))
    return "".join(out)


def compare():
    """Measure the mined inputs against every scripted trade of the examples."""
    agree, disagree, missing, extras = 0, [], [], 0
    for path in glob.glob(str(ROOT / "external/ore/examples/**/*.xml"), recursive=True):
        try:
            root = ET.parse(path).getroot()
        except ET.ParseError:
            continue
        library = Path(path).parent / "scriptlibrary.xml"
        if root.tag != "Portfolio" or not library.exists():
            continue
        loaded = load(library)
        mined = {s["name"]: {i[0]: i for i in s["inputs"]} for s in loaded}
        code = {s["name"]: "".join(v["code"] for v in s["variants"]) for s in loaded}
        for trade in root.iter("Trade"):
            data = trade.find("ScriptedTradeData")
            if data is None or not data.findtext("ScriptName"):
                continue
            inputs = mined.get(data.findtext("ScriptName").strip())
            if inputs is None:
                continue
            for item in data.find("Data") if data.find("Data") is not None else []:
                name = item.findtext("Name")
                stated_array = (item.find("Values") is not None or item.find("ScheduleData") is not None
                                or item.find("DerivedSchedule") is not None)
                got = inputs.get(name)
                script = data.findtext("ScriptName").strip()
                if got is None and not re.search(rf"\b{re.escape(name)}\b", code[script]):
                    # The trade states a variable its script never reads, which
                    # ORE ignores; it is the trade's extra, not a missed input.
                    extras += 1
                elif got is None:
                    missing.append(f"{script}: {name}")
                elif got[1] == item.tag and got[2] == stated_array:
                    agree += 1
                else:
                    disagree.append(f"{name}: mined {got[1]}{'[]' if got[2] else ''}, "
                                    f"stated {item.tag}{'[]' if stated_array else ''}")
    print(f"agree {agree}, disagree {len(disagree)}, not mined {len(missing)}, "
          f"stated but never read {extras}")
    for line in sorted(set(disagree)) + sorted(set(missing)):
        print("  " + line)
    return 0 if not disagree and not missing else 1


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true")
    parser.add_argument("--compare", action="store_true")
    args = parser.parse_args()
    if args.compare:
        return compare()
    text = seed(load(LIBRARY))
    if args.check:
        if not SEED.exists() or SEED.read_text(encoding="utf-8") != text:
            print(f"{SEED.relative_to(ROOT)} is not what ORE's script library gives; "
                  f"run {Path(__file__).relative_to(ROOT)}")
            return 1
        return 0
    SEED.write_text(text, encoding="utf-8")
    return 0


if __name__ == "__main__":
    sys.exit(main())
