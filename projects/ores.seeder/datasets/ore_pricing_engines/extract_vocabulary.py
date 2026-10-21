"""
Reads the ORE user guide chapter "Pricing Engine Configuration" and turns it into the
vocabulary of the pricing engine screens: the product types, the pairs of model and engine
that price each type, and the settings each pair takes.

The chapter is the source: Docs/UserGuide/pricing/pricingengines.tex in an ORE checkout.
Where the chapter disagrees with the ORE code on a name, CORRECTIONS holds the code's
spelling. Every model, engine and product type name is checked against the string literals
of the code, and the names that the code does not know are reported and dropped.

Usage:
    python3 extract_vocabulary.py <ore-checkout> <out.json>
"""
import collections
import json
import os
import re
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import descriptions as authored

CHAPTER = "Docs/UserGuide/pricing/pricingengines.tex"

# The chapter spells these differently from the code.
TYPE_FIX = {
    "CurrencySwap": "CrossCurrencySwap",
    "AmericanSwaption NonStandard": "AmericanSwaption_NonStandard",
    "CappedFlooredYYLeg": "CapFlooredYYLeg",
    "CappedFlooredNonStdYYLeg": "CapFlooredNonStdYYLeg",
    "CapFlooredAverageBMAIndexedCouponLeg": "CapFlooredAverageBMACouponLeg",
}
ENGINE_FIX = {
    "AnalyticEuroepanForwardEngine": "AnalyticEuropeanForwardEngine",
    "FdGarmanKohlhagenVanillaEngine": "FdBlackScholesVanillaEngine",
    "BlackAverageBMAIndexedCouponPricer": "BlackAverageBMACouponPricer",
}
MODEL_FIX = {"CapFlooredYNonStdYYModel": "CapFlooredNonStdYYModel"}

TENOR = re.compile(r"^\d+[DWMY]$")
NUMBER = re.compile(r"^-?\d+(\.\d+)?([eE]-?\d+)?$")
WORD = r"[A-Za-z0-9_.]+"


def clean(s):
    s = re.sub(r"\\verb\+([^+]*)\+", r"\1", s)
    s = s.replace("\\_", "_").replace("\\&", "&").replace("\\%", "%").replace("\\ ", " ")
    s = re.sub(r"``|''", '"', s)
    s = re.sub(r"\\(?:ref|cite|label)\{[^}]*\}", "", s)
    s = re.sub(r"\\(?:texttt|emph|textbf|textit|verb|lstinline)[!+]?\{?([^}!+]*)[}!+]?", r"\1", s)
    s = re.sub(r"\{\\tt ([^}]*)\}", r"\1", s)
    s = re.sub(r"\$([^$]*)\$", r"\1", s)
    s = re.sub(r"%.*", "", s)
    s = re.sub(r"\s+", " ", s).strip()
    return s


def plain(text):
    for wrong, right in authored.TYPOS.items():
        text = text.replace(wrong, right)
    text = re.sub(r"\\(?:Phi|epsilon|eta|rho|sigma|alpha|cdot|frac)", lambda m: m.group(0)[1:], text)
    text = re.sub(r"[\\{}]", "", text)
    text = re.sub(r"\s+", " ", text).strip()
    text = text.replace(" ,", ",").replace(" .", ".")
    if text:
        text = text[0].upper() + text[1:]
        if not text.endswith((".", "?", "!")):
            text += "."
    return text


def sentence(s):
    s = clean(s)
    s = re.sub(r"\s*A sample configuration (is shown )?in( the)? listing\s*\.?", "", s)
    s = re.sub(r"\s*Model Parameters:.*", "", s)
    s = s.strip()
    if s and not s.endswith("."):
        s += "."
    return s


def sections(text):
    parts = re.split(r"\\subsection\{Product Type: ([^}]*)\}", text)
    return [(parts[i], parts[i + 1]) for i in range(1, len(parts), 2)]


def split_name(s):
    return re.sub(r"\s*\(.*\)\s*$", "", s).strip(), (re.search(r"\((.*)\)", s) or [None, ""])[1]


def parameters(chunk, sample):
    pm = re.search(r"The parameters have the following meaning:\s*\\begin\{itemize\}(.*?)\\end\{itemize\}", chunk, re.S)
    found = []
    if pm:
        for item in re.split(r"\\item\s+", pm.group(1))[1:]:
            item = clean(item)
            mm = re.match(r"([^:\[]+?)\s*(\[optional\])?\s*:\s*(.*)$", item)
            if not mm:
                continue
            for name in re.split(r"\s*,\s*| and ", mm.group(1)):
                name = name.strip().replace("\\", "")
                name = authored.SETTING_NAME_FIX.get(name, name)
                if name and " " not in name and name not in authored.NOT_SETTINGS:
                    found.append({"name": name, "optional": bool(mm.group(2)), "text": mm.group(3).strip()})
    return found


def sample_values(chunk):
    out = {"model": {}, "engine": {}}
    xm = re.search(r"\\begin\{minted\}[^\n]*\n(.*?)\\end\{minted\}", chunk, re.S)
    if not xm:
        return out
    xml = xm.group(1)
    for key, tag in (("model", "ModelParameters"), ("engine", "EngineParameters")):
        mt = re.search(rf"<{tag}>(.*?)</{tag}>", xml, re.S)
        if mt:
            for n, v in re.findall(r'<Parameter name="([^"]+)">([^<]*)</Param', mt.group(1)):
                out[key][n] = v.strip()
    return out


def blocks_of(body):
    desc = body.split("Engine description:", 1)[1] if "Engine description:" in body else ""
    pat = re.compile(
        r"(?m)^([A-Za-z0-9_\\]+/[A-Za-z0-9_\\]+)\s+(.*?)(?=^[A-Za-z0-9_\\]+/[A-Za-z0-9_\\]+\s|\Z)", re.S)
    blocks = {}
    for bm in pat.finditer(desc):
        model, engine = clean(bm.group(1)).split("/", 1)
        chunk = bm.group(2)
        lead = re.split(r"The parameters have the following meaning|\\begin\{longlisting\}|\\begin\{equation\}", chunk)[0]
        sample = sample_values(chunk)
        blocks[ENGINE_FIX.get(engine, engine)] = {
            "model": MODEL_FIX.get(model, model),
            "text": sentence(lead),
            "parameters": parameters(chunk, sample),
            "sample": sample,
        }
    return blocks


def build(text):
    by_type = collections.OrderedDict()
    for title, body in sections(text):
        types = [TYPE_FIX.get(clean(t.strip()), clean(t.strip())) for t in title.split(",")]
        used = re.search(r"Used by trade type:(.*?)Available Model/Engine pairs:", body, re.S)
        used = sentence(used.group(1)) if used else ""
        listed = []
        lm = re.search(r"Available Model/Engine pairs:\s*\\begin\{itemize\}(.*?)\\end\{itemize\}", body, re.S)
        if lm:
            for item in re.findall(r"\\item\s*(.*)", lm.group(1)):
                name, note = split_name(clean(item))
                if "/" in name:
                    model, engine = name.split("/", 1)
                    listed.append((MODEL_FIX.get(model.strip(), model.strip()),
                                   ENGINE_FIX.get(engine.strip(), engine.strip()), note))
        blocks = blocks_of(body)
        order = list(listed)
        known = {e for _, e, _ in listed}
        for engine, block in blocks.items():
            if engine not in known:
                order.append((block["model"], engine, ""))
        for t in types:
            area = next((a for a in ("fx", "equity", "commodity") if t.lower().startswith(a)), None)
            pairs = collections.OrderedDict()
            for index, (model, engine, note) in enumerate(order):
                if len(types) > 1 and area and note:
                    others = [a for a in ("fx", "equity", "commodity") if a != area and a in note]
                    if others and area not in note:
                        continue
                pairs[(model, engine)] = {
                    "model": model, "engine": engine, "usual": not pairs,
                    "block": blocks.get(engine),
                }
            by_type[t] = {"used_by": used, "pairs": pairs}
    for t in list(by_type):
        if t.endswith("_XCcy") or t.endswith("_NonStandard"):
            base = t.rsplit("_", 1)[0]
            if base not in by_type:
                continue
            if not by_type[t]["pairs"]:
                by_type[t]["pairs"] = collections.OrderedDict(
                    (k, dict(v)) for k, v in by_type[base]["pairs"].items())
            for k, row in by_type[t]["pairs"].items():
                if row["block"] is None and k in by_type[base]["pairs"]:
                    row["block"] = by_type[base]["pairs"][k]["block"]
    return by_type


def known_in_code(ore, names):
    unknown = []
    for name in sorted(names):
        found = subprocess.run(
            ["grep", "-rlF", f'"{name}"', f"{ore}/OREData/ored", f"{ore}/OREAnalytics/orea"],
            capture_output=True, text=True)
        if not found.stdout.strip():
            unknown.append(name)
    return unknown


def options_of(text):
    text = text.rstrip(".")
    m = re.search(rf"(?:can be|one of|either|values?:)\s+({WORD}(?:\s*(?:,|\bor\b)\s*{WORD})+)", text)
    if m:
        return [x for x in re.split(r"\s*(?:,|\bor\b)\s*", m.group(1)) if x]
    m = re.match(rf"^({WORD}(?:\s*,\s*{WORD})+)$", text)
    if m:
        return [x.strip() for x in m.group(1).split(",")]
    m = re.match(rf"^({WORD}(?:\s*,\s*{WORD})+)\s*[.;(]", text)
    if m:
        return [x.strip() for x in m.group(1).split(",")]
    if re.search(r"\btrue or false\b|\bwhether\b|\bif true\b|\bif false\b", text, re.I):
        return ["true", "false"]
    return []


def settings_of(pair):
    block = pair["block"]
    if not block:
        return []
    rows = collections.OrderedDict()
    sample = block["sample"]
    for p in block["parameters"]:
        name = p["name"]
        scope = "model" if name in sample["model"] else "engine"
        value = sample[scope].get(name)
        if value is None and name in sample["engine" if scope == "model" else "model"]:
            scope = "engine" if scope == "model" else "model"
            value = sample[scope].get(name)
        options = options_of(p["text"])
        if value is not None and value in ("true", "false"):
            options = ["true", "false"]
        if name == "SensitivityTemplate":
            kind, options = "reference", []
        elif value is not None and TENOR.match(value) and ("Period" in name or "Tenor" in name or "Step" in name):
            kind, options = "tenor", []
        elif options:
            kind = "select"
        elif value is not None and NUMBER.match(value):
            kind = "number"
        else:
            kind = "text"
        if kind == "select" and value is not None and value not in options and value not in ("",):
            options = options if len(options) < 2 else options
        rows[(scope, name)] = {
            "scope": scope, "name": name, "kind": kind, "usual": value, "options": options,
            "required": not (p["optional"] or re.search(r"optional|default", p["text"], re.I)),
            "text": authored.SETTING_TEXT.get(name) or plain(p["text"]),
        }
    return list(rows.values())


# The settings whose allowed values the ORE parsers fix. The parser is the authority, since
# the guide lists the values from memory and misspells some of them.
CODE_ENUMS = {
    "Scheme": "parseFdmSchemeDesc",
    "SalvagingAlgorithm": "parseSalvagingAlgorithmType",
    "Training.Sequence": "parseSequenceType",
    "Pricing.Sequence": "parseSequenceType",
    "Training.BasisFunction": "parsePolynomType",
    "BrownianBridgeOrdering": "parseSobolBrownianGeneratorOrdering",
    "SobolDirectionIntegers": "parseSobolRsgDirectionIntegers",
    "RegressorModel": "parseRegressorModel",
}
TRUTH = {"y": "true", "yes": "true", "true": "true", "n": "false", "no": "false", "false": "false"}


def parser_values(ore, function):
    text = open(f"{ore}/OREData/ored/utilities/parsers.cpp", encoding="utf-8").read()
    start = re.search(rf"^[A-Za-z:<>_ ]+\b{function}\(const std::string& s\) \{{", text, re.M)
    if not start:
        return []
    end = text.index("\n}\n", start.end())
    body = text[start.end():end]
    values = re.findall(r'\{"([A-Za-z0-9_]+)",', body) + re.findall(r's == "([A-Za-z0-9_]+)"', body)
    return list(dict.fromkeys(values))


def consolidate(result, ore):
    enums = {name: parser_values(ore, fn) for name, fn in CODE_ENUMS.items()}
    union = collections.defaultdict(list)
    for t in result.values():
        for pair in t["pairs"]:
            for st in pair["settings"]:
                if st["name"] in enums and enums[st["name"]]:
                    st["kind"], st["options"] = "select", enums[st["name"]]
                if st["usual"] is not None and st["usual"].lower() in TRUTH and st["options"] == ["true", "false"]:
                    st["usual"] = TRUTH[st["usual"].lower()]
                if st["kind"] == "select":
                    for o in st["options"]:
                        if o not in union[st["name"]]:
                            union[st["name"]].append(o)
    for t in result.values():
        for pair in t["pairs"]:
            for st in pair["settings"]:
                if st["name"] in union and st["kind"] in ("select", "text") and st["name"] not in enums:
                    st["kind"], st["options"] = "select", union[st["name"]]
                if st["usual"] is not None and st["options"] == ["true", "false"]:
                    st["usual"] = TRUTH.get(st["usual"].lower(), st["usual"])


def main_result(ore):
    text = open(f"{ore}/{CHAPTER}", encoding="utf-8").read()
    by_type = build(text)
    names = set()
    for t, v in by_type.items():
        base = re.sub(r"_(XCcy|NonStandard|Vanilla|Structured|TLock)(_XCcy)?$", "", t)
        names.add(base)
        for model, engine in v["pairs"]:
            names.add(model)
            names.add(engine)
    unknown = set(known_in_code(ore, names))
    result = collections.OrderedDict()
    dropped = []
    for t, v in by_type.items():
        base = re.sub(r"_(XCcy|NonStandard|Vanilla|Structured|TLock)(_XCcy)?$", "", t)
        if base in unknown:
            dropped.append(t)
            continue
        pairs = []
        for (model, engine), pair in v["pairs"].items():
            if model in unknown or engine in unknown:
                dropped.append(f"{t}:{model}/{engine}")
                continue
            block = pair["block"]
            pairs.append({
                "model": model, "engine": engine, "usual": pair["usual"],
                "note": plain(block["text"]) if block else "",
                "settings": settings_of(pair),
            })
        result[t] = {"used_by": v["used_by"], "pairs": pairs}
    consolidate(result, ore)
    print(len(result), "types", sum(len(v["pairs"]) for v in result.values()), "pairs",
          sum(len(p["settings"]) for v in result.values() for p in v["pairs"]), "settings")
    print("dropped (name unknown to the ORE code):", dropped)
    return result


def main(ore, out):
    json.dump(main_result(ore), open(out, "w"), indent=1)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
