#!/usr/bin/env python3
"""
Codegen for oresmd quote-type families.

Reads YAML specs from projects/ores.codegen/modeling/oresmd/ and
generates the boilerplate that currently has to be hand-written for
every new asset class or quote-type extension.

Usage:
    python3 oresmd_codegen.py                    # generate into repo
    python3 oresmd_codegen.py --check            # verify no diffs against committed code
    python3 oresmd_codegen.py --spec fx          # generate only one asset class
"""

import argparse
import sys
from pathlib import Path
from typing import Any

import yaml


def repo_root() -> Path:
    """Find the repository root."""
    current = Path(__file__).resolve()
    for parent in current.parents:
        if (parent / ".git").exists():
            return parent
    raise RuntimeError("Repository root not found")


def load_specs(spec_dir: Path, asset_class: str | None = None) -> dict[str, Any]:
    """Load all oresmd YAML specs from the modeling directory."""
    specs = {}
    for yaml_file in sorted(spec_dir.glob("*.yaml")):
        key = yaml_file.stem.replace("_quote_type", "")
        if asset_class and key != asset_class:
            continue
        with open(yaml_file) as f:
            specs[key] = yaml.safe_load(f)
    return specs


# ---------------------------------------------------------------------------
# Enum generation
# ---------------------------------------------------------------------------

def generate_enums(specs: dict[str, Any]) -> str:
    """Generate enum definitions for all quote types across all asset classes."""
    enums = []

    for key, spec in sorted(specs.items()):
        asset_class = spec["asset_class"]
        qts = spec["quote_types"]
        suffix = _enum_suffix(key)
        enum_name = f"{key}_quote_type"
        doc = _enum_doc(spec)

        values = []
        for qt in qts:
            values.append(f"    {qt['name']},  ///< {qt['ore_type']}/{qt['ore_metric']}")

        comment = f"    // {_descope_comment(spec)}" if _has_descope(spec) else ""
        if comment:
            values.append(comment)

        enums.append(f"""{doc}
enum class {enum_name} {{
{chr(10).join(values)}
}};""")

    return "\n\n".join(enums)


def _enum_suffix(key: str) -> str:
    """Map spec key to enum type suffix used in oresmd_enums.hpp."""
    return "quote_type"


def _enum_doc(spec: dict) -> str:
    """Generate the Doxygen comment block for an enum."""
    ac = spec["asset_class"]
    types = ", ".join(qt["name"] for qt in spec["quote_types"])
    return f"""/**
 * @brief The `quote` query key for {ac} instruments — the ORE TYPE. {ac.capitalize()}-only;
 * only meaningful when `type=quote`.
 */"""


def _descope_comment(spec: dict) -> str:
    """Return a comment about descoped enum values, if any."""
    return ""


def _has_descope(spec: dict) -> bool:
    return False


# ---------------------------------------------------------------------------
# Projection generation
# ---------------------------------------------------------------------------

def generate_projections(specs: dict[str, Any]) -> str:
    """Generate ore_type, ore_metric, and quote_key functions for all asset classes."""
    funcs = []

    for key, spec in sorted(specs.items()):
        qts = spec["quote_types"]
        default = spec.get("default_quote_type", qts[0]["name"])
        entity_field = spec["entity_field"]["name"]

        # ore_type() helper
        ore_type_body = _generate_ore_type(key, qts)
        funcs.append(ore_type_body)

        # ore_*_metric() helper
        ore_metric_body = _generate_ore_metric(key, qts)
        funcs.append(ore_metric_body)

        # quote_key_*() function
        quote_key_body = _generate_quote_key(key, spec)
        funcs.append(quote_key_body)

    return "\n\n".join(funcs)


def _generate_ore_type(key: str, qts: list) -> str:
    """Generate the ore_type() switch function."""
    cases = []
    for qt in qts:
        cases.append(f"    case {key}_quote_type::{qt['name']}: return \"{qt['ore_type']}\";")
    default_type = qts[0]["ore_type"]
    return f"""std::string_view ore_type({key}_quote_type qt) {{
    switch (qt) {{
{chr(10).join(cases)}
    }}
    return "{default_type}";
}}"""


def _generate_ore_metric(key: str, qts: list) -> str:
    """Generate the ore_*_metric() switch function."""
    cases = []
    for qt in qts:
        cases.append(f"    case {key}_quote_type::{qt['name']}: return \"{qt['ore_metric']}\";")
    default_metric = qts[0]["ore_metric"]
    func_name = "ore_metric" if key == "ir" else f"ore_{key}_metric"
    return f"""std::string_view {func_name}({key}_quote_type qt) {{
    switch (qt) {{
{chr(10).join(cases)}
    }}
    return "{default_metric}";
}}"""


def _generate_quote_key(key: str, spec: dict) -> str:
    """Generate the quote_key_*() function body."""
    qts = spec["quote_types"]
    default = spec.get("default_quote_type", qts[0]["name"])
    entity = _entity_accessor(key, spec)
    func_name = f"quote_key_{key}"
    ident_type = f"{key}_market_data_identifier"

    scalar_qt = [qt for qt in qts if qt["point"] == "scalar"]
    curve_qts = [qt for qt in qts if qt["point"] != "scalar"]

    lines = [f"std::optional<std::string> {func_name}(const {ident_type}& id) {{"]
    lines.append("    if (id.type != instrument_type::quote)")
    lines.append("        return std::nullopt;")
    lines.append(f"    const auto qt = id.quote_type.value_or({key}_quote_type::{default});")

    entity_fmt, entity_args = _quote_key_format(key, spec, scalar=True)
    if scalar_qt:
        scalar = scalar_qt[0]
        lines.append(f"    // {scalar['name']}: scalar, 4-segment key.")
        lines.append(f"    if (qt == {key}_quote_type::{scalar['name']})")
        lines.append(f"        return std::format(\"{{}}/{{}}{entity_fmt}\", ore_type(qt), ore_{_metric_func(key)}(qt){entity_args});")

    for qt in curve_qts:
        entity_fmt, entity_args = _quote_key_format(key, spec, scalar=False)
        lines.append(f"    // {qt['name']}: curve, needs point for tenor.")
        lines.append(f"    if (qt == {key}_quote_type::{qt['name']}) {{")
        lines.append("        if (!id.point)")
        lines.append("            return std::nullopt;")
        lines.append(f"        return std::format(\"{{}}/{{}}{entity_fmt}/{{}}\", ore_type(qt), ore_{_metric_func(key)}(qt){entity_args}, to_upper(*id.point));")
        lines.append("    }")

    lines.append("    return std::nullopt;")
    lines.append("}")

    return "\n".join(lines)


def _entity_accessor(key: str, spec: dict) -> str:
    """How to access the entity field for the quote key format string."""
    ef = spec["entity_field"]
    name = ef["name"]
    if key == "fx":
        return f"id.{name}.substr(0, 3), id.{name}.substr(3, 3)"
    return f"id.{name}"


def _metric_func(key: str) -> str:
    return "metric" if key == "ir" else f"{key}_metric"


def _quote_key_format(key: str, spec: dict, scalar: bool) -> tuple[str, str]:
    """Return (format_fragment, args_string) for the entity part of a quote key."""
    if key == "fx":
        # FX: pair is a 6-char currency pair → split into CCY1/CCY2 — two format segments.
        return "/{}/{}", ", id.pair.substr(0, 3), id.pair.substr(3, 3)"
    if _has_ccy_field(spec) and not scalar:
        # Equity/credit/commodity curves: TYPE/METRIC/ENTITY/CCY/TENOR
        return "/{}/{}", ", id." + spec["entity_field"]["name"] + ", id.ccy"
    # Scalar: TYPE/METRIC/ENTITY or TYPE/METRIC/ENTITY/CCY
    if _has_ccy_field(spec):
        return "/{}/{}", ", id." + spec["entity_field"]["name"] + ", id.ccy"
    return "/{}", ", id." + spec["entity_field"]["name"]


def _has_ccy_field(spec: dict) -> bool:
    """Does this asset class have a separate ccy field (as opposed to entity-is-ccy)?"""
    return spec["asset_class"] in ("equity", "credit", "commodity")


# ---------------------------------------------------------------------------
# Test generation
# ---------------------------------------------------------------------------

def generate_projection_tests(specs: dict[str, Any]) -> str:
    """Generate Catch2 TEST_CASE blocks for projections."""
    cases = []
    for key, spec in sorted(specs.items()):
        for test in spec.get("tests", {}).get("projections", []):
            cases.append(f"""TEST_CASE("{test['description']}", tags) {{
    const auto id = parse("{test['uri']}");
    REQUIRE(oresmd_projections::to_quote_key(id) == "{test['expected']}");
}}""")
        # Add default test if applicable
        default_qt = spec.get("default_quote_type")
        if default_qt:
            scalar = [qt for qt in spec["quote_types"] if qt["name"] == default_qt and qt["point"] == "scalar"]
            if scalar:
                default_uri = test.get("uri", "").replace(f"&quote={default_qt}", "")
    return "\n\n".join(cases)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(description="Codegen for oresmd quote types")
    parser.add_argument("--check", action="store_true",
                        help="Verify generated code matches committed code")
    parser.add_argument("--spec", type=str,
                        help="Generate only one asset class (e.g. fx, ir)")
    parser.add_argument("--output-dir", type=str,
                        help="Output directory (default: repo root)")
    args = parser.parse_args()

    root = repo_root()
    spec_dir = root / "projects" / "ores.codegen" / "modeling" / "oresmd"

    if not spec_dir.exists():
        print(f"Spec directory not found: {spec_dir}", file=sys.stderr)
        sys.exit(1)

    specs = load_specs(spec_dir, args.spec)
    if not specs:
        print("No specs found.", file=sys.stderr)
        sys.exit(1)

    print(f"// Generated enums for: {', '.join(sorted(specs))}")
    print(generate_enums(specs))
    print()
    print("// --- Projections ---")
    print(generate_projections(specs))
    print()
    print("// --- Tests ---")
    print(generate_projection_tests(specs))


if __name__ == "__main__":
    main()
