#!/usr/bin/env python3
"""Reapply the ORE lexical-form hand-patch lost on xsdcpp regen.

xsdcpp emits xsd::base<T> as a bare T plus the parse helpers set_float and
set_double, which throw away the document's own digits. A money leaf must
survive the ORE boundary as the exact decimal the document wrote, so this
project keeps the digits beside the parsed float. That change spans two
generated files, plus the FRA's two bare-float members in a third:

  * domain_xsd.hpp grows a conditional lexical_form<T> base, and xsd::base<T>
    derives from it for floating-point T only.
  * domain.cpp's 37 floating-point leaf setters stash the document's text on
    the binding, right after the original set_float/set_double call.

The two helpers themselves are left byte-for-byte as xsdcpp emits them. The
generated leaf tables take `&xsdcpp::set_float` as a function pointer, so each
name must stay a single, non-overloaded function; the stash therefore lives in
the setter body, not in the helper.

Both files are regenerated wholesale by xsdcpp_generate_ore.sh, which drops the
patch. This script re-applies it from fixed anchors on the freshly regenerated
files. It fails loudly -- non-zero exit, a message naming the missing anchor --
when an anchor is absent, because a silent no-op is exactly the failure this
exists to prevent. It is idempotent: a file that already carries the patch is
left alone.

Usage:
    scripts/reapply_ore_lexical_form.py
    scripts/reapply_ore_lexical_form.py --domain-xsd PATH --domain-cpp PATH
    scripts/reapply_ore_lexical_form.py --self-test
    scripts/reapply_ore_lexical_form.py --self-test-missing-anchor
"""
import argparse
import re
import shutil
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent

DEFAULT_DOMAIN_XSD = (
    REPO_ROOT / "projects/ores.ore/core/include/ores.ore.core/domain/domain_xsd.hpp"
)
DEFAULT_DOMAIN_CPP = REPO_ROOT / "projects/ores.ore/core/src/domain/domain.cpp"
DEFAULT_DOMAIN_HPP = REPO_ROOT / "projects/ores.ore/core/include/ores.ore.core/domain/domain.hpp"


class AnchorError(RuntimeError):
    """An anchor the patch needs is missing from a generated file."""


# ---------------------------------------------------------------------------
# domain_xsd.hpp
# ---------------------------------------------------------------------------

XSD_INCLUDES_OLD = "#    include <string>\n#    include <vector>\n"
XSD_INCLUDES_NEW = (
    "#    include <string>\n"
    "#    include <type_traits>\n"
    "#    include <utility>\n"
    "#    include <vector>\n"
)

XSD_BASE_ANCHOR = "template <typename T>\nclass base {\n"

XSD_LEXICAL_NEW = """// The document's own digits are only worth keeping for floating-point leaves.
// A money quantity must survive the boundary as the exact decimal the document
// wrote, not as the binary float nearest to it.  Every other base<T> stays a
// bare value; only the floating instantiations grow a std::string.
template <typename T, bool = std::is_floating_point_v<T>>
struct lexical_form {};

template <typename T>
struct lexical_form<T, true> {
    const std::string& lexical() const {
        return _lexical;
    }
    void lexical(std::string value) {
        _lexical = std::move(value);
    }

private:
    std::string _lexical;
};

template <typename T>
class base : public lexical_form<T> {
"""


def patch_xsd(text: str) -> str:
    if "struct lexical_form" in text:
        if "#    include <type_traits>" not in text or "#    include <utility>" not in text:
            raise AnchorError(
                "domain_xsd.hpp already defines lexical_form but is missing the "
                "<type_traits>/<utility> includes; refusing to guess"
            )
        return text
    if XSD_INCLUDES_OLD not in text:
        raise AnchorError("domain_xsd.hpp: the '#include <string>/<vector>' include block is missing")
    if XSD_BASE_ANCHOR not in text:
        raise AnchorError("domain_xsd.hpp: the 'template <typename T>\\nclass base {' anchor is missing")
    text = text.replace(XSD_INCLUDES_OLD, XSD_INCLUDES_NEW, 1)
    text = text.replace(XSD_BASE_ANCHOR, XSD_LEXICAL_NEW, 1)
    return text


def unpatch_xsd(text: str) -> str:
    if XSD_LEXICAL_NEW not in text:
        raise AnchorError("domain_xsd.hpp: patched lexical_form block not found to un-patch")
    if XSD_INCLUDES_NEW not in text:
        raise AnchorError("domain_xsd.hpp: patched include block not found to un-patch")
    text = text.replace(XSD_INCLUDES_NEW, XSD_INCLUDES_OLD, 1)
    text = text.replace(XSD_LEXICAL_NEW, XSD_BASE_ANCHOR, 1)
    return text


# ---------------------------------------------------------------------------
# domain.cpp
# ---------------------------------------------------------------------------

# The exact text xsdcpp emits. The patch must leave both alone: the leaf tables
# take these two names as function pointers.
CPP_DECL_ORIGINAL = (
    "void set_float(float* obj, const Position& pos, std::string&& val);\n"
    "void set_double(double* obj, const Position& pos, std::string&& val);\n"
)
CPP_DEF_ORIGINAL = """void set_float(float* obj, const Position& pos, std::string&& val) {
    std::stringstream ss(val);
    if (!(ss >> *obj))
        throw VerificationException(pos, "Expected single precision floating point value");
}
void set_double(double* obj, const Position& pos, std::string&& val) {
    std::stringstream ss(val);
    if (!(ss >> *obj))
        throw VerificationException(pos, "Expected double precision floating point value");
}
"""

FLOAT_CALL = "xsdcpp::set_float(&base, pos, std::move(val));\n"
DOUBLE_CALL = "xsdcpp::set_double(&base, pos, std::move(val));\n"
STASH = "obj->lexical(val);"


def _stash(text: str, call: str) -> tuple[str, int]:
    """Add the lexical stash after each `call` line that lacks one."""
    pattern = re.compile(r"([ \t]*)" + re.escape(call) + r"(?!\s*obj->lexical\(val\);)")

    def repl(m: re.Match) -> str:
        return m.group(0) + m.group(1) + STASH + "\n"

    return pattern.subn(repl, text)


# The writer prints a float with nine significant digits, which turns 0.0065
# into 0.00650000013. A leaf that carries its own digits writes those instead,
# so the digits that enter at the boundary are the digits that leave.
GET_STRING_DOUBLE = """inline std::string get_string(double v) {
    std::ostringstream ss;
    ss << std::setprecision(17) << v;
    return ss.str();
}
"""
GET_STRING_LEXICAL = """
inline std::string get_string(float v, const std::string& lexical) {
    return lexical.empty() ? get_string(v) : lexical;
}

inline std::string get_string(double v, const std::string& lexical) {
    return lexical.empty() ? get_string(v) : lexical;
}
"""
WRITE_FLOAT_OLD = "xsdcpp::get_string(static_cast<float>(v))"
WRITE_FLOAT_NEW = "xsdcpp::get_string(static_cast<float>(v), v.lexical())"
WRITE_DOUBLE_OLD = "xsdcpp::get_string(static_cast<double>(v))"
WRITE_DOUBLE_NEW = "xsdcpp::get_string(static_cast<double>(v), v.lexical())"


def _patch_writer(text: str) -> tuple[str, int]:
    if GET_STRING_DOUBLE not in text:
        raise AnchorError("domain.cpp: the original get_string(double) definition is missing")
    if GET_STRING_LEXICAL not in text:
        text = text.replace(GET_STRING_DOUBLE, GET_STRING_DOUBLE + GET_STRING_LEXICAL, 1)
    text = text.replace(WRITE_FLOAT_OLD, WRITE_FLOAT_NEW)
    text = text.replace(WRITE_DOUBLE_OLD, WRITE_DOUBLE_NEW)
    n_write = text.count(WRITE_FLOAT_NEW) + text.count(WRITE_DOUBLE_NEW)
    if n_write == 0:
        raise AnchorError("domain.cpp: a floating-point writer anchor is missing")
    return text, n_write


def _unpatch_writer(text: str) -> str:
    text = text.replace(GET_STRING_LEXICAL, "")
    text = text.replace(WRITE_FLOAT_NEW, WRITE_FLOAT_OLD)
    return text.replace(WRITE_DOUBLE_NEW, WRITE_DOUBLE_OLD)


def patch_cpp(text: str) -> tuple[str, dict[str, int]]:
    if CPP_DECL_ORIGINAL not in text:
        raise AnchorError("domain.cpp: the original set_float/set_double declarations are missing")
    if CPP_DEF_ORIGINAL not in text:
        raise AnchorError(
            "domain.cpp: the original set_float/set_double definitions are missing; "
            "xsdcpp's output shape changed and the patch must be re-derived"
        )

    text, _ = _stash(text, FLOAT_CALL)
    text, _ = _stash(text, DOUBLE_CALL)

    n_float = text.count("xsdcpp::set_float(&base, pos, std::move(val));")
    n_double = text.count("xsdcpp::set_double(&base, pos, std::move(val));")
    if n_float == 0 or n_double == 0:
        raise AnchorError("domain.cpp: a floating-point setter-body anchor is missing")
    n_stash = text.count(STASH)
    if n_stash != n_float + n_double:
        raise AnchorError(
            f"domain.cpp: {n_stash} lexical stashes for {n_float + n_double} setters; "
            "the patch is half-applied"
        )
    text, n_write = _patch_writer(text)
    if n_write != n_float + n_double:
        raise AnchorError(
            f"domain.cpp: {n_write} lexical writers for {n_float + n_double} setters; "
            "a floating-point serializer has a shape the patch does not know"
        )
    return text, {"float_setters": n_float, "double_setters": n_double, "writers": n_write}


def unpatch_cpp(text: str) -> str:
    if STASH not in text:
        raise AnchorError("domain.cpp: no lexical stashes found to un-patch")
    text = re.sub(r"[ \t]*" + re.escape(STASH) + r"\n", "", text)
    return _unpatch_writer(text)


# ---------------------------------------------------------------------------
# Money members that xsdcpp emits as a bare float
# ---------------------------------------------------------------------------

# A forward rate agreement states its strike and notional as plain xs:float
# elements, which the binding holds as bare float members. A bare float has
# nowhere to keep the document's digits, so the two members become a leaf that
# does. The leaf is `moneyFloat`, a lexical xsd::base<float>, with its own
# setter, element info and serializer.
FRA_STRUCT_OLD = """struct forwardRateAgreementData {
    domain::date StartDate{};
    domain::date EndDate{};
    domain::currencyCode Currency{};
    domain::forwardRateAgreementData_Index_t Index{};
    domain::longShort LongShort{};
    float Strike{};
    float Notional{};
};
"""
FRA_STRUCT_NEW = """struct moneyFloat : xsd::base<float> {};

struct forwardRateAgreementData {
    domain::date StartDate{};
    domain::date EndDate{};
    domain::currencyCode Currency{};
    domain::forwardRateAgreementData_Index_t Index{};
    domain::longShort LongShort{};
    domain::moneyFloat Strike{};
    domain::moneyFloat Notional{};
};
"""

MONEY_EXTERN_ANCHOR = "extern const xsdcpp::ElementInfo _forwardRateAgreementData_Info;\n"
MONEY_EXTERN = "extern const xsdcpp::ElementInfo _moneyFloat_Info;\n"

MONEY_SETTER_ANCHOR = "void _set_stFreeStyleNumber(domain::stFreeStyleNumber* obj,\n"
MONEY_SETTER = """void _set_moneyFloat(domain::moneyFloat* obj, const xsdcpp::Position& pos, std::string&& val) {
    float& base = *obj;
    xsdcpp::set_float(&base, pos, std::move(val));
    obj->lexical(val);
}
"""

MONEY_INFO_ANCHOR = "const xsdcpp::ElementInfo _stFreeStyleNumber_Info = {\n"
MONEY_INFO = """const xsdcpp::ElementInfo _moneyFloat_Info = {
    xsdcpp::ElementInfo::ReadTextFlag, (xsdcpp::set_value_t)&domain::_set_moneyFloat};
"""

FRA_CHILD_OLD = """     (xsdcpp::get_field_t)&_get_forwardRateAgreementData_Strike,
     &domain::_float_Info,
"""
FRA_CHILD_NEW = """     (xsdcpp::get_field_t)&_get_forwardRateAgreementData_Strike,
     &domain::_moneyFloat_Info,
"""
FRA_CHILD2_OLD = """     (xsdcpp::get_field_t)&_get_forwardRateAgreementData_Notional,
     &domain::_float_Info,
"""
FRA_CHILD2_NEW = """     (xsdcpp::get_field_t)&_get_forwardRateAgreementData_Notional,
     &domain::_moneyFloat_Info,
"""

MONEY_SER_DECL_ANCHOR = "void _serialize_float(xsdcpp::XmlWriter& w, const char* name, float v);\n"
MONEY_SER_DECL = (
    "void _serialize_moneyFloat(xsdcpp::XmlWriter& w, const char* name, "
    "const domain::moneyFloat& v);\n"
)
MONEY_SER_DEF_ANCHOR = """XSDCPP_MAYBE_UNUSED void _serialize_float(xsdcpp::XmlWriter& w, const char* name, float v) {
    w.startElement(name);
    w.writeText(xsdcpp::get_string(v));
    w.endElement(name);
}

"""
MONEY_SER_DEF = """XSDCPP_MAYBE_UNUSED void
_serialize_moneyFloat(xsdcpp::XmlWriter& w, const char* name, const domain::moneyFloat& v) {
    w.startElement(name);
    w.writeText(xsdcpp::get_string(static_cast<float>(v), v.lexical()));
    w.endElement(name);
}

"""

FRA_SER_OLD = """    _serialize_forwardRateAgreementData_Index_t(w, "Index", v.Index);
    _serialize_longShort(w, "LongShort", v.LongShort);
    _serialize_float(w, "Strike", v.Strike);
    _serialize_float(w, "Notional", v.Notional);
"""
FRA_SER_NEW = """    _serialize_forwardRateAgreementData_Index_t(w, "Index", v.Index);
    _serialize_longShort(w, "LongShort", v.LongShort);
    _serialize_moneyFloat(w, "Strike", v.Strike);
    _serialize_moneyFloat(w, "Notional", v.Notional);
"""

# (old, new) edits; each `old` must occur exactly once.
_MONEY_HPP_EDITS = [(FRA_STRUCT_OLD, FRA_STRUCT_NEW)]
_MONEY_CPP_EDITS = [
    (MONEY_EXTERN_ANCHOR, MONEY_EXTERN_ANCHOR + MONEY_EXTERN),
    (MONEY_SETTER_ANCHOR, MONEY_SETTER + MONEY_SETTER_ANCHOR),
    (MONEY_INFO_ANCHOR, MONEY_INFO + MONEY_INFO_ANCHOR),
    (FRA_CHILD_OLD, FRA_CHILD_NEW),
    (FRA_CHILD2_OLD, FRA_CHILD2_NEW),
    (MONEY_SER_DECL_ANCHOR, MONEY_SER_DECL_ANCHOR + MONEY_SER_DECL),
    (MONEY_SER_DEF_ANCHOR, MONEY_SER_DEF_ANCHOR + MONEY_SER_DEF),
    (FRA_SER_OLD, FRA_SER_NEW),
]


def _apply_edits(text: str, edits: list[tuple[str, str]], name: str) -> str:
    done = [new in text for _, new in edits]
    if all(done):
        return text
    if any(done):
        raise AnchorError(
            f"{name}: the money-member patch is half-applied; restore the file and run again"
        )
    for old, new in edits:
        if text.count(old) != 1:
            raise AnchorError(
                f"{name}: the money-member anchor {old.splitlines()[0]!r} occurs "
                f"{text.count(old)} times, not once; xsdcpp's output shape changed"
            )
        text = text.replace(old, new, 1)
    return text


def _revert_edits(text: str, edits: list[tuple[str, str]]) -> str:
    for old, new in reversed(edits):
        text = text.replace(new, old, 1)
    return text


def patch_money_hpp(text: str) -> str:
    return _apply_edits(text, _MONEY_HPP_EDITS, "domain.hpp")


def unpatch_money_hpp(text: str) -> str:
    return _revert_edits(text, _MONEY_HPP_EDITS)


def patch_money_cpp(text: str) -> str:
    return _apply_edits(text, _MONEY_CPP_EDITS, "domain.cpp")


def unpatch_money_cpp(text: str) -> str:
    return _revert_edits(text, _MONEY_CPP_EDITS)


# ---------------------------------------------------------------------------
# Driver
# ---------------------------------------------------------------------------


def apply_patch(
    xsd_path: Path, cpp_path: Path, hpp_path: Path = DEFAULT_DOMAIN_HPP, verbose: bool = True
) -> dict[str, int]:
    xsd_text = xsd_path.read_text()
    cpp_text = cpp_path.read_text()
    hpp_text = hpp_path.read_text()
    new_xsd = patch_xsd(xsd_text)
    new_cpp, counts = patch_cpp(cpp_text)
    new_cpp = patch_money_cpp(new_cpp)
    new_hpp = patch_money_hpp(hpp_text)
    if new_xsd != xsd_text:
        xsd_path.write_text(new_xsd)
    if new_cpp != cpp_text:
        cpp_path.write_text(new_cpp)
    if new_hpp != hpp_text:
        hpp_path.write_text(new_hpp)
    if verbose:
        xsd_state = "patched" if new_xsd != xsd_text else "already patched"
        cpp_state = "patched" if new_cpp != cpp_text else "already patched"
        print(f"{xsd_path}: {xsd_state}", file=sys.stderr)
        print(
            f"{cpp_path}: {cpp_state} "
            f"({counts['float_setters']} float and {counts['double_setters']} double setters, "
            f"{counts['writers']} writers)",
            file=sys.stderr,
        )
    return counts


def self_test() -> int:
    with tempfile.TemporaryDirectory() as tmp:
        tmp = Path(tmp)
        xsd = tmp / "domain_xsd.hpp"
        cpp = tmp / "domain.cpp"
        hpp = tmp / "domain.hpp"
        shutil.copyfile(DEFAULT_DOMAIN_XSD, xsd)
        shutil.copyfile(DEFAULT_DOMAIN_CPP, cpp)
        shutil.copyfile(DEFAULT_DOMAIN_HPP, hpp)

        xsd.write_text(unpatch_xsd(xsd.read_text()))
        cpp.write_text(unpatch_cpp(unpatch_money_cpp(cpp.read_text())))
        hpp.write_text(unpatch_money_hpp(hpp.read_text()))
        if "moneyFloat" in cpp.read_text() or "moneyFloat" in hpp.read_text():
            print("SELF-TEST FAIL: un-patch left a money member behind", file=sys.stderr)
            return 1
        if "struct lexical_form" in xsd.read_text():
            print("SELF-TEST FAIL: un-patch left lexical_form behind", file=sys.stderr)
            return 1
        if STASH in cpp.read_text():
            print("SELF-TEST FAIL: un-patch left a lexical stash behind", file=sys.stderr)
            return 1

        counts = apply_patch(xsd, cpp, hpp, verbose=False)
        restored_xsd = xsd.read_text()
        restored_cpp = cpp.read_text()
        restored_hpp = hpp.read_text()
        if restored_xsd != DEFAULT_DOMAIN_XSD.read_text():
            print("SELF-TEST FAIL: domain_xsd.hpp was not restored byte for byte", file=sys.stderr)
            return 1
        if restored_cpp != DEFAULT_DOMAIN_CPP.read_text():
            print("SELF-TEST FAIL: domain.cpp was not restored byte for byte", file=sys.stderr)
            return 1

        if restored_hpp != DEFAULT_DOMAIN_HPP.read_text():
            print("SELF-TEST FAIL: domain.hpp was not restored byte for byte", file=sys.stderr)
            return 1

        apply_patch(xsd, cpp, hpp, verbose=False)
        if (
            xsd.read_text() != restored_xsd
            or cpp.read_text() != restored_cpp
            or hpp.read_text() != restored_hpp
        ):
            print("SELF-TEST FAIL: a second run was not idempotent", file=sys.stderr)
            return 1
        if counts["float_setters"] != 35 or counts["double_setters"] != 2:
            print(
                "SELF-TEST FAIL: expected 35 float and 2 double setters, got "
                f"{counts['float_setters']} and {counts['double_setters']}",
                file=sys.stderr,
            )
            return 1

        print(
            "SELF-TEST OK: un-patch -> reapply restored all three files byte for byte; "
            f"second run was a no-op; {counts['float_setters']} float and "
            f"{counts['double_setters']} double setter bodies stashed the lexical form"
        )
        return 0


def self_test_missing_anchor() -> int:
    with tempfile.TemporaryDirectory() as tmp:
        tmp = Path(tmp)
        xsd = tmp / "domain_xsd.hpp"
        cpp = tmp / "domain.cpp"
        hpp = tmp / "domain.hpp"
        shutil.copyfile(DEFAULT_DOMAIN_XSD, xsd)
        shutil.copyfile(DEFAULT_DOMAIN_CPP, cpp)
        shutil.copyfile(DEFAULT_DOMAIN_HPP, hpp)
        xsd.write_text(unpatch_xsd(xsd.read_text()))
        cpp.write_text(unpatch_cpp(unpatch_money_cpp(cpp.read_text())))
        hpp.write_text(unpatch_money_hpp(hpp.read_text()))

        cpp.write_text(cpp.read_text().replace(CPP_DEF_ORIGINAL, "", 1))
        try:
            apply_patch(xsd, cpp, hpp, verbose=False)
        except AnchorError as e:
            print(f"SELF-TEST (missing anchor) OK: reapply refused with: {e}")
            return 0
        print("SELF-TEST FAIL: a missing anchor did not make the reapply fail", file=sys.stderr)
        return 1


def main() -> int:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument("--domain-xsd", type=Path, default=DEFAULT_DOMAIN_XSD)
    parser.add_argument("--domain-cpp", type=Path, default=DEFAULT_DOMAIN_CPP)
    parser.add_argument("--domain-hpp", type=Path, default=DEFAULT_DOMAIN_HPP)
    parser.add_argument("--self-test", action="store_true", help="round-trip the patch on temp copies")
    parser.add_argument(
        "--self-test-missing-anchor",
        action="store_true",
        help="assert that a missing anchor makes the patch exit non-zero",
    )
    args = parser.parse_args()

    if args.self_test:
        return self_test()
    if args.self_test_missing_anchor:
        return self_test_missing_anchor()

    try:
        apply_patch(args.domain_xsd, args.domain_cpp, args.domain_hpp)
    except AnchorError as e:
        print(f"reapply_ore_lexical_form: FAILED: {e}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
