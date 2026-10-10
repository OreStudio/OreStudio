#!/usr/bin/env python3
"""Reapply the ORE lexical-form hand-patch lost on xsdcpp regen.

xsdcpp emits xsd::base<T> as a bare T plus the parse helpers set_float and
set_double, which throw away the document's own digits. A money leaf must
survive the ORE boundary as the exact decimal the document wrote, so this
project keeps the digits beside the parsed float. That change spans two
generated files:

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
    return text, {"float_setters": n_float, "double_setters": n_double}


def unpatch_cpp(text: str) -> str:
    if STASH not in text:
        raise AnchorError("domain.cpp: no lexical stashes found to un-patch")
    return re.sub(r"[ \t]*" + re.escape(STASH) + r"\n", "", text)


# ---------------------------------------------------------------------------
# Driver
# ---------------------------------------------------------------------------


def apply_patch(xsd_path: Path, cpp_path: Path, verbose: bool = True) -> dict[str, int]:
    xsd_text = xsd_path.read_text()
    cpp_text = cpp_path.read_text()
    new_xsd = patch_xsd(xsd_text)
    new_cpp, counts = patch_cpp(cpp_text)
    if new_xsd != xsd_text:
        xsd_path.write_text(new_xsd)
    if new_cpp != cpp_text:
        cpp_path.write_text(new_cpp)
    if verbose:
        xsd_state = "patched" if new_xsd != xsd_text else "already patched"
        cpp_state = "patched" if new_cpp != cpp_text else "already patched"
        print(f"{xsd_path}: {xsd_state}", file=sys.stderr)
        print(
            f"{cpp_path}: {cpp_state} "
            f"({counts['float_setters']} float and {counts['double_setters']} double setters)",
            file=sys.stderr,
        )
    return counts


def self_test() -> int:
    with tempfile.TemporaryDirectory() as tmp:
        tmp = Path(tmp)
        xsd = tmp / "domain_xsd.hpp"
        cpp = tmp / "domain.cpp"
        shutil.copyfile(DEFAULT_DOMAIN_XSD, xsd)
        shutil.copyfile(DEFAULT_DOMAIN_CPP, cpp)

        xsd.write_text(unpatch_xsd(xsd.read_text()))
        cpp.write_text(unpatch_cpp(cpp.read_text()))
        if "struct lexical_form" in xsd.read_text():
            print("SELF-TEST FAIL: un-patch left lexical_form behind", file=sys.stderr)
            return 1
        if STASH in cpp.read_text():
            print("SELF-TEST FAIL: un-patch left a lexical stash behind", file=sys.stderr)
            return 1

        counts = apply_patch(xsd, cpp, verbose=False)
        restored_xsd = xsd.read_text()
        restored_cpp = cpp.read_text()
        if restored_xsd != DEFAULT_DOMAIN_XSD.read_text():
            print("SELF-TEST FAIL: domain_xsd.hpp was not restored byte for byte", file=sys.stderr)
            return 1
        if restored_cpp != DEFAULT_DOMAIN_CPP.read_text():
            print("SELF-TEST FAIL: domain.cpp was not restored byte for byte", file=sys.stderr)
            return 1

        apply_patch(xsd, cpp, verbose=False)
        if xsd.read_text() != restored_xsd or cpp.read_text() != restored_cpp:
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
            "SELF-TEST OK: un-patch -> reapply restored both files byte for byte; "
            f"second run was a no-op; {counts['float_setters']} float and "
            f"{counts['double_setters']} double setter bodies stashed the lexical form"
        )
        return 0


def self_test_missing_anchor() -> int:
    with tempfile.TemporaryDirectory() as tmp:
        tmp = Path(tmp)
        xsd = tmp / "domain_xsd.hpp"
        cpp = tmp / "domain.cpp"
        shutil.copyfile(DEFAULT_DOMAIN_XSD, xsd)
        shutil.copyfile(DEFAULT_DOMAIN_CPP, cpp)
        xsd.write_text(unpatch_xsd(xsd.read_text()))
        cpp.write_text(unpatch_cpp(cpp.read_text()))

        cpp.write_text(cpp.read_text().replace(CPP_DEF_ORIGINAL, "", 1))
        try:
            apply_patch(xsd, cpp, verbose=False)
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
        apply_patch(args.domain_xsd, args.domain_cpp)
    except AnchorError as e:
        print(f"reapply_ore_lexical_form: FAILED: {e}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
