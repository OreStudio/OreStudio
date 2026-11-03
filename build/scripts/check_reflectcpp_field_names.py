"""Compile-check distinct and duplicate reflection names with MSVC or GCC/Clang.

Run in the compiler's development shell, passing the installed reflectcpp
include directory. This verifies the overlay retains compile-time validation.
"""
import argparse
from pathlib import Path
import subprocess
import tempfile


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--compiler", required=True)
    parser.add_argument("--include-dir", type=Path, required=True)
    args = parser.parse_args(argv)
    names = ", ".join(f'"field_{index:02d}"' for index in range(32))
    source = (
        '#include <rfl/Field.hpp>\n'
        '#include <optional>\n'
        '#include <string>\n'
        '#include <chrono>\n'
        'template <class... Ts> consteval bool same_positions() {\n'
        '  return rfl::internal::tuple::calculate_positions<Ts...>() ==\n'
        '      (rfl::internal::tuple::Positions<0>{} + ... +\n'
        '       rfl::internal::tuple::PositionWrapper<Ts>{}).to_array();\n'
        '}\n'
        'struct alignas(64) Aligned { char value; };\n'
        'static_assert(same_positions<>());\n'
        'static_assert(same_positions<char, double, char, Aligned, int>());\n'
        'static_assert(same_positions<std::string, std::optional<bool>,\n'
        '    std::optional<std::chrono::year_month_day>, long long, char>());\n'
        f'using Names = rfl::Literal<{names}>;\n'
        'static_assert(!Names::has_duplicates());\n'
        'static_assert(rfl::internal::no_duplicate_field_names<rfl::Tuple<>>());\n'
        'static_assert(rfl::internal::no_duplicate_field_names<rfl::Tuple<\n'
        '    rfl::Field<"alpha", int>, rfl::Field<"beta", int>>>());\n'
        '#ifdef TEST_DUPLICATES\n'
        'rfl::Literal<"same", "same"> duplicates;\n'
        '#endif\n'
    )
    msvc = Path(args.compiler).name.lower() in ("cl", "cl.exe", "clang-cl", "clang-cl.exe")
    with tempfile.TemporaryDirectory(prefix="ores-field-names-") as directory:
        folder = Path(directory)
        cpp = folder / "field_names.cpp"
        cpp.write_text(source, encoding="utf-8", newline="\n")
        include = str(args.include_dir.resolve())
        if msvc:
            command = [args.compiler, "/nologo", "/c", "/std:c++latest", "/EHsc", "/utf-8",
                       f"/I{include}", f"/Fo{folder / 'fields.obj'}", str(cpp)]
            define = "/DTEST_DUPLICATES"
        else:
            command = [args.compiler, "-std=c++23", "-c", f"-I{include}",
                       "-o", str(folder / "fields.o"), str(cpp)]
            define = "-DTEST_DUPLICATES"
        valid = subprocess.run(command, capture_output=True, text=True)
        if valid.returncode:
            raise RuntimeError("Distinct names failed to compile:\n" + valid.stdout + valid.stderr)
        invalid = subprocess.run(command + [define], capture_output=True, text=True)
        if (invalid.returncode == 0 or "Duplicate field names are not allowed" not in
                invalid.stdout + invalid.stderr):
            raise RuntimeError("Duplicate validation failed:\n" + invalid.stdout + invalid.stderr)
    print("Distinct names compile; duplicate names are rejected at compile time.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
