"""Tests for the prettier step the TypeScript render runs.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_prettier_format.py

Codegen is a two-step process: render the template, then normalise the output.
The C++ half is clang-format; the TypeScript half is prettier, and it was
missing, which is why the tree carried 306 generated files that no formatter
owned and a format check that could never pass.

These tests assert the rule rather than the template: the step formats the
TypeScript it is given with the repository's configuration, covers the React
extension, leaves every other extension alone, and does nothing when prettier
is absent. The step is a no-op without prettier, so the tests that need it skip
rather than fail on a machine that has none.
"""
import subprocess
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen import generate  # noqa: E402

CODEGEN_DIR = REPO_ROOT / "projects" / "ores.codegen"

# Two-space indentation and double quotes: what a template emits when nobody
# normalises it, and what the step has to turn into the house style.
UNFORMATTED = 'export const subjects = {\n  list: "iam.v1.accounts.list",\n};\n'
FORMATTED = "export const subjects = {\n    list: 'iam.v1.accounts.list',\n};\n"

UNFORMATTED_TSX = (
    "export const Panel = () => (\n"
    "  <div>\n"
    "      <span>hi</span>\n"
    "  </div>\n"
    ");\n"
)
FORMATTED_TSX = (
    "export const Panel = () => (\n"
    "    <div>\n"
    "        <span>hi</span>\n"
    "    </div>\n"
    ");\n"
)


@pytest.fixture(autouse=True)
def _require_prettier() -> None:
    if generate._prettier_exe(REPO_ROOT) is None:
        pytest.skip("prettier is not installed")


def test_formats_type_script_with_the_house_style(tmp_path: Path) -> None:
    target = tmp_path / "subjects.ts"
    target.write_text(UNFORMATTED, encoding="utf-8")

    generate.prettier_format_files([target], CODEGEN_DIR)

    assert target.read_text(encoding="utf-8") == FORMATTED


def test_covers_the_react_extension(tmp_path: Path) -> None:
    target = tmp_path / "Panel.tsx"
    target.write_text(UNFORMATTED_TSX, encoding="utf-8")

    generate.prettier_format_files([target], CODEGEN_DIR)

    assert target.read_text(encoding="utf-8") == FORMATTED_TSX


def test_leaves_every_other_extension_alone(tmp_path: Path) -> None:
    target = tmp_path / "handler.cpp"
    body = "int  main( ){return 0;}\n"
    target.write_text(body, encoding="utf-8")

    generate.prettier_format_files([target], CODEGEN_DIR)

    assert target.read_text(encoding="utf-8") == body


def test_is_a_no_op_when_prettier_is_absent(tmp_path: Path, monkeypatch) -> None:
    monkeypatch.setattr(generate, "_prettier_exe", lambda _root: None)
    target = tmp_path / "subjects.ts"
    target.write_text(UNFORMATTED, encoding="utf-8")

    generate.prettier_format_files([target], CODEGEN_DIR)

    assert target.read_text(encoding="utf-8") == UNFORMATTED


def test_the_running_prettier_is_the_declared_one() -> None:
    """The drift gate compares bytes, so the version has to be the workspace's.

    A newer prettier formats differently, and every generated TypeScript file
    then reads as drifted. This is the assertion the workflow's install step
    rests on.
    """
    exe = generate._prettier_exe(REPO_ROOT)
    running = subprocess.run(
        [exe, "--version"], check=True, capture_output=True, text=True
    ).stdout.strip()

    assert running == generate._declared_prettier_version(REPO_ROOT)
