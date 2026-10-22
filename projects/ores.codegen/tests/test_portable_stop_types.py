"""Tests for check_portable_stop_types.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_portable_stop_types.py

The check fails when code outside ``ores.platform`` names a C++20 stop type,
because Apple's libc++ has none of them and the macOS build, which runs at
night, is the only place that notices. Most tests drive the check against a
throw-away tree. The last runs it against the real tree, which is what stops
the check from passing every tree by finding nothing.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/scripts"))

import check_portable_stop_types as check  # noqa: E402


def _write(root: Path, name: str, body: str) -> None:
    path = root / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(body, encoding="utf-8")


def _findings(monkeypatch, root: Path) -> list[str]:
    monkeypatch.setattr(check, "REPO_ROOT", root)
    monkeypatch.setattr(check, "PROJECTS_DIR", root / "projects")
    return check.check()


def test_the_retry_thread_that_broke_the_macos_build_fails(tmp_path, monkeypatch):
    _write(tmp_path, "projects/ores.synthetic/service/src/app/application.cpp", """\
    std::jthread restore_retries;
    restore_retries = std::jthread([](std::stop_token stop) { (void)stop; });
""")

    findings = _findings(monkeypatch, tmp_path)

    assert len(findings) == 2
    assert "application.cpp:1: std::jthread" in findings[0]
    assert "application.cpp:2: std::jthread" in findings[1]


def test_each_standard_stop_type_fails(tmp_path, monkeypatch):
    for name in ("stop_token", "stop_source", "stop_callback"):
        _write(tmp_path, f"projects/ores.widget/src/{name}.cpp", f"std::{name} s;\n")

    assert len(_findings(monkeypatch, tmp_path)) == 3


def test_the_stop_token_include_fails(tmp_path, monkeypatch):
    _write(tmp_path, "projects/ores.widget/src/a.cpp", "#include <stop_token>\n")

    findings = _findings(monkeypatch, tmp_path)

    assert len(findings) == 1
    assert "<stop_token>" in findings[0]


def test_ores_platform_may_name_the_standard_types(tmp_path, monkeypatch):
    _write(tmp_path, "projects/ores.platform/include/x.hpp", "std::jthread t;\n")
    _write(tmp_path, "projects/ores.platform/tests/x_tests.cpp", "std::stop_token t;\n")

    assert _findings(monkeypatch, tmp_path) == []


def test_a_comment_or_a_string_that_names_a_type_is_not_a_finding(tmp_path, monkeypatch):
    _write(tmp_path, "projects/ores.widget/src/a.cpp", """\
// Apple's libc++ has no std::jthread.
/* nor std::stop_token,
   nor std::stop_source */
const char* why = "no std::stop_callback here";
const char* raw = R"(std::jthread)";
""")

    assert _findings(monkeypatch, tmp_path) == []


def test_the_portable_types_are_not_findings(tmp_path, monkeypatch):
    _write(tmp_path, "projects/ores.widget/src/a.cpp", """\
ores::platform::concurrency::stoppable_thread worker;
const ores::platform::concurrency::stop_source& stop;
""")

    assert _findings(monkeypatch, tmp_path) == []


def test_a_vendored_tree_is_not_read(tmp_path, monkeypatch):
    _write(tmp_path, "projects/ores.web/node_modules/pkg/a.cpp", "std::jthread t;\n")

    assert _findings(monkeypatch, tmp_path) == []


def test_the_self_test_passes():
    assert check.self_test() == 0


def test_the_real_tree_passes():
    assert check.check() == []
