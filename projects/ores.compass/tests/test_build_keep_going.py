"""
Tests for `compass build --keep-going`.

Run with:  python -m pytest projects/ores.compass/tests/test_build_keep_going.py -v
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from compass import keep_going_args  # noqa: E402


def _cache(tmp_path, generator):
    (tmp_path / "CMakeCache.txt").write_text(
        f"CMAKE_BUILD_TYPE:STRING=Release\nCMAKE_GENERATOR:INTERNAL={generator}\n")
    return tmp_path


def test_ninja_keeps_going_past_any_number_of_failures(tmp_path):
    assert keep_going_args(_cache(tmp_path, "Ninja"), "x") == ["--", "-k", "0"]


def test_make_takes_a_bare_flag(tmp_path):
    """`make -k 0` would read 0 as a target to build."""
    assert keep_going_args(_cache(tmp_path, "Unix Makefiles"), "x-ninja") == ["--", "-k"]


def test_the_cache_wins_over_the_preset_name(tmp_path):
    assert keep_going_args(_cache(tmp_path, "Ninja"), "linux-gcc-make") == ["--", "-k", "0"]


def test_before_the_first_configure_the_preset_name_decides(tmp_path):
    assert keep_going_args(tmp_path, "linux-clang-release-ninja") == ["--", "-k", "0"]
    assert keep_going_args(tmp_path, "linux-clang-release-make") == ["--", "-k"]


def test_every_ninja_generator_takes_a_failure_count(tmp_path):
    assert keep_going_args(_cache(tmp_path, "Ninja Multi-Config"), "x") == ["--", "-k", "0"]


def test_an_unknown_generator_gets_no_option(tmp_path):
    """A guessed option could fail a build that would otherwise run."""
    assert keep_going_args(_cache(tmp_path, "Xcode"), "x") == []


def test_an_unknown_preset_name_before_configure_means_make(tmp_path):
    assert keep_going_args(tmp_path, "linux-gcc-debug") == ["--", "-k"]
    assert keep_going_args(tmp_path, "Linux-Clang-Ninja") == ["--", "-k", "0"]


def test_ninja_takes_the_failure_count_given(tmp_path):
    assert keep_going_args(_cache(tmp_path, "Ninja"), "x", 3) == ["--", "-k", "3"]


def test_make_ignores_the_failure_count(tmp_path):
    assert keep_going_args(_cache(tmp_path, "Unix Makefiles"), "x", 3) == ["--", "-k"]
