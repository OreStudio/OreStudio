"""
Tests for the publish line in `compass services status`.

No services and no cmake: the library directory is a temporary one and the
verdict is a pure function, so these cases describe what the report says
about a publish tree that holds more than one build.

Run with:  python -m pytest projects/ores.compass/tests/test_compass_publish_versions.py -v
"""

import sys
from pathlib import Path

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import compass_services


def _link(lib_dir, stem, version):
    """Create the .so.0 -> .so.0.0.<n> pair a publish tree holds."""
    (lib_dir / f"{stem}.so.0.0.{version}").write_text("")
    (lib_dir / f"{stem}.so.0").symlink_to(f"{stem}.so.0.0.{version}")


def test_one_build_reads_as_its_version(tmp_path):
    _link(tmp_path, "libores.iam.core", 29)
    _link(tmp_path, "libores.iam.service", 29)

    label, level = compass_services.publish_verdict(
        compass_services.publish_library_versions(tmp_path))

    assert level == "ok"
    assert "0.0.29" in label


def test_two_builds_name_the_libraries_left_behind(tmp_path):
    """The interrupted-link case: old service libraries, new core libraries."""
    _link(tmp_path, "libores.iam.core", 29)
    _link(tmp_path, "libores.iam.service", 28)

    label, level = compass_services.publish_verdict(
        compass_services.publish_library_versions(tmp_path))

    assert level == "critical"
    assert "MIXED" in label
    assert "libores.iam.service" in label
    assert "libores.iam.core" not in label
    assert "0.0.28" in label and "0.0.29" in label


def test_the_verdict_names_the_command_that_repairs_it(tmp_path):
    _link(tmp_path, "libores.iam.core", 29)
    _link(tmp_path, "libores.iam.service", 28)

    label, _ = compass_services.publish_verdict(
        compass_services.publish_library_versions(tmp_path))

    assert compass_services.PUBLISH_MIXED_FIX in label


def test_an_empty_publish_tree_is_reported_not_passed(tmp_path):
    label, level = compass_services.publish_verdict(
        compass_services.publish_library_versions(tmp_path))

    assert level == "critical"
    assert "no libraries" in label


def test_versions_are_compared_as_numbers_not_strings(tmp_path):
    """0.0.9 is behind 0.0.29; a string comparison would call it newer."""
    _link(tmp_path, "libores.iam.core", 29)
    _link(tmp_path, "libores.iam.service", 9)

    label, level = compass_services.publish_verdict(
        compass_services.publish_library_versions(tmp_path))

    assert level == "critical"
    assert "behind 0.0.29" in label
