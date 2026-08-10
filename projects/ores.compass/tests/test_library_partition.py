"""Tests for library_partition — ldd intersection computation and staging."""

import os
import stat
import subprocess
import sys
import tempfile
from pathlib import Path
from unittest import mock

import pytest

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from library_partition import (
    _is_system_lib,
    _ldd_libs,
    _copy_file,
    compute_partition,
    stage_for_build,
)


# ── _is_system_lib ──────────────────────────────────────────────────────

@pytest.mark.parametrize("soname,expected", [
    ("linux-vdso.so.1", True),
    ("linux-vdso.so.2", True),
    ("ld-linux-x86-64.so.2", True),
    ("libores.service.so.0", False),
    ("libc.so.6", False),          # filtered by path, not soname pattern
    ("libstdc++.so.6", False),
    ("", False),
])
def test_is_system_lib(soname, expected):
    assert _is_system_lib(soname) == expected


# ── _ldd_libs ───────────────────────────────────────────────────────────

LDD_ORES_IAM = """\
	linux-vdso.so.1 (0x7fff00000000)
	libores.service.so.0 => /tmp/stage/lib/libores.service.so.0 (0x7f0000000000)
	libores.logging.so.0 => /tmp/stage/lib/libores.logging.so.0 (0x7f0000000000)
	libores.utility.so.0 => /tmp/stage/lib/libores.utility.so.0 (0x7f0000000000)
	libores.nats.so.0 => /tmp/stage/lib/libores.nats.so.0 (0x7f0000000000)
	libm.so.6 => /usr/lib/x86_64-linux-gnu/libm.so.6 (0x7f0000000000)
	libc.so.6 => /usr/lib/x86_64-linux-gnu/libc.so.6 (0x7f0000000000)
	ld-linux-x86-64.so.2 => /usr/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2 (0x7f0000000000)
"""

LDD_STATIC = """\
	not a dynamic executable
"""

LDD_ONLY_SYSTEM = """\
	linux-vdso.so.1 (0x7fff00000000)
	libc.so.6 => /usr/lib/x86_64-linux-gnu/libc.so.6 (0x7f0000000000)
	ld-linux-x86-64.so.2 => /usr/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2 (0x7f0000000000)
"""

LDD_SINGLE = """\
	linux-vdso.so.1 (0x7fff00000000)
	libores.database.so.0 => /tmp/stage/lib/libores.database.so.0 (0x7f0000000000)
	ld-linux-x86-64.so.2 => /usr/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2 (0x7f0000000000)
"""

LDD_UNRESOLVED = """\
	linux-vdso.so.1 (0x7fff00000000)
	libmissing.so.0 => not found
	libores.service.so.0 => /tmp/stage/lib/libores.service.so.0 (0x7f0000000000)
	ld-linux-x86-64.so.2 => /usr/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2 (0x7f0000000000)
"""

LDD_NO_LIBS_PATH = """\
	libores.foo.so.0 => /other/path/lib/libores.foo.so.0 (0x7f0000000000)
"""

# Sibling of lib_dir sharing its path as a string prefix — the old
# str(lib_dir) in str(resolved) check misclassified this as inside lib_dir.
LDD_SIBLING_PREFIX = """\
	libores.foo.so.0 => /tmp/stage/lib-old/libores.foo.so.0 (0x7f0000000000)
"""


class TestLddLibs:
    def test_extracts_ores_libs_only(self, monkeypatch):
        """Only libs under lib_dir are returned; system libs excluded."""
        def fake_run(cmd, capture_output, text, env):
            return subprocess.CompletedProcess(
                args=cmd, returncode=0, stdout=LDD_ORES_IAM, stderr="")

        monkeypatch.setattr(subprocess, "run", fake_run)
        libs = _ldd_libs(Path("/tmp/stage/bin/ores.iam.service"),
                         Path("/tmp/stage/lib"))
        assert libs == {
            "libores.service.so.0",
            "libores.logging.so.0",
            "libores.utility.so.0",
            "libores.nats.so.0",
        }

    def test_returns_empty_for_static_binary(self, monkeypatch):
        def fake_run(cmd, capture_output, text, env):
            return subprocess.CompletedProcess(
                args=cmd, returncode=0, stdout=LDD_STATIC, stderr="")

        monkeypatch.setattr(subprocess, "run", fake_run)
        libs = _ldd_libs(Path("/tmp/stage/bin/static_bin"),
                         Path("/tmp/stage/lib"))
        assert libs == set()

    def test_returns_empty_when_only_system_libs(self, monkeypatch):
        def fake_run(cmd, capture_output, text, env):
            return subprocess.CompletedProcess(
                args=cmd, returncode=0, stdout=LDD_ONLY_SYSTEM, stderr="")

        monkeypatch.setattr(subprocess, "run", fake_run)
        libs = _ldd_libs(Path("/tmp/stage/bin/svc"), Path("/tmp/stage/lib"))
        assert libs == set()

    def test_excludes_libs_outside_lib_dir(self, monkeypatch):
        """Libs resolved to paths not under lib_dir are system libs."""
        def fake_run(cmd, capture_output, text, env):
            return subprocess.CompletedProcess(
                args=cmd, returncode=0, stdout=LDD_NO_LIBS_PATH, stderr="")

        monkeypatch.setattr(subprocess, "run", fake_run)
        libs = _ldd_libs(Path("/tmp/stage/bin/svc"), Path("/tmp/stage/lib"))
        assert libs == set()

    def test_excludes_libs_in_sibling_dir_sharing_path_prefix(self,
                                                             monkeypatch):
        """A sibling dir sharing lib_dir's path as a string prefix
        (e.g. .../lib-old) is outside lib_dir and must not match."""
        def fake_run(cmd, capture_output, text, env):
            return subprocess.CompletedProcess(
                args=cmd, returncode=0, stdout=LDD_SIBLING_PREFIX, stderr="")

        monkeypatch.setattr(subprocess, "run", fake_run)
        libs = _ldd_libs(Path("/tmp/stage/bin/svc"), Path("/tmp/stage/lib"))
        assert libs == set()

    def test_skips_unresolved_libs(self, monkeypatch):
        def fake_run(cmd, capture_output, text, env):
            return subprocess.CompletedProcess(
                args=cmd, returncode=0, stdout=LDD_UNRESOLVED, stderr="")

        monkeypatch.setattr(subprocess, "run", fake_run)
        libs = _ldd_libs(Path("/tmp/stage/bin/svc"), Path("/tmp/stage/lib"))
        assert libs == {"libores.service.so.0"}

    def test_raises_on_ldd_failure(self, monkeypatch):
        def fake_run(cmd, capture_output, text, env):
            return subprocess.CompletedProcess(
                args=cmd, returncode=1, stdout="", stderr="cannot read")

        monkeypatch.setattr(subprocess, "run", fake_run)
        with pytest.raises(RuntimeError, match="ldd failed"):
            _ldd_libs(Path("/tmp/stage/bin/svc"), Path("/tmp/stage/lib"))

    def test_sets_ld_library_path(self, monkeypatch):
        """Verify LD_LIBRARY_PATH is passed to ldd."""
        captured_env = {}

        def fake_run(cmd, capture_output, text, env):
            captured_env["LD_LIBRARY_PATH"] = env.get("LD_LIBRARY_PATH", "")
            return subprocess.CompletedProcess(
                args=cmd, returncode=0, stdout=LDD_SINGLE, stderr="")

        monkeypatch.setattr(subprocess, "run", fake_run)
        _ldd_libs(Path("/tmp/stage/bin/svc"), Path("/my/custom/lib"))
        assert captured_env["LD_LIBRARY_PATH"] == "/my/custom/lib"


# ── compute_partition ───────────────────────────────────────────────────

class TestComputePartition:
    """Tests using mocked _ldd_libs — no real binaries needed."""

    def test_computes_intersection(self, monkeypatch):
        svc_libs = {
            "ores.iam.service":    {"libores.service", "libores.logging", "libores.utility"},
            "ores.refdata.service": {"libores.service", "libores.logging"},
            "ores.dq.service":     {"libores.service", "libores.nats"},
        }
        def fake_ldd(binary, lib_dir):
            return svc_libs.get(binary.name, set())
        monkeypatch.setattr("library_partition._ldd_libs", fake_ldd)
        monkeypatch.setattr(Path, "is_file", lambda self: True)

        common, extras = compute_partition(
            Path("/fake/bin"), Path("/fake/lib"),
            ["ores.iam.service", "ores.refdata.service", "ores.dq.service"],
        )
        assert common == {"libores.service"}
        assert extras["ores.iam.service"] == {"libores.logging", "libores.utility"}
        assert extras["ores.refdata.service"] == {"libores.logging"}
        assert extras["ores.dq.service"] == {"libores.nats"}

    def test_all_common_no_extras(self, monkeypatch):
        def fake_ldd(binary, lib_dir):
            return {"libores.service", "libores.logging"}
        monkeypatch.setattr("library_partition._ldd_libs", fake_ldd)
        monkeypatch.setattr(Path, "is_file", lambda self: True)

        common, extras = compute_partition(
            Path("/fake/bin"), Path("/fake/lib"),
            ["a", "b", "c"],
        )
        assert common == {"libores.service", "libores.logging"}
        assert extras == {}  # no extras when all identical

    def test_single_service(self, monkeypatch):
        def fake_ldd(binary, lib_dir):
            return {"libores.service", "libores.logging"}
        monkeypatch.setattr("library_partition._ldd_libs", fake_ldd)
        monkeypatch.setattr(Path, "is_file", lambda self: True)

        common, extras = compute_partition(
            Path("/fake/bin"), Path("/fake/lib"), ["only_svc"],
        )
        # With one service, intersection is all its libs.
        assert common == {"libores.service", "libores.logging"}
        assert extras == {}

    def test_binary_not_found_raises(self, monkeypatch):
        with pytest.raises(FileNotFoundError, match="not found"):
            compute_partition(
                Path("/nonexistent/bin"), Path("/nonexistent/lib"),
                ["missing.service"],
            )

    def test_static_service_preserves_intersection(self, monkeypatch):
        """A statically-linked service contributes empty set but should
        not zero out the intersection for other services."""
        def fake_ldd(binary, lib_dir):
            if binary.name == "static_svc":
                return set()  # statically linked
            return {"libores.service", "libores.logging"}
        monkeypatch.setattr("library_partition._ldd_libs", fake_ldd)
        monkeypatch.setattr(Path, "is_file", lambda self: True)

        common, extras = compute_partition(
            Path("/fake/bin"), Path("/fake/lib"),
            ["ores.iam.service", "static_svc", "ores.refdata.service"],
        )
        # The static binary is skipped entirely; intersection of the two
        # real services is the full set they share.
        assert common == {"libores.service", "libores.logging"}


# ── stage_for_build ─────────────────────────────────────────────────────

class TestStageForBuild:
    @pytest.fixture(autouse=True)
    def setup(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.stage = Path(self.tmp.name)
        # Create source bin/ and lib/ with fake content.
        self.bin_dir = self.stage / "bin"
        self.bin_dir.mkdir()
        self.lib_dir = self.stage / "lib"
        self.lib_dir.mkdir()
        yield
        self.tmp.cleanup()

    def _make_binary(self, name: str) -> None:
        p = self.bin_dir / name
        p.write_bytes(b"#!/bin/fake\n")
        p.chmod(0o755)

    def _make_lib(self, soname: str, real_name: str | None = None) -> Path:
        """Create a SONAME symlink and its real file. Returns real Path."""
        real = self.lib_dir / (real_name or soname)
        real.write_bytes(b"ELF...fake\n")
        real.chmod(0o755)
        sym = self.lib_dir / soname
        if soname != real.name:
            sym.symlink_to(real.name)
        return real

    def test_creates_base_and_service_dirs(self):
        self._make_binary("ores.iam.service")
        self._make_lib("libores.service.so.0", "libores.service.so.0.1.0")
        self._make_lib("libores.logging.so.0", "libores.logging.so.0.1.0")

        common = {"libores.service.so.0"}
        extras = {"ores.iam.service": {"libores.logging.so.0"}}

        stage_for_build(self.stage, self.bin_dir, self.lib_dir,
                        ["ores.iam.service"], common, extras)

        # Base
        assert (self.stage / "base/lib/libores.service.so.0").is_symlink()
        assert (self.stage / "base/lib/libores.service.so.0.1.0").is_file()
        assert (self.stage / "base/log").is_dir()
        assert (self.stage / "base/run").is_dir()
        assert (self.stage / "base/storage").is_dir()
        # Permissions
        for d in ("log", "run", "storage"):
            mode = (self.stage / "base" / d).stat().st_mode
            assert stat.S_IMODE(mode) == 0o777

        # Per-service
        svc = self.stage / "services/ores.iam.service"
        assert (svc / "bin/ores.iam.service").is_file()
        assert (svc / "lib/libores.logging.so.0").is_symlink()
        assert (svc / "lib/libores.logging.so.0.1.0").is_file()

    def test_service_with_no_extras(self):
        self._make_binary("ores.iam.service")
        self._make_lib("libores.service.so.0", "libores.service.so.0.1.0")

        common = {"libores.service.so.0"}
        extras = {}  # no extras

        stage_for_build(self.stage, self.bin_dir, self.lib_dir,
                        ["ores.iam.service"], common, extras)

        svc = self.stage / "services/ores.iam.service"
        assert (svc / "bin/ores.iam.service").is_file()
        assert not (svc / "lib").exists()  # no lib dir needed

    def test_preserves_executable_bit_on_binary(self):
        self._make_binary("ores.iam.service")
        self._make_lib("libores.service.so.0", "libores.service.so.0.1.0")

        common = {"libores.service.so.0"}
        extras = {}

        stage_for_build(self.stage, self.bin_dir, self.lib_dir,
                        ["ores.iam.service"], common, extras)

        st = (self.stage / "services/ores.iam.service/bin/ores.iam.service").stat()
        assert st.st_mode & stat.S_IXUSR

    def test_real_lib_not_a_symlink(self):
        """When the SONAME IS the real file (no symlink indirection),
        it is copied directly without trying to readlink it."""
        self._make_binary("ores.iam.service")
        # Create a lib that is the real file (not a symlink).
        real = self.lib_dir / "libores.foo.so.0"
        real.write_bytes(b"ELF...\n")
        real.chmod(0o755)

        stage_for_build(self.stage, self.bin_dir, self.lib_dir,
                        ["ores.iam.service"],
                        {"libores.foo.so.0"}, {})

        dst = self.stage / "base/lib/libores.foo.so.0"
        assert dst.is_file()
        assert not dst.is_symlink()

    def test_multiple_services(self):
        for svc in ["ores.iam.service", "ores.refdata.service", "ores.dq.service"]:
            self._make_binary(svc)
        for lib in ["libores.service.so.0", "libores.logging.so.0", "libores.nats.so.0"]:
            self._make_lib(lib, lib + ".1.0")

        common = {"libores.service.so.0", "libores.logging.so.0"}
        extras = {"ores.dq.service": {"libores.nats.so.0"}}

        stage_for_build(self.stage, self.bin_dir, self.lib_dir,
                        ["ores.iam.service", "ores.refdata.service", "ores.dq.service"],
                        common, extras)

        # All three service dirs exist
        for svc in ["ores.iam.service", "ores.refdata.service", "ores.dq.service"]:
            assert (self.stage / "services" / svc / "bin" / svc).is_file()

        # Only dq has extra libs
        assert not (self.stage / "services/ores.iam.service/lib").exists()
        assert not (self.stage / "services/ores.refdata.service/lib").exists()
        assert (self.stage / "services/ores.dq.service/lib/libores.nats.so.0").is_symlink()


# ── _copy_file ──────────────────────────────────────────────────────────

class TestCopyFile:
    @pytest.fixture(autouse=True)
    def setup(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.src_dir = Path(self.tmp.name) / "src"
        self.src_dir.mkdir()
        self.dst_dir = Path(self.tmp.name) / "dst"
        yield
        self.tmp.cleanup()

    def test_copies_content(self):
        src = self.src_dir / "a.txt"
        src.write_text("hello")
        dst = self.dst_dir / "sub" / "a.txt"
        _copy_file(src, dst)
        assert dst.read_text() == "hello"

    def test_preserves_executable(self):
        src = self.src_dir / "runme"
        src.write_bytes(b"#!/bin/sh\necho ok\n")
        src.chmod(0o755)
        dst = self.dst_dir / "runme"
        _copy_file(src, dst)
        assert dst.stat().st_mode & stat.S_IXUSR


# ── Integration test with real binaries ─────────────────────────────────

@pytest.mark.integration
class TestRealBinaries:
    """Smoke test using the actual checkout's staged binaries (requires
    a prior `compass build` + `docker/stage-runtime.sh` run)."""

    @pytest.fixture(autouse=True)
    def setup(self):
        self.root = Path(__file__).resolve().parent.parent.parent.parent
        self.stage = self.root / "build" / "docker-stage"
        if not (self.stage / "bin").is_dir():
            pytest.skip("build/docker-stage not staged — run stage-runtime.sh first")

    def test_computes_partition_with_real_binaries(self):
        services = [
            "ores.iam.service",
            "ores.refdata.service",
            "ores.dq.service",
            "ores.workspace.service",
        ]
        # Only test services whose binaries exist.
        present = [s for s in services if (self.stage / "bin" / s).is_file()]
        if len(present) < 2:
            pytest.skip("need at least 2 staged service binaries")

        common, extras = compute_partition(
            self.stage / "bin", self.stage / "lib", present,
        )
        # Common should be non-empty — all services share at least
        # libores.service.so.0 and a few others.
        assert len(common) >= 1, f"Expected common libs, got none for {present}"
        # Every common lib should be a valid SONAME format.
        for lib in common:
            assert lib.startswith("libores."), f"Unexpected non-ores lib: {lib}"
