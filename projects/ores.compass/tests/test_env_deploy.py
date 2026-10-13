"""
Tests for env_deploy.py (compass env deploy).

Run with:  python -m pytest projects/ores.compass/tests/test_env_deploy.py -v
No live database, ssh, or podman access required — subprocess boundaries
are monkeypatched; the pure functions (profile generation, env parsing,
compute env writing) run against tmp_path fixtures.
"""

import subprocess
import sys
from pathlib import Path

import pytest

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import env_deploy


def _write(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")
    return path


# --- _read_env ------------------------------------------------------------

class TestReadEnv:
    def test_plain_values(self, tmp_path):
        p = _write(tmp_path / "e.env", "A=1\nB=hello\n")
        assert env_deploy._read_env(p) == {"A": "1", "B": "hello"}

    def test_quotes_stripped(self, tmp_path):
        # docker/.env uses KEY="value" for values with escaped \n (the
        # JWT PEM); podman's --env-file passes the quotes through unless
        # stripped, so reading must strip exactly one pair.
        p = _write(tmp_path / "e.env", 'JWT="line1\\nline2"\nPLAIN=abc\n')
        assert env_deploy._read_env(p) == {"JWT": "line1\\nline2",
                                           "PLAIN": "abc"}

    def test_comments_and_blank_lines_skipped(self, tmp_path):
        p = _write(tmp_path / "e.env",
                   "# comment\n\nA=1\n   # indented comment\nB=2\n")
        assert env_deploy._read_env(p) == {"A": "1", "B": "2"}

    def test_missing_file_returns_empty(self, tmp_path):
        assert env_deploy._read_env(tmp_path / "nope.env") == {}

    def test_malformed_lines_skipped(self, tmp_path):
        p = _write(tmp_path / "e.env", "NO_EQUALS_HERE\n=4\nA=1\n")
        assert env_deploy._read_env(p) == {"A": "1"}


# --- _strip_quotes --------------------------------------------------------

class TestStripQuotes:
    def test_strips_matching_quotes(self):
        assert env_deploy._strip_quotes('A="v1"') == "A=v1"

    def test_leaves_unquoted(self):
        assert env_deploy._strip_quotes("A=v1") == "A=v1"

    def test_leaves_comment_lines(self):
        assert env_deploy._strip_quotes("# A=\"v1\"") == "# A=\"v1\""

    def test_adds_quotes_to_unquoted_value_with_whitespace(self):
        # The JWT PEM in docker/.env is unquoted but its header/footer
        # (-----BEGIN PRIVATE KEY-----) has spaces. Bash sourcing would
        # interpret those spaces as command separators, so _strip_quotes
        # must wrap the value in quotes for the profile.
        result = env_deploy._strip_quotes(
            'ORES_IAM_SERVICE_JWT_PRIVATE_KEY=-----BEGIN PRIVATE KEY-----\\nk1\\n-----END PRIVATE KEY-----\\n')
        assert result == (
            'ORES_IAM_SERVICE_JWT_PRIVATE_KEY='
            '"-----BEGIN PRIVATE KEY-----\\nk1\\n-----END PRIVATE KEY-----\\n"')

    def test_leaves_unquoted_without_whitespace(self):
        assert env_deploy._strip_quotes("A=v1") == "A=v1"

    def test_keeps_quotes_on_quoted_value_with_whitespace(self):
        result = env_deploy._strip_quotes(
            'JWT="-----BEGIN KEY-----\\nk1\\n-----END KEY-----\\n"')
        assert result == 'JWT="-----BEGIN KEY-----\\nk1\\n-----END KEY-----\\n"'


# --- _generate_profile ----------------------------------------------------

class TestGenerateProfile:
    def test_rewrites_db_port_and_paths(self, tmp_path):
        root = tmp_path / "checkout"
        src = _write(root / "docker" / ".env", (
            "ORES_CHECKOUT_LABEL=brave_hopper\n"
            "ORES_DB_PORT=5432\n"
            "ORES_NATS_SERVICE_DB_PORT=5432\n"
            "ORES_TEST_DB_PORT=5432\n"
            f"ORES_NATS_TLS_CA={root}/build/keys/nats/ca.crt\n"
            f"ORES_NATS_STORE_DIR={root}/build/nats/jetstream\n"
        ))
        profile = root / ".env.newton"
        env_deploy._generate_profile(root, profile, src, "newton",
                                     "~/ores-deploy", "5433")
        text = profile.read_text(encoding="utf-8")
        assert "ORES_DB_PORT=5433" in text
        assert "ORES_NATS_SERVICE_DB_PORT=5433" in text
        assert "ORES_TEST_DB_PORT=5433" in text
        # Absolute checkout paths become the remote root (which itself
        # stays ~-prefixed; deploy pins the expansion later).
        assert "ORES_NATS_TLS_CA=~/ores-deploy/build/keys/nats/ca.crt" in text
        assert "ORES_NATS_STORE_DIR=~/ores-deploy/build/nats/jetstream" in text
        # Remote configuration block appended.
        assert "ORES_REMOTE_HOST=newton" in text
        assert "ORES_REMOTE_ROOT=~/ores-deploy" in text
        assert "ORES_REMOTE_DB_PORT=5433" in text

    def test_pgport_emitted_when_remote_db_off_default(self, tmp_path):
        # libpq (psql / compass db) reads PGPORT natively; the local .env
        # omits it when the DB is on 5432, so the profile must add it for
        # a remote Postgres on 5433 (the sprint-24 Newton finding).
        root = tmp_path / "checkout"
        src = _write(root / "docker" / ".env",
                     "ORES_DB_PORT=5432\nORES_IAM_SERVICE_DB_PORT=5432\n")
        profile = root / ".env.newton"
        env_deploy._generate_profile(root, profile, src, "newton",
                                     "~/ores-deploy", "5433")
        text = profile.read_text(encoding="utf-8")
        assert "PGPORT=5433" in text
        assert "ORES_DB_PORT=5433" in text

    def test_pgport_rewritten_when_present_in_source(self, tmp_path):
        root = tmp_path / "checkout"
        src = _write(root / "docker" / ".env",
                     "PGPORT=5432\nORES_DB_PORT=5432\n")
        profile = root / ".env.host2"
        env_deploy._generate_profile(root, profile, src, "host2",
                                     "~/ores-deploy", "5433")
        text = profile.read_text(encoding="utf-8")
        assert "PGPORT=5433" in text
        # Only one PGPORT line.
        assert text.count("PGPORT=") == 1

    def test_no_pgport_when_remote_port_is_default(self, tmp_path):
        root = tmp_path / "checkout"
        src = _write(root / "docker" / ".env",
                     "ORES_DB_PORT=5432\n")
        profile = root / ".env.host2"
        env_deploy._generate_profile(root, profile, src, "host2",
                                     "~/ores-deploy", "5432")
        assert "PGPORT=" not in profile.read_text(encoding="utf-8")

    def test_no_rewrite_when_ports_match(self, tmp_path):
        root = tmp_path / "checkout"
        src = _write(root / "docker" / ".env",
                     "ORES_DB_PORT=5433\nORES_NATS_SERVICE_DB_PORT=5433\n")
        profile = root / ".env.host2"
        env_deploy._generate_profile(root, profile, src, "host2",
                                     "~/ores-deploy", "5433")
        text = profile.read_text(encoding="utf-8")
        assert "ORES_DB_PORT=5433" in text
        assert "ORES_DB_PORT=5432" not in text

    def test_strips_quotes_during_generation(self, tmp_path):
        root = tmp_path / "checkout"
        src = _write(root / "docker" / ".env",
                     'ORES_IAM_SERVICE_JWT_PRIVATE_KEY="k1\\nk2"\n')
        profile = root / ".env.host2"
        env_deploy._generate_profile(root, profile, src, "host2",
                                     "~/ores-deploy", "5433")
        assert "ORES_IAM_SERVICE_JWT_PRIVATE_KEY=k1\\nk2" in \
            profile.read_text(encoding="utf-8")

    def test_keeps_quotes_on_values_with_whitespace(self, tmp_path):
        # The profile is bash-sourced on the remote; an unquoted value
        # with spaces ("-----BEGIN PRIVATE KEY-----") would be executed
        # as a command. Quotes must survive on such lines (podman never
        # reads this value — remote-run.sh overrides it via --env).
        root = tmp_path / "checkout"
        src = _write(root / "docker" / ".env",
                     'ORES_IAM_SERVICE_JWT_PRIVATE_KEY='
                     '"-----BEGIN PRIVATE KEY-----\\nk1\\n'
                     '-----END PRIVATE KEY-----\\n"\n')
        profile = root / ".env.host2"
        env_deploy._generate_profile(root, profile, src, "host2",
                                     "~/ores-deploy", "5433")
        assert ('ORES_IAM_SERVICE_JWT_PRIVATE_KEY='
                '"-----BEGIN PRIVATE KEY-----\\nk1\\n'
                '-----END PRIVATE KEY-----\\n"') in \
            profile.read_text(encoding="utf-8")

    def test_non_db_port_values_untouched(self, tmp_path):
        root = tmp_path / "checkout"
        src = _write(root / "docker" / ".env",
                     "ORES_HTTP_PORT=8080\nORES_NATS_PORT=4222\n")
        profile = root / ".env.host2"
        env_deploy._generate_profile(root, profile, src, "host2",
                                     "~/ores-deploy", "5433")
        text = profile.read_text(encoding="utf-8")
        assert "ORES_HTTP_PORT=8080" in text
        assert "ORES_NATS_PORT=4222" in text


# --- _write_compute_env ---------------------------------------------------

class TestWriteComputeEnv:
    def _profile(self, tmp_path):
        keys = tmp_path / "serving" / "keys"
        _write(keys / "ca.crt", "CA")
        _write(keys / "client.crt", "CERT")
        _write(keys / "client.key", "KEY")
        return {
            "ORES_COMPUTE_TENANT_ID": "acme",
            "ORES_COMPUTE_NATS_URL": "nats://192.168.1.10:4222",
            "ORES_COMPUTE_NATS_SUBJECT_PREFIX": "ores.prod.main1",
            "ORES_COMPUTE_NATS_WIRE_FORMAT": "json",
            "ORES_COMPUTE_NATS_TLS_CA": str(keys / "ca.crt"),
            "ORES_COMPUTE_NATS_TLS_CERT": str(keys / "client.crt"),
            "ORES_COMPUTE_NATS_TLS_KEY": str(keys / "client.key"),
        }

    def test_writes_app_prefixed_env_with_remote_paths(self, tmp_path):
        root = tmp_path / "checkout"
        env_path, keys_stage = env_deploy._write_compute_env(
            root, self._profile(tmp_path), "prod-main1", "/home/marco/ores-deploy", "newton")
        text = env_path.read_text(encoding="utf-8")
        # The wrapper parser maps env via make_mapper("COMPUTE_WRAPPER"),
        # so the block must be app-prefixed (the TLS trio in particular
        # is excluded from the shared-domain fallback).
        assert "ORES_COMPUTE_WRAPPER_NATS_URL=nats://192.168.1.10:4222" in text
        assert "ORES_COMPUTE_WRAPPER_NATS_SUBJECT_PREFIX=ores.prod.main1" in text
        assert "ORES_COMPUTE_WRAPPER_NATS_WIRE_FORMAT=json" in text
        assert ("ORES_COMPUTE_WRAPPER_NATS_TLS_CA="
                "/home/marco/ores-deploy/compute/keys/ca.crt") in text
        assert ("ORES_COMPUTE_WRAPPER_NATS_TLS_CERT="
                "/home/marco/ores-deploy/compute/keys/client.crt") in text
        assert ("ORES_COMPUTE_WRAPPER_NATS_TLS_KEY="
                "/home/marco/ores-deploy/compute/keys/client.key") in text
        assert "ORES_COMPUTE_REPLICAS" not in text
        assert "ORES_COMPUTE_WRAPPER_TENANT_ID=acme" in text
        assert "ORES_COMPUTE_LABEL=prod-main1" in text

    def test_writes_replicas_only_when_the_profile_sets_them(self, tmp_path):
        profile = self._profile(tmp_path)
        profile["ORES_COMPUTE_REPLICAS"] = "3"
        env_path, _ = env_deploy._write_compute_env(
            tmp_path / "checkout", profile, "prod-main1", "/home/marco/ores-deploy", "newton")
        assert "ORES_COMPUTE_REPLICAS=3\n" in env_path.read_text(encoding="utf-8")

    def test_certs_staged_with_basenames(self, tmp_path):
        root = tmp_path / "checkout"
        _, keys_stage = env_deploy._write_compute_env(
            root, self._profile(tmp_path), "l", "/root", "newton")
        staged = sorted(p.name for p in keys_stage.iterdir())
        assert staged == ["ca.crt", "client.crt", "client.key"]

    def test_http_base_url_optional(self, tmp_path):
        root = tmp_path / "checkout"
        prof = self._profile(tmp_path)
        env_path, _ = env_deploy._write_compute_env(root, prof, "l", "/root", "newton")
        assert "ORES_COMPUTE_WRAPPER_HTTP_BASE_URL" not in \
            env_path.read_text(encoding="utf-8")
        prof["ORES_COMPUTE_HTTP_BASE_URL"] = "http://192.168.1.10:8080"
        env_path, _ = env_deploy._write_compute_env(root, prof, "l", "/root", "newton")
        assert ("ORES_COMPUTE_WRAPPER_HTTP_BASE_URL="
                "http://192.168.1.10:8080") in env_path.read_text(encoding="utf-8")

    def test_missing_tls_file_raises(self, tmp_path):
        root = tmp_path / "checkout"
        prof = self._profile(tmp_path)
        prof["ORES_COMPUTE_NATS_TLS_KEY"] = str(tmp_path / "gone.key")
        import pytest
        with pytest.raises(RuntimeError, match="compute TLS file not found"):
            env_deploy._write_compute_env(root, prof, "l", "/root", "newton")


# --- Deploys that run at the same time ------------------------------------

class TestDeploysAtTheSameTime:
    def _profile(self, tmp_path, replicas):
        keys = tmp_path / "serving" / "keys"
        _write(keys / "ca.crt", "CA")
        _write(keys / "client.crt", "CERT")
        _write(keys / "client.key", "KEY")
        return {
            "ORES_COMPUTE_TENANT_ID": "acme",
            "ORES_COMPUTE_NATS_URL": "nats://192.168.1.10:4222",
            "ORES_COMPUTE_NATS_SUBJECT_PREFIX": "ores.prod.main1",
            "ORES_COMPUTE_NATS_WIRE_FORMAT": "json",
            "ORES_COMPUTE_NATS_TLS_CA": str(keys / "ca.crt"),
            "ORES_COMPUTE_NATS_TLS_CERT": str(keys / "client.crt"),
            "ORES_COMPUTE_NATS_TLS_KEY": str(keys / "client.key"),
            "ORES_COMPUTE_REPLICAS": str(replicas),
        }

    def test_each_host_stages_its_own_compute_env(self, tmp_path):
        root = tmp_path / "checkout"
        newton, _ = env_deploy._write_compute_env(
            root, self._profile(tmp_path, 11), "l", "/r", "marco@192.168.1.22")
        laptop, _ = env_deploy._write_compute_env(
            root, self._profile(tmp_path, 3), "l", "/r", "marco@192.168.1.11")

        # One host's file is never the other's, so the replica count each host is sent cannot be
        # overwritten by a deploy that runs beside it.
        assert newton != laptop
        assert "ORES_COMPUTE_REPLICAS=11\n" in newton.read_text(encoding="utf-8")
        assert "ORES_COMPUTE_REPLICAS=3\n" in laptop.read_text(encoding="utf-8")

    def test_the_host_stage_is_a_safe_directory_name(self, tmp_path):
        stage = env_deploy._host_stage(tmp_path, "marco@192.168.1.22")
        assert stage.parent == tmp_path / "build" / "docker-stage" / "hosts"
        assert "@" not in stage.name and "/" not in stage.name

    def test_an_image_is_copied_with_the_standard_podman_command(self, tmp_path, monkeypatch):
        calls = []
        monkeypatch.setattr(env_deploy, "_run",
                            lambda cmd, root, **kw: calls.append(cmd))
        env_deploy._transfer_one_image(
            tmp_path, "marco@192.168.1.22", "localhost/ores-compute-wrapper:1.0", None, None)
        assert calls == [["podman", "image", "scp", "localhost/ores-compute-wrapper:1.0",
                          "marco@192.168.1.22::"]]

    def _checkout_with_fake_compass(self, tmp_path, failing=()):
        root = tmp_path / "checkout"
        root.mkdir()
        # The child of a many-host deploy is `compass.sh env deploy HOST ...`.
        script = root / "compass.sh"
        script.write_text(
            "#!/bin/sh\n"
            'echo "deploying $3"\n'
            'echo "Copying blob sha256:abc"\n'
            f'case "$3" in {"|".join(failing) or "NONE"}) exit 3;; esac\n'
            "exit 0\n", encoding="utf-8")
        script.chmod(0o755)
        return root

    def _args(self, hosts):
        import argparse
        return argparse.Namespace(host=hosts, role="compute", stop=False,
                                  purge=False, setup_host=False)

    def test_several_hosts_deploy_and_the_output_names_each(self, tmp_path, capsys):
        root = self._checkout_with_fake_compass(tmp_path)
        code = env_deploy._deploy_many(self._args(["newton", "laptop"]), root)
        out = capsys.readouterr().out
        assert code == 0
        assert "[newton] deploying newton" in out
        assert "[laptop] deploying laptop" in out
        # The layer lines are noise and are left out.
        assert "Copying blob" not in out
        assert "newton           ok" in out and "laptop           ok" in out

    def test_one_failed_host_fails_the_run_and_is_named(self, tmp_path, capsys):
        root = self._checkout_with_fake_compass(tmp_path, failing=("simba",))
        code = env_deploy._deploy_many(self._args(["newton", "simba"]), root)
        out = capsys.readouterr().out
        assert code == 1
        assert "simba            FAILED (exit 3)" in out
        assert "newton           ok" in out


# --- _remote_root ---------------------------------------------------------

class TestRemoteRoot:
    def test_expands_tilde_via_remote_home(self, monkeypatch):
        def fake_ssh(host, command, **kwargs):
            assert host == "newton"
            assert command == 'printf %s "$HOME"'
            return subprocess.CompletedProcess([], 0, stdout="/home/marco")
        monkeypatch.setattr(env_deploy, "_ssh", fake_ssh)
        assert env_deploy._remote_root("newton", "~/ores-deploy") == \
            "/home/marco/ores-deploy"

    def test_absolute_root_untouched(self, monkeypatch):
        def fake_ssh(host, command, **kwargs):
            raise AssertionError("should not ssh for an absolute root")
        monkeypatch.setattr(env_deploy, "_ssh", fake_ssh)
        assert env_deploy._remote_root("newton", "/opt/ores") == "/opt/ores"

    def test_empty_home_raises(self, monkeypatch):
        def fake_ssh(host, command, **kwargs):
            return subprocess.CompletedProcess([], 0, stdout="")
        monkeypatch.setattr(env_deploy, "_ssh", fake_ssh)
        import pytest
        with pytest.raises(RuntimeError, match="cannot resolve remote HOME"):
            env_deploy._remote_root("newton", "~/ores-deploy")


# --- _version_tag ---------------------------------------------------------

class TestVersionTag:
    def test_returns_version_commit(self, tmp_path, monkeypatch):
        cmake = tmp_path / "CMakeLists.txt"
        cmake.write_text(
            "project(OreStudio VERSION 0.0.25 LANGUAGES CXX\n")
        fake_subprocess = subprocess.CompletedProcess(
            [], 0, stdout="abc1234\n")
        monkeypatch.setattr(env_deploy.subprocess, "run",
                           lambda *a, **kw: fake_subprocess)
        assert env_deploy._version_tag(tmp_path) == "0.0.25-abc1234"

    def test_falls_back_to_0_0_0(self, tmp_path, monkeypatch):
        fake_subprocess = subprocess.CompletedProcess(
            [], 0, stdout="abc1234\n")
        monkeypatch.setattr(env_deploy.subprocess, "run",
                           lambda *a, **kw: fake_subprocess)
        assert env_deploy._version_tag(tmp_path) == "0.0.0-abc1234"

    def test_git_failure_falls_back_to_unknown(self, tmp_path, monkeypatch):
        cmake = tmp_path / "CMakeLists.txt"
        cmake.write_text(
            "project(OreStudio VERSION 0.0.25 LANGUAGES CXX\n")
        def fake_run(cmd, **kwargs):
            if cmd[0] == "git":
                return subprocess.CompletedProcess([], 1, stdout="")
            return subprocess.CompletedProcess([], 0, stdout="abc\n")
        monkeypatch.setattr(env_deploy.subprocess, "run", fake_run)
        assert env_deploy._version_tag(tmp_path) == "0.0.25-unknown"


# --- small helpers --------------------------------------------------------

class TestHelpers:
    def test_label_from_env(self, tmp_path):
        assert env_deploy._label({"ORES_CHECKOUT_LABEL": "swift_curie"},
                                 tmp_path) == "swift_curie"

    def test_label_falls_back_to_checkout_name(self, tmp_path):
        assert env_deploy._label({}, Path("/x/ores_dev_my_box")) == \
            "ores_dev_my_box"

    def test_profile_path(self, tmp_path):
        # The named-env-file registry: `--env <host>` resolves to the
        # same file (compass.py's _resolve_env_file).
        assert env_deploy._profile_path(tmp_path, "newton") == \
            tmp_path / ".env.newton"

    def test_missing_compute_keys(self):
        full = {k: "v" for k in env_deploy._COMPUTE_REQUIRED}
        assert env_deploy._missing_compute_keys(full) == []
        empty = dict(full)
        del empty["ORES_COMPUTE_TENANT_ID"]
        assert env_deploy._missing_compute_keys(empty) == \
            ["ORES_COMPUTE_TENANT_ID"]


# --- subprocess composition ----------------------------------------------

class TestStreamImages:
    def test_separates_base_nats_and_overlays(self, tmp_path, monkeypatch):
        """Base image is transferred whole; per-service overlays are
        scp'd and built remotely."""
        transfer_calls = []
        scp_calls = []
        ssh_calls = []
        svc_stage = tmp_path / "build" / "docker-stage" / "services"
        _write(svc_stage / "ores.iam.service" / "bin" / "ores.iam.service",
               "ELF...")

        def fake_transfer(project_root, host, img, stage, remote_tmp):
            transfer_calls.append(img)

        def fake_ssh(host, cmd, **kwargs):
            ssh_calls.append(cmd)

        def fake_scp(project_root, host, local, remote, recursive=False):
            scp_calls.append(str(local))

        monkeypatch.setattr(env_deploy, "_transfer_one_image", fake_transfer)
        monkeypatch.setattr(env_deploy, "_ssh", fake_ssh)
        monkeypatch.setattr(env_deploy, "_scp", fake_scp)
        monkeypatch.setattr(env_deploy, "_version_tag",
                            lambda r: "0.0.25-abc")
        # Pretend the services staging dir exists.
        monkeypatch.setattr(env_deploy.Path, "is_dir",
                            lambda self: True)

        env_deploy._stream_images(
            tmp_path, "marco@192.168.1.22",
            ["localhost/ores-service-base:0.0.25-abc",
             "localhost/ores.iam.service:0.0.25-abc",
             "localhost/ores.refdata.service:0.0.25-abc",
             "localhost/ores-nats:0.0.25-abc"])

        # Base image transferred via save/load.
        assert "localhost/ores-service-base:0.0.25-abc" in transfer_calls
        # NATS transferred via save/load.
        assert "localhost/ores-nats:0.0.25-abc" in transfer_calls
        # Overlays are NOT in transfer — they're built remotely.
        for svc in ["ores.iam.service", "ores.refdata.service"]:
            assert svc not in transfer_calls
        # Remote build script includes overlay services.


class TestRemoteScript:
    def test_env_assignments_and_stdin(self, monkeypatch):
        captured = {}

        def fake_ssh(host, command, input_text=None, check=False, **kwargs):
            captured["host"] = host
            captured["command"] = command
            captured["input"] = input_text
            captured["check"] = check
            return subprocess.CompletedProcess([], 0)
        monkeypatch.setattr(env_deploy, "_ssh", fake_ssh)
        env_deploy._remote_script("newton", "echo hi",
                                  {"REMOTE_ROOT": "/home/marco/ores-deploy",
                                   "ROLE": "compute"})
        assert captured["host"] == "newton"
        assert captured["command"] == \
            "REMOTE_ROOT=/home/marco/ores-deploy ROLE=compute bash -s"
        assert captured["input"] == "echo hi"
        assert captured["check"] is True

    def test_stop_script_removes_per_service_containers(self, monkeypatch):
        captured = {}

        def fake_ssh(host, command, input_text=None, check=False, **kwargs):
            captured["input"] = input_text
            return subprocess.CompletedProcess([], 0)
        monkeypatch.setattr(env_deploy, "_ssh", fake_ssh)

        repo_root = Path(__file__).resolve().parents[3]
        env_deploy._stop(repo_root, "newton",
                         {"ORES_CHECKOUT_LABEL": "brave_hopper"},
                         "/home/marco/ores-deploy", "runtime", purge=False)

        script = captured["input"]
        # remote-run.sh starts one container per service
        # (ores-<svc dotted-to-dashed>-<label>); the old single-pod name
        # ores-services-<label> is never created, so its removal must be
        # the label-scoped name filter, not fixed names.
        assert "ores-services-" not in script
        assert '--filter "name=ores-.*-${label}"' in script

    def test_setup_host_script_escapes_backticks(self, monkeypatch):
        captured = {}

        def fake_ssh(host, command, input_text=None, check=False, **kwargs):
            captured["input"] = input_text
            return subprocess.CompletedProcess([], 0)
        monkeypatch.setattr(env_deploy, "_ssh", fake_ssh)

        repo_root = Path(__file__).resolve().parents[3]
        env_deploy._setup_host(repo_root, "newton")

        script = captured["input"]
        # The "Done" echo runs inside `ssh host bash -s`: unescaped
        # backticks would execute `compass env deploy <host>` as command
        # substitution on the remote (with <host> read as a redirect).
        assert "\\`compass env deploy <host>\\`" in script


# --- deploy all -----------------------------------------------------------

class TestDeployAll:
    def _profiles(self, root):
        _write(root / ".env.goedel",
               "ORES_REMOTE_HOST=marco@192.168.1.199\n"
               "ORES_COMPUTE_NATS_URL=aaaa\n")
        _write(root / ".env.newton",
               "ORES_REMOTE_HOST=marco@192.168.1.22\n"
               "ORES_COMPUTE_NATS_URL=bbbb\n")
        _write(root / ".env.runtime-only", "ORES_REMOTE_HOST=marco@h\n")

    def test_lists_only_profiles_with_compute_settings(self, tmp_path):
        self._profiles(tmp_path)

        assert env_deploy._compute_aliases(tmp_path) == ["goedel", "newton"]

    def test_a_host_that_is_down_is_skipped_and_the_rest_deploy(
            self, tmp_path, monkeypatch, capsys):
        self._profiles(tmp_path)
        deployed = []
        monkeypatch.setattr(env_deploy, "_reachable",
                            lambda host, timeout=5: "199" in host)
        monkeypatch.setattr(
            env_deploy, "_cmd_deploy",
            lambda argv, root: deployed.append(argv) or 0)

        code = env_deploy.run(["all"], tmp_path)

        assert deployed == [["goedel", "--role", "compute"]]
        assert code == 1
        out = capsys.readouterr().out
        assert "down: no answer over ssh, skipped" in out
        assert "newton           down" in out
        assert "goedel           ok" in out

    def test_a_failing_host_does_not_stop_the_others(
            self, tmp_path, monkeypatch):
        self._profiles(tmp_path)
        seen = []

        def deploy(argv, root):
            seen.append(argv[0])
            if argv[0] == "goedel":
                raise RuntimeError("podman missing")
            return 0

        monkeypatch.setattr(env_deploy, "_reachable",
                            lambda host, timeout=5: True)
        monkeypatch.setattr(env_deploy, "_cmd_deploy", deploy)

        assert env_deploy.run(["all"], tmp_path) == 1
        assert seen == ["goedel", "newton"]

    def test_an_exit_inside_one_deploy_does_not_stop_the_others(
            self, tmp_path, monkeypatch):
        self._profiles(tmp_path)
        seen = []

        def deploy(argv, root):
            seen.append(argv[0])
            if argv[0] == "goedel":
                raise SystemExit(2)
            return 0

        monkeypatch.setattr(env_deploy, "_reachable",
                            lambda host, timeout=5: True)
        monkeypatch.setattr(env_deploy, "_cmd_deploy", deploy)

        assert env_deploy.run(["all"], tmp_path) == 1
        assert seen == ["goedel", "newton"]

    def test_succeeds_when_every_host_deploys(self, tmp_path, monkeypatch):
        self._profiles(tmp_path)
        monkeypatch.setattr(env_deploy, "_reachable",
                            lambda host, timeout=5: True)
        monkeypatch.setattr(env_deploy, "_cmd_deploy", lambda argv, root: 0)

        assert env_deploy.run(["all"], tmp_path) == 0

    def test_stop_is_passed_through(self, tmp_path, monkeypatch):
        self._profiles(tmp_path)
        seen = []
        monkeypatch.setattr(env_deploy, "_reachable",
                            lambda host, timeout=5: True)
        monkeypatch.setattr(
            env_deploy, "_cmd_deploy",
            lambda argv, root: seen.append(argv) or 0)

        env_deploy.run(["all", "--stop"], tmp_path)

        assert seen[0] == ["goedel", "--role", "compute", "--stop"]

    def test_no_compute_profile_is_an_error(self, tmp_path, capsys):
        assert env_deploy.run(["all"], tmp_path) == 1
        assert "No host profile has compute settings" in capsys.readouterr().out


class TestRequirePodman:
    def test_passes_when_podman_is_there(self, monkeypatch):
        monkeypatch.setattr(
            env_deploy, "_ssh",
            lambda host, command, **kw: subprocess.CompletedProcess([], 0))

        env_deploy._require_podman("marco@h")

    def test_names_the_host_and_the_fix_when_it_is_missing(self, monkeypatch):
        monkeypatch.setattr(
            env_deploy, "_ssh",
            lambda host, command, **kw: subprocess.CompletedProcess([], 1))

        with pytest.raises(RuntimeError) as err:
            env_deploy._require_podman("marco@192.168.1.11")

        assert "marco@192.168.1.11" in str(err.value)
        assert "sudo apt install podman" in str(err.value)
