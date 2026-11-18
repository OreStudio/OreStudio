"""Tests for image_build — the image names and the publish path."""

import subprocess
import sys
from pathlib import Path
from unittest import mock

import pytest

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import image_build
from image_build import image_names, publish, set_engine


@pytest.fixture(autouse=True)
def engine():
    set_engine("podman")
    yield
    set_engine("podman")


class TestImageNames:
    def test_names_the_two_shared_images_and_nats(self, monkeypatch):
        monkeypatch.setattr(image_build, "runtime_services", lambda root: [])
        assert image_names(Path(".")) == [
            "ores-service-base", "ores-service-libs", "ores-nats",
            "ores-compute-wrapper", "ores-web", "ores-shell"]

    def test_every_runtime_service_is_a_name(self, monkeypatch):
        monkeypatch.setattr(
            image_build, "runtime_services",
            lambda root: ["ores.iam.service", "ores.trade.service"])
        assert image_names(Path("."))[6:] == [
            "ores.iam.service", "ores.trade.service"]


class TestPublish:
    def test_publishes_every_image_under_the_tag_and_the_alias(self):
        calls = []

        def fake_run(cmd, project_root, check=True, **kwargs):
            calls.append(cmd)
            return subprocess.CompletedProcess(cmd, 0)

        with mock.patch.object(image_build, "runtime_services",
                               lambda root: ["ores.iam.service"]), \
             mock.patch.object(image_build, "_published", lambda ref: False), \
             mock.patch.object(image_build, "_image_exists", lambda ref: True), \
             mock.patch.object(image_build, "_run", fake_run):
            pushed = publish(Path("."), "ghcr.io/orestudio", "1.0.0-abc",
                             "main")

        assert pushed == 7
        pushes = [c for c in calls if "push" in c]
        assert [c[-1] for c in pushes] == [
            "ghcr.io/orestudio/ores-service-base:1.0.0-abc",
            "ghcr.io/orestudio/ores-service-base:main",
            "ghcr.io/orestudio/ores-service-libs:1.0.0-abc",
            "ghcr.io/orestudio/ores-service-libs:main",
            "ghcr.io/orestudio/ores-nats:1.0.0-abc",
            "ghcr.io/orestudio/ores-nats:main",
            "ghcr.io/orestudio/ores-compute-wrapper:1.0.0-abc",
            "ghcr.io/orestudio/ores-compute-wrapper:main",
            "ghcr.io/orestudio/ores-web:1.0.0-abc",
            "ghcr.io/orestudio/ores-web:main",
            "ghcr.io/orestudio/ores-shell:1.0.0-abc",
            "ghcr.io/orestudio/ores-shell:main",
            "ghcr.io/orestudio/ores.iam.service:1.0.0-abc",
            "ghcr.io/orestudio/ores.iam.service:main",
        ]

    def test_skips_a_tag_the_registry_already_holds(self):
        calls = []

        def fake_run(cmd, project_root, check=True, **kwargs):
            calls.append(cmd)
            return subprocess.CompletedProcess(cmd, 0)

        with mock.patch.object(image_build, "runtime_services", lambda root: []), \
             mock.patch.object(image_build, "_published", lambda ref: True), \
             mock.patch.object(image_build, "_image_exists", lambda ref: True), \
             mock.patch.object(image_build, "_run", fake_run):
            pushed = publish(Path("."), "ghcr.io/orestudio", "1.0.0-abc")

        assert pushed == 0
        assert calls == []

    def test_a_dry_run_pushes_nothing(self):
        calls = []

        def fake_run(cmd, project_root, check=True, **kwargs):
            calls.append(cmd)
            return subprocess.CompletedProcess(cmd, 0)

        with mock.patch.object(image_build, "runtime_services", lambda root: []), \
             mock.patch.object(image_build, "_published", lambda ref: False), \
             mock.patch.object(image_build, "_image_exists", lambda ref: True), \
             mock.patch.object(image_build, "_run", fake_run):
            pushed = publish(Path("."), "ghcr.io/orestudio", "1.0.0-abc",
                             dry_run=True)

        assert pushed == 6
        assert calls == []


class TestBuildArgs:
    def test_podman_asks_for_the_docker_format(self):
        set_engine("podman")
        assert image_build._build_args(["-t", "x"]) == [
            "podman", "build", "--format", "docker", "-t", "x"]

    def test_docker_keeps_its_default_format(self):
        set_engine("docker")
        assert image_build._build_args(["-t", "x"]) == [
            "docker", "build", "-t", "x"]

    def test_an_unknown_engine_is_refused(self):
        with pytest.raises(ValueError, match="unknown container engine"):
            set_engine("nerdctl")
