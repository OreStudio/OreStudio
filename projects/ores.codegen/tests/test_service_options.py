"""Tests for the options a service model declares in its * Options section.

Run with:
    python3 -m pytest projects/ores.codegen/tests/test_service_options.py

A service names its own options once, and codegen writes the member, its wire
field, its command-line declaration and its assignment from that list. A
service whose application is hand-written turns ores.cpp.service-app off and
its five config addresses on, and gets the config files and nothing else.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.generate import resolve_targets  # noqa: E402
from codegen.org_loader import load_org_component_overview_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects" / "ores.codegen"

CONFIG = ("config_options_header", "config_options_impl", "config_parser_header",
          "config_parser_impl", "config_parser_exception")

SERVICE = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000ce
{drawer}:END:
#+title: ores.demo.service
#+type: ores.codegen.component
#+component_kind: service
#+name: demo.service
#+full_name: ores.demo.service
#+brief: Demo service

* Summary

Fixture body.

{options}"""

OPTIONS = """\
* Options
:PROPERTIES:
:group: Demo
:END:

** http_base_url
:PROPERTIES:
:cpp_type: std::string
:default: "http://localhost:8080"
:help: Base URL of the "storage" API
:END:

Base URL of the storage API.

** writer_password
:PROPERTIES:
:cpp_type: std::string
:option: writer-db-password
:default: ""
:help: Password of the writer role
:secret: t
:END:

Password of the writer role.
"""


def _model(tmp_path, options=OPTIONS, drawer=""):
    path = tmp_path / "projects/ores.demo/service/modeling/component_overview.org"
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(SERVICE.format(drawer=drawer, options=options), encoding="utf-8")
    return path


def _component(tmp_path, options=OPTIONS):
    return load_org_component_overview_model(_model(tmp_path, options))["component"]


def test_each_option_is_read_in_model_order(tmp_path):
    c = _component(tmp_path)

    assert [o["name"] for o in c["service_options"]] == ["http_base_url", "writer_password"]
    assert c["service_options_group"] == "Demo"


def test_the_flag_is_the_member_name_unless_the_model_names_it(tmp_path):
    flags = [o["option"] for o in _component(tmp_path)["service_options"]]

    assert flags == ["http-base-url", "writer-db-password"]


def test_a_secret_is_counted_so_the_shape_guard_allows_its_absence(tmp_path):
    c = _component(tmp_path)

    assert [o["secret"] for o in c["service_options"]] == [False, True]
    assert c["service_secret_count"] == 1


def test_help_text_is_escaped_for_a_cpp_string(tmp_path):
    assert _component(tmp_path)["service_options"][0]["help"] == \
        'Base URL of the \\"storage\\" API'


def test_a_service_without_options_declares_none(tmp_path):
    assert _component(tmp_path, options="")["has_service_options"] is False


def test_an_option_without_a_default_is_refused(tmp_path):
    with pytest.raises(ValueError, match="default"):
        _component(tmp_path, OPTIONS.replace(':default: "http://localhost:8080"\n', ""))


def test_options_without_a_group_are_refused(tmp_path):
    with pytest.raises(ValueError, match=":group:"):
        _component(tmp_path, OPTIONS.replace(":group: Demo\n", ""))


def test_a_hand_written_service_gets_its_config_and_nothing_else(tmp_path):
    drawer = ":ores.cpp.service-app.enabled: nil\n" + "".join(
        f":ores.cpp.service-app.{a}.enabled: true\n" for a in CONFIG)
    units, _, _ = resolve_targets(_model(tmp_path, drawer=drawer), CODEGEN_BASE,
                                  address="ores.cpp.service-app")

    outputs = sorted(u["output"] for u in units)
    assert outputs and all("/config/" in o for o in outputs), outputs
    assert len(outputs) == len(CONFIG)
