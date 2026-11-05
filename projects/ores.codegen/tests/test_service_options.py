"""Tests for the options a service model declares in its * Options section.

Run with:
    python3 -m pytest projects/ores.codegen/tests/test_service_options.py

A service names its own options once, and codegen writes the member, its wire
field, its command-line declaration and its assignment from that list. A
service whose application is hand-written turns ores.cpp.service-app off and
its five config addresses on, and gets the config files and nothing else.
"""
import re
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402
from codegen.generate import resolve_targets  # noqa: E402
from codegen.org_loader import load_org_component_overview_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects" / "ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

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


POSITIVE = OPTIONS + """
** workers
:PROPERTIES:
:cpp_type: std::uint32_t
:default: 4
:help: Worker threads
:positive: t
:END:

The worker threads.
"""


def _render(tmp_path, template, options=POSITIVE):
    model = _model(tmp_path, options)
    out = tmp_path / "out"
    generate_from_model(str(model), DATA_DIR, TEMPLATES_DIR, out, is_processing_batch=True,
                        target_template=template, target_output="rendered")
    return (out / "rendered").read_text(encoding="utf-8")


def test_the_parser_checks_a_positive_integral_option(tmp_path):
    parser = _render(tmp_path, "cpp_service_config_parser.cpp.mustache")

    assert 'r.workers = vm["workers"].as<std::uint32_t>();' in parser
    assert "if (!std::isfinite(r.workers) || r.workers <= 0)" in parser
    assert '"--workers must be a positive number."' in parser
    assert "#include <cmath>" in parser


def test_the_parser_declares_every_option_under_its_flag(tmp_path):
    parser = _render(tmp_path, "cpp_service_config_parser.cpp.mustache")

    assert 'options_description own("Demo");' in parser
    assert '"writer-db-password"' in parser
    assert 'r.writer_password = vm["writer-db-password"].as<std::string>();' in parser


def test_a_secret_is_a_member_but_not_in_the_wire_shape(tmp_path):
    header = _render(tmp_path, "cpp_service_config_options.hpp.mustache")

    assert "std::string writer_password;" in header
    assert '"writer_password"' not in header
    assert "names_every_member<options>(1)" in header


def test_a_misspelt_secret_is_refused(tmp_path):
    with pytest.raises(ValueError, match="Write t or nil"):
        _component(tmp_path, OPTIONS.replace(":secret: t", ":secret: tru"))


def test_the_group_key_is_read_in_any_case(tmp_path):
    assert _component(tmp_path, OPTIONS.replace(":group:", ":GROUP:"))[
        "service_options_group"] == "Demo"


OPTIONS_STRUCT = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000cf
:ores.cpp.field-group.field_group_header.enabled: nil
:ores.cpp.field-group.options_header.enabled: true
:ores.ts.enabled: nil
:END:
#+title: ores.demo.demo_options
#+type: ores.codegen.field_group
#+component: demo
#+entity_singular: demo_options
#+namespace: ores::demo
#+brief: Options of the demo.
#+header: config/demo_options.hpp
{printer}
* Fields

** url
:PROPERTIES:
:cpp_type: std::string
:default_value: "nats://localhost:4222"
:END:

The server.

** password
:PROPERTIES:
:cpp_type: std::string
:secret: t
:END:
"""


def _options_header(tmp_path, printer="#+export_macro: ORES_DEMO_EXPORT"):
    model = tmp_path / "projects/ores.demo/modeling/ores.demo.demo_options_field_group.org"
    model.parent.mkdir(parents=True, exist_ok=True)
    model.write_text(OPTIONS_STRUCT.format(printer=printer), encoding="utf-8")
    units, _, _ = resolve_targets(model, CODEGEN_BASE, address="ores.cpp.field-group")
    outputs = [u["output"] for u in units]
    out = tmp_path / "out"
    generate_from_model(str(model), DATA_DIR, TEMPLATES_DIR, out, is_processing_batch=True,
                        target_template="cpp_options_header.hpp.mustache",
                        target_output="rendered")
    return outputs, (out / "rendered").read_text(encoding="utf-8")


def test_an_options_struct_is_written_where_its_model_names_it(tmp_path):
    outputs, _ = _options_header(tmp_path)

    assert outputs == ["projects/ores.demo/include/ores.demo/config/demo_options.hpp"]


def test_an_options_struct_leaves_its_secret_out_of_the_shape(tmp_path):
    _, header = _options_header(tmp_path)

    assert "std::string password;" in header
    assert '"password"' not in header
    assert "names_every_member<demo_options>(1)" in header
    assert "#ifndef ORES_DEMO_CONFIG_DEMO_OPTIONS_HPP" in header


def test_an_options_struct_declares_its_printer_unless_told_not_to(tmp_path):
    _, with_printer = _options_header(tmp_path)
    _, without = _options_header(tmp_path, printer="#+printer: none")

    assert "ORES_DEMO_EXPORT std::ostream& operator<<" in with_printer
    assert "operator<<" not in without
    assert "<iosfwd>" not in without


def test_a_default_is_brace_initialised_so_an_explicit_constructor_compiles(tmp_path):
    """std::chrono::seconds has an explicit constructor: '= 30' does not compile."""
    chrono = OPTIONS_STRUCT.replace("** password", """** timeout
:PROPERTIES:
:cpp_type: std::chrono::seconds
:default_value: 30
:END:

** password""")
    model = tmp_path / "projects/ores.demo/modeling/ores.demo.demo_options_field_group.org"
    model.parent.mkdir(parents=True, exist_ok=True)
    model.write_text(chrono.format(printer=""), encoding="utf-8")
    out = tmp_path / "out"
    generate_from_model(str(model), DATA_DIR, TEMPLATES_DIR, out, is_processing_batch=True,
                        target_template="cpp_options_header.hpp.mustache",
                        target_output="rendered")
    header = (out / "rendered").read_text(encoding="utf-8")

    assert re.search(r"std::chrono::seconds timeout\{\s*30\s*\};", header)
    assert "= 30" not in header


def test_a_service_option_initialiser_is_brace_initialised(tmp_path):
    options = POSITIVE.replace(":default: 4\n", ":default: 4\n:initial: 4\n")
    header = _render(tmp_path, "cpp_service_config_options.hpp.mustache", options)

    assert re.search(r"std::uint32_t workers\{\s*4\s*\};", header)
