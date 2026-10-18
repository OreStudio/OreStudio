"""Tests for build/scripts/check_stream_coverage.py.

Run::

    projects/ores.codegen/venv/bin/python -m pytest \
        projects/ores.codegen/tests/test_check_stream_coverage.py -q

The check reads the declarations a service sends at start, so the tests build a
small tree of declarations and ask what the check makes of it. The subject rules
are tested on their own, because every verdict rests on them.
"""
import importlib.util
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SPEC = importlib.util.spec_from_file_location(
    "check_stream_coverage", REPO_ROOT / "build" / "scripts" / "check_stream_coverage.py")
check_stream_coverage = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = check_stream_coverage
SPEC.loader.exec_module(check_stream_coverage)

check = check_stream_coverage.check


def declarations(root, files):
    for rel, text in files.items():
        path = root / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")


def application(streams):
    body = "\n".join(
        f'        admin.ensure_stream(\n'
        f'            nats.make_stream_name("{name}"),\n'
        f'            nats.covering_subjects({{{", ".join(subjects)}}}));'
        for name, subjects in streams
    )
    return (
        "#include <ores.nats/service/client.hpp>\n\n"
        "void run() {\n"
        "    ores::nats::service::client nats;\n"
        "    auto admin = nats.make_admin();\n"
        "    try {\n"
        f"{body}\n"
        "    } catch (const std::exception& e) {\n"
        "        throw;\n"
        "    }\n"
        "}\n"
    )


def registrar(service, routes, subjects, group=""):
    initialiser = f'{{"{group}"}}' if group else ""
    on_lines = "\n".join(
        f'    routes.on("{subject}", [](ores::nats::message msg) {{}});'
        for subject in subjects
    )
    return (
        "#include <ores.service/messaging/step_command_stream.hpp>\n\n"
        "void register_handlers(ores::nats::service::client& nats) {\n"
        f"    ores::service::messaging::step_routes routes{initialiser};\n"
        f"{on_lines}\n"
        "    ores::service::messaging::serve_step_commands("
        f'nats, "{service}", routes);\n'
        "}\n"
    )


def test_a_stream_over_its_step_family_is_clean(tmp_path):
    declarations(tmp_path, {
        "projects/a/core/src/messaging/registrar.cpp":
            registrar("ores.a.service", "routes", ["a.v1.ops.step"]),
    })
    findings, catalogue = check(tmp_path)
    assert findings == []
    assert list(catalogue.streams) == ["a_steps"]
    assert catalogue.streams["a_steps"].filters == ("a.v1.ops.step", "a.v1.ops.step.>")


def raw_registrar(subject, stream_var, stream_suffix):
    return (
        "#include <ores.nats/service/client.hpp>\n\n"
        "void register_handlers(ores::nats::service::client& nats) {\n"
        f'    const std::string {stream_var}(nats.make_stream_name("{stream_suffix}"));\n'
        f'    nats.js_queue_subscribe("{subject}", "raw", "q",'
        f" [](ores::nats::message msg) {{}}, {stream_var}, 10);\n"
        "}\n"
    )


def test_a_durable_consumer_with_no_stream_fails(tmp_path):
    declarations(tmp_path, {
        "projects/a/service/src/app/application.cpp":
            application([("a", ['"a.v1.ops.other"'])]),
        "projects/a/core/src/messaging/registrar.cpp":
            raw_registrar("a.v1.ops.step", "stream", "a"),
    })
    findings, _ = check(tmp_path)
    assert any(
        "durable subject with no stream: 'a.v1.ops.step'" in finding
        for finding in findings
    ), findings


def test_a_raw_durable_consumer_names_the_stream_it_reads(tmp_path):
    declarations(tmp_path, {
        "projects/a/service/src/app/application.cpp":
            application([("a", ['"a.v1.ops.step"'])]),
        "projects/a/core/src/messaging/registrar.cpp":
            raw_registrar("a.v1.ops.step", "stream", "b"),
    })
    findings, _ = check(tmp_path)
    assert any("durable consumer on an unensured stream: 'b'" in finding
               for finding in findings), findings


def test_two_streams_over_one_subject_fail(tmp_path):
    declarations(tmp_path, {
        "projects/a/service/src/app/application.cpp":
            application([("a", ['"a.v1.ops.step"']), ("b", ['"a.v1.ops.step"'])]),
    })
    findings, _ = check(tmp_path)
    assert any("stream subject overlap: 'a' filter 'a.v1.ops.step'" in finding
               for finding in findings), findings
    assert any("JSStreamSubjectOverlapErr" in finding for finding in findings), findings


def test_one_stream_name_with_two_filter_sets_fails(tmp_path):
    declarations(tmp_path, {
        "projects/a/service/src/app/application.cpp":
            application([("a", ['"a.v1.ops.step"'])]),
        "projects/b/service/src/app/application.cpp":
            application([("a", ['"a.v1.ops.other"'])]),
    })
    findings, _ = check(tmp_path)
    assert any("one stream, two filter sets: 'a'" in finding
               for finding in findings), findings


def test_an_unreadable_declaration_fails(tmp_path):
    declarations(tmp_path, {
        "projects/a/core/src/messaging/registrar.cpp":
            "#include <ores.service/messaging/step_command_stream.hpp>\n\n"
            "void register_handlers(ores::nats::service::client& nats) {\n"
            "    ores::service::messaging::serve_step_commands("
            'nats, "ores.a.service", somewhere_else);\n'
            "}\n",
    })
    findings, _ = check(tmp_path)
    assert any("the routes table 'somewhere_else' is not declared" in finding
               for finding in findings), findings


def test_an_empty_tree_fails(tmp_path):
    findings, _ = check(tmp_path)
    assert any("no stream declaration found" in finding for finding in findings), findings


def test_a_filter_matches_its_children_but_not_itself(tmp_path):
    matches = check_stream_coverage.filter_matches
    assert matches("a.v1.ops.step", "a.v1.ops.step")
    assert matches("a.v1.ops.step.>", "a.v1.ops.step.one")
    assert not matches("a.v1.ops.step.>", "a.v1.ops.step")
    assert matches("a.v1.ops.*", "a.v1.ops.step")
    assert not matches("a.v1.ops.*", "a.v1.ops.step.one")
    assert not matches("a.v1.ops.step.>", "a.v1.ops.other")


def test_two_filters_overlap_when_one_subject_matches_both():
    overlaps = check_stream_coverage.filters_overlap
    assert overlaps("a.v1.ops.step", "a.v1.ops.step")
    assert overlaps("a.v1.ops.>", "a.v1.ops.step")
    assert overlaps("a.v1.ops.*", "a.v1.ops.step")
    assert overlaps("a.v1.ops.>", "a.v1.ops.step.>")
    assert not overlaps("a.v1.ops.step", "a.v1.ops.step.>")
    assert not overlaps("a.v1.ops.step", "a.v1.ops.other")


def test_a_consumer_on_a_stream_that_does_not_cover_it_fails(tmp_path):
    declarations(tmp_path, {
        "projects/a/service/src/app/application.cpp":
            application([("a", ['"a.v1.ops.other"']), ("b", ['"a.v1.ops.step"'])]),
        "projects/a/core/src/messaging/registrar.cpp":
            raw_registrar("a.v1.ops.step", "stream", "a"),
    })
    findings, _ = check(tmp_path)
    assert any("durable consumer outside its stream: 'a.v1.ops.step'" in finding
               for finding in findings), findings


def test_a_new_durable_entry_point_in_the_transport_fails(tmp_path):
    declarations(tmp_path, {
        "projects/ores.nats/include/ores.nats/service/client.hpp":
            "class client {\n"
            "    subscription js_queue_subscribe(int);\n"
            "    subscription js_durable_consume(int);\n"
            "};\n",
    })
    findings, _ = check(tmp_path)
    assert any("JetStream call this check does not read: 'js_durable_consume'"
               in finding for finding in findings), findings
