"""Tests for build/scripts/check_forward_declarable_includes.py.

Run::

    python3 -m pytest build/scripts/tests/test_forward_declarable_includes.py

The check answers one question per header and type: does this header include the
definition of a type it only names? Everything it must not report is a header
that uses the type in full -- a member, a return by value, a vector, a
dereference inside an inline function -- and everything it must report is a
header that borrows the name and nothing else. Both directions matter: the
silent side keeps the check from demanding an impossible fix, and the loud side
is the rule.
"""
import importlib.util
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SPEC = importlib.util.spec_from_file_location(
    "check_forward_declarable_includes",
    REPO_ROOT / "build" / "scripts" / "check_forward_declarable_includes.py")
check = importlib.util.module_from_spec(SPEC)
# A dataclass looks itself up by module name while it is defined, so the module
# has to be registered before its body runs.
sys.modules[SPEC.name] = check
SPEC.loader.exec_module(check)

CONTEXT = "ores.database/domain/context.hpp"
CONTEXT_FWD = "ores.database/domain/context_fwd.hpp"


def tree(tmp_path, headers, forwarded=True):
    """A repo root with the context header and one file per entry in headers.

    headers maps a path under projects/ to its body. The defining header and
    its forward declaration are written first, so a body only has to name them.
    """
    root = tmp_path
    include = root / "projects" / "ores.database" / "include" / "ores.database"
    (include / "domain").mkdir(parents=True)
    (include / "domain" / "context.hpp").write_text(
        "namespace ores::database {\nclass context {\npublic:\n"
        "    int pq_pool() const;\n};\n}\n", encoding="utf-8")
    (include / "domain" / "context_fwd.hpp").write_text(
        "namespace ores::database {\nclass context;\n}\n", encoding="utf-8")
    for path, body in headers.items():
        file = root / "projects" / path
        file.parent.mkdir(parents=True, exist_ok=True)
        file.write_text(body, encoding="utf-8")
    return root


def configure(tmp_path, monkeypatch, forwarding=CONTEXT_FWD):
    """Points the check at a type list and a baseline inside the test tree."""
    types = tmp_path / "types.list"
    types.write_text(
        f"ores::database::context class {CONTEXT} "
        f"{forwarding or '-'}\n", encoding="utf-8")
    baseline = tmp_path / "includes.baseline"
    baseline.write_text("# no debt\n", encoding="utf-8")
    monkeypatch.setattr(check, "LIST_PATH", types)
    monkeypatch.setattr(check, "BASELINE_PATH", baseline)
    return baseline


def run(root, monkeypatch, capsys, *extra):
    monkeypatch.setattr("sys.argv", ["check_forward_declarable_includes.py",
                                     "--root", str(root), *extra])
    return check.main()


def test_a_header_that_only_borrows_the_name_is_reported(
        tmp_path, monkeypatch, capsys):
    root = tree(tmp_path, {"ores.x/include/ores.x/service.hpp":
                           f'#include "{CONTEXT}"\n'
                           'void write(const ores::database::context& ctx);\n'})
    configure(tmp_path, monkeypatch)
    assert run(root, monkeypatch, capsys) == 1
    assert "NEW offenders" in capsys.readouterr().out


def test_the_forward_declaration_header_satisfies_the_rule(
        tmp_path, monkeypatch, capsys):
    root = tree(tmp_path, {"ores.x/include/ores.x/service.hpp":
                           f'#include "{CONTEXT_FWD}"\n'
                           'void write(const ores::database::context& ctx);\n'})
    configure(tmp_path, monkeypatch)
    assert run(root, monkeypatch, capsys) == 0


def test_a_by_value_parameter_needs_the_definition(
        tmp_path, monkeypatch, capsys):
    root = tree(tmp_path, {"ores.x/include/ores.x/service.hpp":
                           f'#include "{CONTEXT}"\n'
                           'void write(ores::database::context ctx);\n'})
    configure(tmp_path, monkeypatch)
    assert run(root, monkeypatch, capsys) == 0


def test_a_member_held_by_value_needs_the_definition(
        tmp_path, monkeypatch, capsys):
    root = tree(tmp_path, {"ores.x/include/ores.x/service.hpp":
                           f'#include "{CONTEXT}"\n'
                           'struct service {\n'
                           '    ores::database::context ctx_;\n};\n'})
    configure(tmp_path, monkeypatch)
    assert run(root, monkeypatch, capsys) == 0


def test_an_inline_body_that_calls_through_a_reference_needs_the_definition(
        tmp_path, monkeypatch, capsys):
    root = tree(tmp_path, {"ores.x/include/ores.x/service.hpp":
                           f'#include "{CONTEXT}"\n'
                           'inline int pool_size(const ores::database::context& ctx) {\n'
                           '    return ctx.pq_pool();\n}\n'})
    configure(tmp_path, monkeypatch)
    assert run(root, monkeypatch, capsys) == 0


def test_a_vector_of_the_type_needs_the_definition(
        tmp_path, monkeypatch, capsys):
    root = tree(tmp_path, {"ores.x/include/ores.x/service.hpp":
                           f'#include "{CONTEXT}"\n'
                           'struct service {\n'
                           '    std::vector<ores::database::context> ctxs_;\n};\n'})
    configure(tmp_path, monkeypatch)
    assert run(root, monkeypatch, capsys) == 0


def test_a_shared_pointer_argument_only_names_the_type(
        tmp_path, monkeypatch, capsys):
    root = tree(tmp_path, {"ores.x/include/ores.x/service.hpp":
                           f'#include "{CONTEXT}"\n'
                           'struct service {\n'
                           '    std::shared_ptr<ores::database::context> ctx_;\n'
                           '};\n'})
    configure(tmp_path, monkeypatch)
    assert run(root, monkeypatch, capsys) == 1


def test_a_unique_pointer_argument_needs_the_definition(
        tmp_path, monkeypatch, capsys):
    root = tree(tmp_path, {"ores.x/include/ores.x/service.hpp":
                           f'#include "{CONTEXT}"\n'
                           'struct service {\n'
                           '    std::unique_ptr<ores::database::context> ctx_;\n'
                           '};\n'})
    configure(tmp_path, monkeypatch)
    assert run(root, monkeypatch, capsys) == 0


def test_a_use_in_a_comment_is_not_a_use(tmp_path, monkeypatch, capsys):
    root = tree(tmp_path, {"ores.x/include/ores.x/service.hpp":
                           f'#include "{CONTEXT}"\n'
                           '// ores::database::context ctx is handed in.\n'
                           'void write(const ores::database::context& ctx);\n'})
    configure(tmp_path, monkeypatch)
    assert run(root, monkeypatch, capsys) == 1


def test_a_short_name_bound_by_an_alias_is_a_use(tmp_path, monkeypatch, capsys):
    root = tree(tmp_path, {"ores.x/include/ores.x/service.hpp":
                           f'#include "{CONTEXT}"\n'
                           'namespace x {\n'
                           'using context = ores::database::context;\n'
                           'void write(const context& ctx);\n}\n'})
    configure(tmp_path, monkeypatch)
    assert run(root, monkeypatch, capsys) == 1


def test_a_generated_header_is_reported_once_for_its_template(
        tmp_path, monkeypatch, capsys):
    banner = ("/**\n * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY\n"
              " * Template: cpp_widget.hpp.mustache\n */\n")
    body = (banner + f'#include "{CONTEXT}"\n'
            'void write(const ores::database::context& ctx);\n')
    root = tree(tmp_path, {
        "ores.x/include/ores.x/a.hpp": body,
        "ores.x/include/ores.x/b.hpp": body,
    })
    configure(tmp_path, monkeypatch)
    assert run(root, monkeypatch, capsys) == 1
    out = capsys.readouterr().out
    assert "template:cpp_widget.hpp.mustache" in out


def test_the_definition_itself_is_not_an_offender(
        tmp_path, monkeypatch, capsys):
    root = tree(tmp_path, {})
    configure(tmp_path, monkeypatch)
    assert run(root, monkeypatch, capsys) == 0


def test_a_baseline_entry_silences_the_offender(tmp_path, monkeypatch, capsys):
    root = tree(tmp_path, {"ores.x/include/ores.x/service.hpp":
                           f'#include "{CONTEXT}"\n'
                           'void write(const ores::database::context& ctx);\n'})
    baseline = configure(tmp_path, monkeypatch)
    assert run(root, monkeypatch, capsys, "--write-baseline") == 0
    assert baseline.read_text().strip().endswith("service.hpp")
    assert run(root, monkeypatch, capsys) == 0


def test_an_offender_added_after_the_baseline_fails(
        tmp_path, monkeypatch, capsys):
    root = tree(tmp_path, {"ores.x/include/ores.x/service.hpp":
                           f'#include "{CONTEXT}"\n'
                           'void write(const ores::database::context& ctx);\n'})
    configure(tmp_path, monkeypatch)
    assert run(root, monkeypatch, capsys, "--write-baseline") == 0
    (root / "projects" / "ores.x" / "include" / "ores.x" / "extra.hpp").write_text(
        f'#include "{CONTEXT}"\n'
        'void remove(const ores::database::context& ctx);\n', encoding="utf-8")
    assert run(root, monkeypatch, capsys) == 1
    assert "extra.hpp" in capsys.readouterr().out


def test_a_type_declared_in_place_needs_no_forwarding_header(
        tmp_path, monkeypatch, capsys):
    root = tree(tmp_path, {"ores.x/include/ores.x/service.hpp":
                           f'#include "{CONTEXT}"\n'
                           'void write(const ores::database::context& ctx);\n'})
    configure(tmp_path, monkeypatch, forwarding=None)
    assert run(root, monkeypatch, capsys) == 1
    assert "declare class ores::database::context;" in capsys.readouterr().out


def test_an_empty_type_list_is_an_error(tmp_path, monkeypatch):
    types = tmp_path / "types.list"
    types.write_text("# nothing\n", encoding="utf-8")
    monkeypatch.setattr(check, "LIST_PATH", types)
    try:
        check.read_declarables(types)
    except SystemExit as error:
        assert "guard is off" in str(error)
    else:
        raise AssertionError("an empty type list must not pass silently")
