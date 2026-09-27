"""Tests for check_paste_block_symbols.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_paste_block_symbols.py

A model can paste hand-written C++ into generated output through an
``:implements`` block, and the block reaches the compiler only when its
component is regenerated. So a block can name a symbol that a rename elsewhere
deleted, and nothing notices until somebody regenerates and the build breaks.

These tests drive the check against a throw-away tree: a declared name resolves,
an undeclared one fails, the same identifier under the wrong namespace fails even
though it exists elsewhere, and an export macro between the keyword and the name
does not hide the declaration.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/scripts"))

import check_paste_block_symbols as cps  # noqa: E402

MODEL_DIR = "projects/ores.widget/modeling"
HEADER = "projects/ores.widget/include/ores.widget/messaging/widget.hpp"

# The shape the codebase writes: a namespace chain, and an export macro between
# the keyword and the type's name.
HEADER_BODY = """\
#pragma once
namespace ores::widget::messaging {
constexpr std::string_view widget_id_header = "x-widget-id";

enum class step_outcome { completed, failed };

class ORES_WIDGET_EXPORT widget_transfer final {
public:
    static widget_transfer system();
};

namespace detail {
int helper();
}
}
"""


def _write(path: Path, body: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(body, encoding="utf-8")


def _model(body: str) -> str:
    return ("#+title: ores.widget.widget\n#+type: ores.codegen.entity\n\n"
            + body)


def _point_at(monkeypatch, repo: Path) -> None:
    monkeypatch.setattr(cps, "REPO_ROOT", repo)
    monkeypatch.setattr(cps, "PROJECTS_DIR", repo / "projects")


def _tree(tmp_path: Path, block: str) -> Path:
    _write(tmp_path / HEADER, HEADER_BODY)
    _write(tmp_path / MODEL_DIR / "ores.widget.widget.org", _model(block))
    return tmp_path


def test_a_declared_name_resolves(tmp_path, monkeypatch, capsys):
    _point_at(monkeypatch, tmp_path)
    _tree(tmp_path, """\
#+begin_src cpp :name save :implements 11111111-1111-1111-1111-111111111111
        using ores::widget::messaging::widget_id_header;
#+end_src
""")

    assert cps.main() == 0
    assert "resolve" in capsys.readouterr().out


def test_an_undeclared_name_fails(tmp_path, monkeypatch, capsys):
    _point_at(monkeypatch, tmp_path)
    _tree(tmp_path, """\
#+begin_src cpp :name save :implements 11111111-1111-1111-1111-111111111111
        using ores::widget::messaging::no_such_symbol;
#+end_src
""")

    assert cps.main() == 1
    err = capsys.readouterr().err
    assert "ores::widget::messaging::no_such_symbol" in err
    assert "block save" in err
    assert "1 of 1 referenced name(s)" in err


def test_the_same_identifier_in_the_wrong_namespace_fails(
        tmp_path, monkeypatch, capsys):
    # The measured defect. ores.service held the three workflow header names and
    # a rename moved them to ores.workflow.api, while refdata's pasted block
    # still named the old home. The identifier exists; the scope does not.
    _point_at(monkeypatch, tmp_path)
    _tree(tmp_path, """\
#+begin_src cpp :name workflow_override :implements 11111111-1111-1111-1111-111111111111
        using ores::widget::messaging::widget_id_header;
        using ores::widget::other::widget_id_header;
#+end_src
""")

    assert cps.main() == 1
    err = capsys.readouterr().err
    assert "ores::widget::other::widget_id_header" in err
    assert "ores::widget::messaging::widget_id_header" not in err
    assert "1 of 2 referenced name(s)" in err


def test_a_type_member_resolves(tmp_path, monkeypatch, capsys):
    # An enum enumerator and a static member, both reached through a type
    # rather than a namespace.
    _point_at(monkeypatch, tmp_path)
    _tree(tmp_path, """\
#+begin_src cpp :name outcomes :implements 11111111-1111-1111-1111-111111111111
        using ores::widget::messaging::step_outcome::completed;
        using ores::widget::messaging::widget_transfer::system;
#+end_src
""")

    assert cps.main() == 0
    assert "2 name(s)" in capsys.readouterr().out


def test_an_export_macro_does_not_hide_the_type(tmp_path, monkeypatch, capsys):
    _point_at(monkeypatch, tmp_path)
    _tree(tmp_path, """\
#+begin_src cpp :name transfer :implements 11111111-1111-1111-1111-111111111111
        using ores::widget::messaging::widget_transfer;
#+end_src
""")

    assert cps.main() == 0


def test_a_name_inside_a_nested_namespace_resolves(tmp_path, monkeypatch,
                                                   capsys):
    _point_at(monkeypatch, tmp_path)
    _tree(tmp_path, """\
#+begin_src cpp :name detail :implements 11111111-1111-1111-1111-111111111111
        using ores::widget::messaging::detail::helper;
#+end_src
""")

    assert cps.main() == 0


def test_a_tree_with_no_paste_block_fails(tmp_path, monkeypatch, capsys):
    _point_at(monkeypatch, tmp_path)
    _write(tmp_path / HEADER, HEADER_BODY)
    _write(tmp_path / MODEL_DIR / "ores.widget.widget.org",
           _model("#+title: no block here\n"))

    assert cps.main() == 1
    assert "no :implements block" in capsys.readouterr().err


def test_a_block_with_no_qualified_name_fails(tmp_path, monkeypatch, capsys):
    # A check that finds nothing must not pass: an empty pattern would report a
    # clean tree forever.
    _point_at(monkeypatch, tmp_path)
    _tree(tmp_path, """\
#+begin_src cpp :name plain :implements 11111111-1111-1111-1111-111111111111
        int x = 1;
#+end_src
""")

    assert cps.main() == 1
    assert "no qualified name" in capsys.readouterr().err


def test_blocks_are_found_in_any_components_modeling_directory(
        tmp_path, monkeypatch, capsys):
    _point_at(monkeypatch, tmp_path)
    _write(tmp_path / HEADER, HEADER_BODY)
    other = tmp_path / "projects/ores.other/modeling"
    _write(other / "ores.other.thing.org",
           _model("""\
#+begin_src cpp :name other_block :implements 22222222-2222-2222-2222-222222222222
        using ores::widget::messaging::no_such_symbol;
#+end_src
"""))

    assert cps.main() == 1
    err = capsys.readouterr().err
    assert "ores.other.thing.org" in err
    assert "block other_block" in err


def test_a_model_without_the_marker_is_not_read(tmp_path, monkeypatch, capsys):
    # A source block that is not an :implements block is a model's own prose or
    # example, not code that will be pasted, so its names are not this check's.
    _point_at(monkeypatch, tmp_path)
    _write(tmp_path / HEADER, HEADER_BODY)
    _write(tmp_path / MODEL_DIR / "ores.widget.widget.org",
           _model("""\
#+begin_src cpp :name example
        using ores::widget::messaging::no_such_symbol;
#+end_src
"""))

    assert cps.main() == 1
    assert "no :implements block" in capsys.readouterr().err
