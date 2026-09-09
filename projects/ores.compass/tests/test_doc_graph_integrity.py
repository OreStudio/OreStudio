"""
Tests for the doc-graph integrity invariants: which .org files the walker
sees, and the dangling id-link check that guards the corpus.

Run with:  python -m pytest projects/ores.compass/tests/test_doc_graph_integrity.py -v
No live database required.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import doc_index
import compass


def write_doc(path: Path, uuid: str, body: str = "") -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        f":PROPERTIES:\n:ID: {uuid}\n:END:\n"
        f"#+title: {path.stem}\n#+type: knowledge\n\n{body}\n",
        encoding="utf-8",
    )


def test_vendored_external_pruned_but_documentation_external_indexed(tmp_path):
    write_doc(tmp_path / "external" / "vendored.org",
              "11111111-1111-1111-1111-111111111111")
    write_doc(tmp_path / "doc" / "knowledge" / "external" / "quantlib.org",
              "22222222-2222-2222-2222-222222222222")

    found = {p.name for p in doc_index.find_org_files(tmp_path)}
    assert found == {"quantlib.org"}


def test_dangling_links_reported_and_resolved_ones_are_not():
    files = [
        (Path("a.org"), ":ID: AAAAAAAA-0000-0000-0000-000000000000\n"
                        "[[id:BBBBBBBB-0000-0000-0000-000000000000][b]]\n"
                        "[[id:CCCCCCCC-0000-0000-0000-000000000000][gone]]\n"),
        (Path("b.org"), ":ID: BBBBBBBB-0000-0000-0000-000000000000\n"),
    ]
    id_types = compass._collect_org_types(files)

    dangling = compass._lint_dangling_links(files, id_types)

    assert [(path, target) for path, _line, target, _label in dangling] == [
        ("a.org", "CCCCCCCC-0000-0000-0000-000000000000"),
    ]
