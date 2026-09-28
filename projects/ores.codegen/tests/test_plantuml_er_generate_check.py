"""Tests for plantuml_er_generate.py --check.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_plantuml_er_generate_check.py

The check is the generator dry run the component clean standard's G07 item
asks for: a verdict a CI step can trust without writing the artefact. It is
only worth trusting if it fails on a stale diagram and passes on a current
one, so both halves are pinned here, along with the promise that --check
never rewrites the file it judged. Every path is under tmp_path; the real
projects/ores.sql/modeling/ores_schema.puml is never read or written.
"""
import json
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

import plantuml_er_generate as peg  # noqa: E402

TITLE = "ORES Database Schema"
# What the one-line template below renders from the model above.
CURRENT = f"@startuml\n{TITLE}\n@enduml\n"
TEMPLATE = "@startuml\n{{title}}\n@enduml\n"
# The real template stamps the render with the time it ran, so the check has
# to discount that one line or it can never pass. This template reproduces it.
STAMPED_TEMPLATE = "@startuml\n' Generated: {{generated_at}}\n{{title}}\n@enduml\n"


def _run_check(tmp_path: Path, committed: str | None, template: str = TEMPLATE,
               model: dict | None = None):
    """Run --check over a throw-away model and template; return (rc, output)."""
    model_path = tmp_path / "model.json"
    model_path.write_text(json.dumps(model or {"title": TITLE}), encoding="utf-8")
    template_path = tmp_path / "er.mustache"
    template_path.write_text(template, encoding="utf-8")
    output = tmp_path / "ores_schema.puml"
    if committed is not None:
        output.write_text(committed, encoding="utf-8")
    rc = peg.main(["--model", str(model_path), "--template", str(template_path),
                   "--output", str(output), "--check"])
    return rc, output


def test_check_ignores_the_generation_stamp(tmp_path):
    """A fresh render stamps the time it ran, so the stamp never matches."""
    model = {"title": TITLE, "generated_at": "2026-09-28T12:00:00Z"}
    committed = f"@startuml\n' Generated: 2020-01-01T00:00:00Z\n{TITLE}\n@enduml\n"
    rc, _ = _run_check(tmp_path, committed, STAMPED_TEMPLATE, model)
    assert rc == 0


def test_check_fails_when_only_the_body_changed(tmp_path):
    """Discounting the stamp must not discount the diagram itself."""
    model = {"title": TITLE, "generated_at": "2026-09-28T12:00:00Z"}
    committed = f"@startuml\n' Generated: 2026-09-28T12:00:00Z\nDIFFERENT\n@enduml\n"
    rc, _ = _run_check(tmp_path, committed, STAMPED_TEMPLATE, model)
    assert rc == 1


def test_check_passes_when_the_artefact_matches(tmp_path):
    rc, _ = _run_check(tmp_path, CURRENT)
    assert rc == 0


def test_check_fails_when_the_artefact_has_drifted(tmp_path, capsys):
    rc, output = _run_check(tmp_path, "@startuml\nSTALE\n@enduml\n")
    err = capsys.readouterr().err
    assert rc == 1
    assert "stale ER diagram" in err
    assert str(output) in err
    assert "1 line differs" in err


def test_check_never_rewrites_the_artefact_it_judged(tmp_path, capsys):
    stale = "@startuml\nSTALE\n@enduml\n"
    rc, output = _run_check(tmp_path, stale)
    capsys.readouterr()
    assert rc == 1
    assert output.read_text(encoding="utf-8") == stale


def test_check_reports_a_missing_artefact(tmp_path, capsys):
    rc, output = _run_check(tmp_path, None)
    err = capsys.readouterr().err
    assert rc == 1
    assert str(output) in err
    assert "does not exist" in err
