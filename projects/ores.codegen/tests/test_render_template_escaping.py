"""The generator emits SQL, C++ and org, so a rendered value stays verbatim.

Mustache escapes ``{{value}}`` for HTML by default. That is wrong here: a
description carrying an apostrophe or an ampersand would reach the generated
SQL as an HTML entity, and the row would be stored corrupted.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_render_template_escaping.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import render_template  # noqa: E402


def test_a_value_carrying_an_apostrophe_is_not_html_escaped(tmp_path):
    template = tmp_path / "sql_demo_populate.mustache"
    template.write_text("'{{description}}'", encoding="utf-8")

    rendered = render_template(
        template, {"description": "Flag of Cote d'Ivoire & dependencies"})

    assert rendered == "'Flag of Cote d'Ivoire & dependencies'"
