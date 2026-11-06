"""The embedded catalogue must fit MSVC literals without changing its data."""
import importlib.util
from pathlib import Path
import re

import pytest

spec = importlib.util.spec_from_file_location(
    "catalogue_generator", Path(__file__).resolve().parents[1] / "generate_output_catalogue.py")
generator = importlib.util.module_from_spec(spec)
spec.loader.exec_module(generator)


@pytest.mark.parametrize("text", ["", "x" * 90000, "😀" * 12000,
                                 'first\n' + 'column,' * 8000 + '\nlast\n',
                                 'a raw delimiter: )CAT"'])
def test_large_and_unicode_tables_round_trip_in_bounded_literals(text):
    literals = generator.raw_string_literals(text)
    parts = []
    for literal in literals:
        match = re.fullmatch(r'R"([^ ()\\\t\r\n]{1,16})\((.*)\)\1"', literal, re.DOTALL)
        assert match is not None
        part = match[2]
        assert len(part.encode("utf-8")) <= 16380
        assert f'){match[1]}"' not in part
        parts.append(part)
    assert "".join(parts) == text
