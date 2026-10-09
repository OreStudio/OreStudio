#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
"""Tests for tools/ore_conventions/extract.py.

Run with the project venv:

    ./projects/ores.codegen/venv/bin/python tools/ore_conventions/test_extract.py

Every fixture is a small inline XML document, so the tests never depend on the
real ORE corpus.
"""

import contextlib
import importlib.util
import io
import tempfile
import unittest
from pathlib import Path

MODULE_PATH = Path(__file__).resolve().parent / "extract.py"
_spec = importlib.util.spec_from_file_location("ore_conventions_extract", MODULE_PATH)
extract = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(extract)


def doc(*entries: str) -> str:
    return "<Conventions>\n" + "\n".join(entries) + "\n</Conventions>\n"


def write(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


def read_canonical(out_dir: Path) -> dict[tuple[str, str], dict]:
    rows = {}
    lines = (out_dir / "conventions-canonical.tsv").read_text().splitlines()
    header = lines[0].split("\t")
    for line in lines[1:]:
        values = line.split("\t")
        row = dict(zip(header, values))
        rows[(row["kind"], row["id"])] = row
    return rows


def read_alias_rows(out_dir: Path) -> list[list[str]]:
    lines = (out_dir / "conventions-aliases.tsv").read_text().splitlines()
    return [line.split("\t") for line in lines[1:] if line]


class FixtureCase(unittest.TestCase):
    def setUp(self) -> None:
        self._tmp = tempfile.TemporaryDirectory()
        self.root = Path(self._tmp.name)
        self.examples = self.root / "examples"
        self.out = self.root / "out"
        self.decisions = self.root / "decisions.tsv"
        self.decisions.write_text(
            "kind\tid\tdecision\tsignature\n", encoding="utf-8"
        )

    def tearDown(self) -> None:
        self._tmp.cleanup()

    def set_decisions(self, *rows: tuple[str, str, str, str]) -> None:
        lines = ["\t".join(extract.DECISIONS_HEADER)]
        lines.extend("\t".join(row) for row in rows)
        self.decisions.write_text("\n".join(lines) + "\n", encoding="utf-8")

    def run_tool(self) -> extract.RunResult:
        return extract.run(self.examples, self.decisions, self.out)

    def corpus_with_empty_field_outlier(self) -> None:
        """K/X: a 1-file variant with an empty field, a 2-file plain variant."""
        write(
            self.examples / "a" / "conv.xml",
            doc(
                "<K><Id>X</Id><Conventions>CA</Conventions>"
                "<FixingCalendar></FixingCalendar></K>"
            ),
        )
        for name in ("b", "c"):
            write(
                self.examples / name / "conv.xml",
                doc("<K><Id>X</Id><Conventions>CB</Conventions></K>"),
            )

    def corpus_with_suffix_rename(self) -> None:
        """K/FOO and K/FOO-CONVENTIONS carry one identical definition."""
        write(
            self.examples / "a" / "conv.xml",
            doc("<K><Id>FOO</Id><A>1</A></K>"),
        )
        write(
            self.examples / "b" / "conv.xml",
            doc("<K><Id>FOO-CONVENTIONS</Id><A>1</A></K>"),
        )

    def corpus_with_vendor_prefix_twins(self) -> None:
        """K/ICE:ANO and K/PLATTS:ANO are different feeds, not a rename."""
        write(
            self.examples / "a" / "conv.xml",
            doc(
                "<K><Id>ICE:ANO</Id><A>1</A></K>",
                "<K><Id>PLATTS:ANO</Id><A>1</A></K>",
            ),
        )

    def corpus_with_ambiguous_group(self) -> None:
        """K/EUR-DEPOSIT and K/EUR-EURIBOR-CONVENTIONS share a definition."""
        write(
            self.examples / "a" / "conv.xml",
            doc(
                "<K><Id>EUR-DEPOSIT</Id><A>1</A></K>",
                "<K><Id>EUR-EURIBOR-CONVENTIONS</Id><A>1</A></K>",
            ),
        )


class TestNonEmptyFieldFix(FixtureCase):
    def test_empty_field_does_not_win_on_completeness(self) -> None:
        self.corpus_with_empty_field_outlier()
        result = self.run_tool()
        row = read_canonical(self.out)[("K", "X")]

        # The old rule counted present-but-empty FixingCalendar, so the
        # 1-file variant won on 2 fields. The corrected rule counts only
        # non-empty fields, ties, and then picks the plurality by files.
        self.assertEqual(row["signature"], "Conventions=CB")
        self.assertEqual(row["decided_by"], "rule-2")
        self.assertEqual(row["source_path"], "b/conv.xml")
        self.assertEqual(row["variant_count"], "2")
        self.assertEqual(result.flagged, 0)


class TestPinnedDecision(FixtureCase):
    def test_pin_is_honoured_verbatim(self) -> None:
        self.corpus_with_empty_field_outlier()
        winner_signature = "Conventions=CA ; FixingCalendar="
        self.set_decisions(
            ("K", "X", extract.digest_of(winner_signature), winner_signature)
        )
        result = self.run_tool()
        row = read_canonical(self.out)[("K", "X")]

        self.assertEqual(row["signature"], winner_signature)
        self.assertEqual(row["decided_by"], "pinned")
        self.assertEqual(result.pinned, 1)
        self.assertEqual(result.flagged, 0)
        self.assertEqual(result.stale, 0)

    def test_pin_by_full_signature(self) -> None:
        self.corpus_with_empty_field_outlier()
        winner_signature = "Conventions=CA ; FixingCalendar="
        self.set_decisions(("K", "X", winner_signature, winner_signature))
        self.run_tool()
        row = read_canonical(self.out)[("K", "X")]
        self.assertEqual(row["decided_by"], "pinned")

    def test_todo_does_not_pin(self) -> None:
        self.corpus_with_empty_field_outlier()
        self.set_decisions(("K", "X", "TODO", ""))
        result = self.run_tool()
        row = read_canonical(self.out)[("K", "X")]
        self.assertEqual(row["decided_by"], "rule-2")
        self.assertEqual(result.todo, 1)
        self.assertEqual(result.pinned, 0)


class TestStalePin(FixtureCase):
    def test_stale_pin_is_reported_and_retained(self) -> None:
        self.corpus_with_empty_field_outlier()
        gone = "Conventions=GONE"
        self.set_decisions(("K", "X", extract.digest_of(gone), gone))

        stdout = io.StringIO()
        with contextlib.redirect_stdout(stdout):
            code = extract.main(
                [
                    "--examples",
                    str(self.examples),
                    "--decisions",
                    str(self.decisions),
                    "--out",
                    str(self.out),
                ]
            )
        result = extract.run(self.examples, self.decisions, self.out)
        row = read_canonical(self.out)[("K", "X")]

        self.assertEqual(code, 0)
        self.assertEqual(result.stale, 1)
        self.assertEqual(result.pinned, 0)
        # The pin is kept, not dropped, and it outlives the corpus match.
        self.assertEqual(row["signature"], gone)
        self.assertEqual(row["source_path"], extract.STALE_SOURCE)
        self.assertIn("STALE pin", stdout.getvalue())
        self.assertIn("K / X", stdout.getvalue())

        audit = (self.out / "conventions-resolutions.md").read_text()
        self.assertIn("## Stale pins", audit)
        self.assertIn("Conventions=GONE", audit)
        review = (self.out / "conventions-review.tsv").read_text()
        self.assertIn("STALE pin", review)

    def test_pin_for_absent_id_is_stale_and_retained(self) -> None:
        self.corpus_with_empty_field_outlier()
        gone = "Conventions=VANISHED"
        self.set_decisions(("K", "MISSING", extract.digest_of(gone), gone))
        result = self.run_tool()

        self.assertEqual(result.stale, 1)
        self.assertEqual(result.canonical_ids, 2)
        row = read_canonical(self.out)[("K", "MISSING")]
        self.assertEqual(row["signature"], gone)
        self.assertEqual(row["decided_by"], "pinned")
        self.assertEqual(row["variant_count"], "0")


class TestFlaggedNotSilentlyResolved(FixtureCase):
    def corpus_with_forced_tie(self) -> None:
        """K/Y: two non-legacy variants tie on fields and files -> rule-4."""
        write(
            self.examples / "a" / "conv.xml",
            doc("<K><Id>Y</Id><A>1</A></K>", "<K><Id>Z</Id><B>only</B></K>"),
        )
        write(
            self.examples / "b" / "conv.xml",
            doc("<K><Id>Y</Id><A>2</A></K>"),
        )

    def test_flagged_id_is_kept_and_listed(self) -> None:
        self.corpus_with_forced_tie()
        result = self.run_tool()
        rows = read_canonical(self.out)

        self.assertIn(("K", "Y"), rows)
        row = rows[("K", "Y")]
        self.assertIn(row["decided_by"], {"rule-3", "rule-4"})
        self.assertIn(row["signature"], {"A=1", "A=2"})
        self.assertEqual(result.flagged, 1)

        review = (self.out / "conventions-review.tsv").read_text().splitlines()
        review_rows = [line for line in review[1:] if line]
        self.assertEqual(len(review_rows), 1)
        fields = review_rows[0].split("\t")
        self.assertEqual(fields[0], "K")
        self.assertEqual(fields[1], "Y")
        self.assertEqual(fields[2], "2")
        self.assertIn("rule-4", fields[5])

        audit = (self.out / "conventions-resolutions.md").read_text()
        self.assertIn("FLAGGED FOR REVIEW", audit)
        self.assertIn("### K / Y", audit)

    def test_only_variant_is_not_flagged(self) -> None:
        self.corpus_with_forced_tie()
        self.run_tool()
        row = read_canonical(self.out)[("K", "Z")]
        self.assertEqual(row["decided_by"], "only-variant")
        self.assertEqual(row["variant_count"], "1")


class TestPatternASuffixRename(FixtureCase):
    def test_rename_resolves_to_conventions_spelling(self) -> None:
        self.corpus_with_suffix_rename()
        result = self.run_tool()
        rows = read_canonical(self.out)

        self.assertNotIn(("K", "FOO"), rows)
        row = rows[("K", "FOO-CONVENTIONS")]
        self.assertEqual(row["signature"], "A=1")
        self.assertEqual(row["decided_by"], "renamed")
        self.assertEqual(row["variant_count"], "1")
        self.assertEqual(row["aliases"], "FOO")
        self.assertEqual(result.pattern_a, 1)
        self.assertEqual(result.aliases, 1)
        self.assertEqual(result.canonical_ids, 1)

    def test_alias_is_recorded_once_and_not_duplicated(self) -> None:
        self.corpus_with_suffix_rename()
        self.run_tool()

        alias_rows = read_alias_rows(self.out)
        self.assertEqual(alias_rows, [["K", "FOO", "FOO-CONVENTIONS", "A"]])
        canonical_ids = [
            line.split("\t")[1]
            for line in (
                self.out / "conventions-canonical.tsv"
            ).read_text().splitlines()[1:]
        ]
        self.assertEqual(canonical_ids.count("FOO"), 0)
        self.assertEqual(canonical_ids.count("FOO-CONVENTIONS"), 1)


class TestPatternBVendorPrefix(FixtureCase):
    def test_vendor_prefix_twins_are_not_merged(self) -> None:
        self.corpus_with_vendor_prefix_twins()
        result = self.run_tool()
        rows = read_canonical(self.out)

        self.assertIn(("K", "ICE:ANO"), rows)
        self.assertIn(("K", "PLATTS:ANO"), rows)
        self.assertEqual(rows[("K", "ICE:ANO")]["aliases"], "")
        self.assertEqual(rows[("K", "PLATTS:ANO")]["aliases"], "")
        self.assertEqual(read_alias_rows(self.out), [])
        self.assertEqual(result.pattern_b, 1)
        self.assertEqual(result.pattern_a, 0)
        self.assertEqual(result.aliases, 0)

        audit = (self.out / "conventions-resolutions.md").read_text()
        self.assertIn("Pattern B — vendor/exchange prefix twins", audit)


class TestPatternCFlagged(FixtureCase):
    def test_ambiguous_multi_stem_group_is_flagged_not_merged(self) -> None:
        self.corpus_with_ambiguous_group()
        result = self.run_tool()
        rows = read_canonical(self.out)

        self.assertIn(("K", "EUR-DEPOSIT"), rows)
        self.assertIn(("K", "EUR-EURIBOR-CONVENTIONS"), rows)
        self.assertEqual(rows[("K", "EUR-DEPOSIT")]["decided_by"], "only-variant")
        self.assertEqual(read_alias_rows(self.out), [])
        self.assertEqual(result.pattern_c, 1)
        self.assertEqual(result.aliases, 0)

        review = (self.out / "conventions-review.tsv").read_text()
        self.assertIn("pattern-C", review)
        self.assertIn("EUR-DEPOSIT", review)
        self.assertIn("EUR-EURIBOR-CONVENTIONS", review)

        audit = (self.out / "conventions-resolutions.md").read_text()
        self.assertIn("Pattern C — unrecognised, flagged for a human", audit)


class TestPinByAlias(FixtureCase):
    def test_pin_via_alias_resolves_instead_of_going_stale(self) -> None:
        self.corpus_with_suffix_rename()
        self.set_decisions(("K", "FOO", extract.digest_of("A=1"), "A=1"))
        result = self.run_tool()
        rows = read_canonical(self.out)

        self.assertEqual(result.stale, 0)
        self.assertEqual(result.pinned, 1)
        row = rows[("K", "FOO-CONVENTIONS")]
        self.assertEqual(row["decided_by"], "pinned")
        self.assertEqual(row["signature"], "A=1")
        self.assertEqual(row["aliases"], "FOO")
        self.assertNotIn(("K", "FOO"), rows)


class TestDeterminism(FixtureCase):
    def test_two_runs_are_byte_identical(self) -> None:
        write(
            self.examples / "a" / "conv.xml",
            doc(
                "<K><Id>X</Id><A>1</A><B>1</B></K>",
                "<K><Id>Y</Id><A>1</A><B>1</B><C>1</C></K>",
            ),
        )
        write(
            self.examples / "b" / "conv.xml",
            doc("<K><Id>X</Id><A>1</A><B>2</B></K>", "<K><Id>W</Id><A>1</A></K>"),
        )
        write(
            self.examples / "c" / "conv.xml",
            doc("<K><Id>W</Id><A>1</A></K>", "<K><Id>W</Id><A>2</A></K>"),
        )
        first = self.root / "out1"
        second = self.root / "out2"
        extract.run(self.examples, self.decisions, first)
        extract.run(self.examples, self.decisions, second)

        for name in (
            "conventions-canonical.tsv",
            "conventions-resolutions.md",
            "conventions-review.tsv",
            "conventions-aliases.tsv",
        ):
            self.assertEqual(
                (first / name).read_bytes(),
                (second / name).read_bytes(),
                f"{name} differs between runs",
            )


class TestDecisionsContract(FixtureCase):
    def test_bad_decisions_file_is_a_real_failure(self) -> None:
        self.corpus_with_empty_field_outlier()
        self.decisions.write_text("not\ta\tvalid\theader\n", encoding="utf-8")
        stderr = io.StringIO()
        with contextlib.redirect_stderr(stderr):
            code = extract.main(
                [
                    "--examples",
                    str(self.examples),
                    "--decisions",
                    str(self.decisions),
                    "--out",
                    str(self.out),
                ]
            )
        self.assertNotEqual(code, 0)
        self.assertIn("error:", stderr.getvalue())

    def test_missing_examples_path_fails(self) -> None:
        code = extract.main(
            [
                "--examples",
                str(self.root / "nope"),
                "--decisions",
                str(self.decisions),
                "--out",
                str(self.out),
            ]
        )
        self.assertNotEqual(code, 0)

    def test_duplicate_decision_row_fails(self) -> None:
        self.set_decisions(("K", "X", "TODO", ""), ("K", "X", "TODO", ""))
        with self.assertRaises(extract.DecisionError):
            self.run_tool()


if __name__ == "__main__":
    unittest.main(verbosity=2)
