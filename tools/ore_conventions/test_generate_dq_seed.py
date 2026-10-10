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
"""Tests for tools/ore_conventions/generate_dq_seed.py.

Run with the project venv:

    ./projects/ores.codegen/venv/bin/python tools/ore_conventions/test_generate_dq_seed.py

Every fixture is a tiny inline canonical TSV and a tiny inline index map, so
the tests never depend on the ORE corpus or on the committed map.
"""

import importlib.util
import tempfile
import unittest
from pathlib import Path

MODULE_PATH = Path(__file__).resolve().parent / "generate_dq_seed.py"
_spec = importlib.util.spec_from_file_location("ore_conventions_generate", MODULE_PATH)
generate_dq_seed = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(generate_dq_seed)

EURIBOR_URI = "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=6M"


def write(path: Path, text: str) -> None:
    path.write_text(text, encoding="utf-8")


class GenerateDqSeedIndexMapTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = Path(self.tmp.name)
        self.tsv = self.root / "conventions-canonical.tsv"
        write(
            self.tsv,
            "kind\tid\tsignature\tvariant_count\tdecided_by\taliases\n"
            f"FRA\tEUR-6M-FRA\tIndex=EUR-EURIBOR-6M\t1\tonly-variant\t\n",
        )
        self.map = self.root / "oresmd_index_map.tsv"

    def tearDown(self):
        self.tmp.cleanup()

    def test_a_name_on_the_map_seeds_its_uri(self):
        write(self.map, f"EUR-EURIBOR-6M\t{EURIBOR_URI}\n")

        seeded, per_kind = generate_dq_seed.generate(self.tsv, self.map)

        self.assertEqual(seeded, ["FRA"])
        (_, values, _), = per_kind["FRA"]
        self.assertEqual(values["oresmd_uri"], f"'{EURIBOR_URI}'")

    def test_a_name_missing_from_the_map_fails_and_names_it(self):
        write(self.map, "USD-SOFR\t" + EURIBOR_URI + "\n")

        with self.assertRaises(generate_dq_seed.GenError) as raised:
            generate_dq_seed.generate(self.tsv, self.map)

        self.assertIn("EUR-EURIBOR-6M", str(raised.exception))
        self.assertIn("FRA", str(raised.exception))

    def test_an_index_kind_seeds_its_own_id_as_the_uri(self):
        write(
            self.tsv,
            "kind\tid\tsignature\tvariant_count\tdecided_by\taliases\n"
            f"IborIndex\tEUR-EURIBOR-6M\t\t1\tonly-variant\t\n",
        )
        write(self.map, f"EUR-EURIBOR-6M\t{EURIBOR_URI}\n")

        _, per_kind = generate_dq_seed.generate(self.tsv, self.map)

        (_, values, _), = per_kind["IborIndex"]
        self.assertEqual(values["oresmd_uri"], f"'{EURIBOR_URI}'")

    def test_a_map_line_that_is_not_a_pair_is_refused(self):
        write(self.map, f"EUR-EURIBOR-6M={EURIBOR_URI}\n")

        with self.assertRaises(generate_dq_seed.GenError) as raised:
            generate_dq_seed.generate(self.tsv, self.map)

        self.assertIn("oresmd_index_map.tsv", str(raised.exception))

    def test_the_dumped_index_names_are_the_ones_the_map_must_cover(self):
        write(self.map, f"EUR-EURIBOR-6M\t{EURIBOR_URI}\n")

        dump = self.root / "index_names.txt"
        rc = generate_dq_seed.main(
            ["--tsv", str(self.tsv), "--dump-index-names", str(dump)]
        )

        self.assertEqual(rc, 0)
        self.assertEqual(dump.read_text(encoding="utf-8"), "EUR-EURIBOR-6M\n")


if __name__ == "__main__":
    unittest.main()
