#!/usr/bin/env python3
# -*- mode: python; tab-width: 4; indent-tabs-mode: nil -*-
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
"""Tests of how a soak wave is shared out among the parties of a tenant."""

import random
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

import grid_soak  # noqa: E402

PARTIES = ["a", "b", "c", "d"]


def test_the_shares_add_up_to_the_wave_and_every_party_has_a_job():
    rng = random.Random(1)
    for count in (4, 5, 17, 120):
        parts = grid_soak.split_count(rng, count, PARTIES)
        assert sum(jobs for _, jobs in parts) == count
        assert {party for party, _ in parts} == set(PARTIES)
        assert all(jobs >= 1 for _, jobs in parts)


def test_a_wave_smaller_than_the_parties_reaches_a_subset_with_a_job_each():
    parts = grid_soak.split_count(random.Random(2), 2, PARTIES)
    assert len(parts) == 2
    assert [jobs for _, jobs in parts] == [1, 1]
    assert len({party for party, _ in parts}) == 2


def test_no_parties_or_no_jobs_share_nothing():
    assert grid_soak.split_count(random.Random(3), 10, []) == []
    assert grid_soak.split_count(random.Random(3), 0, PARTIES) == []


def test_over_many_waves_every_party_gets_about_a_quarter():
    rng = random.Random(4)
    totals = dict.fromkeys(PARTIES, 0)
    for _ in range(400):
        for party, jobs in grid_soak.split_count(rng, 50, PARTIES):
            totals[party] += jobs
    grand = sum(totals.values())
    for party in PARTIES:
        assert 0.22 < totals[party] / grand < 0.28


def test_the_script_switches_party_before_each_batch_and_names_each_batch():
    script = grid_soak.wave_script("u@t", "pw", "ref", [("p1", 3), ("p2", 4)], "AV", "PKG")
    lines = script.splitlines()
    assert lines.index("account_operations switch-party p1") < lines.index(
        'batches add ref-p1 open system.new_record "grid soak"'
    )
    assert "compute dispatch-batch ref-p2 4 AV PKG" in lines
    assert lines.index("account_operations switch-party p2") > lines.index(
        "compute dispatch-batch ref-p1 3 AV PKG"
    )


def test_a_wave_with_no_party_keeps_the_login_party_and_the_plain_reference():
    script = grid_soak.wave_script("u@t", "pw", "ref", [(None, 9)], "AV", "PKG")
    assert "switch-party" not in script
    assert "compute dispatch-batch ref 9 AV PKG" in script.splitlines()


def test_a_heavier_party_takes_more_of_every_wave_on_average():
    rng = random.Random(5)
    totals = dict.fromkeys(PARTIES, 0)
    for _ in range(400):
        for party, jobs in grid_soak.split_count(rng, 50, PARTIES, [5.0, 3.0, 2.0, 0.5]):
            totals[party] += jobs
    ordered = sorted(PARTIES, key=lambda party: -totals[party])
    assert ordered == ["a", "b", "c", "d"]
    grand = sum(totals.values())
    assert totals["a"] / grand > 0.4
    assert totals["d"] / grand < 0.1


def test_a_small_wave_reaches_the_heavy_parties_more_often():
    rng = random.Random(6)
    seen = dict.fromkeys(PARTIES, 0)
    for _ in range(600):
        for party, _ in grid_soak.split_count(rng, 1, PARTIES, [5.0, 3.0, 2.0, 0.5]):
            seen[party] += 1
    assert seen["a"] > seen["b"] > seen["c"] > seen["d"]


def test_party_weights_default_to_the_acme_shape_and_can_be_overridden():
    weights = grid_soak.parse_party_weights(["ACMCOR=4"])
    assert weights["ACCOUK"] == 5.0
    assert weights["ACMCOR"] == 4.0


def test_a_weight_that_is_not_a_finite_positive_number_is_refused():
    import pytest

    for bad in ("X=nan", "X=inf", "X=0", "X=-1", "X=heavy"):
        with pytest.raises(SystemExit):
            grid_soak.parse_party_weights([bad])
