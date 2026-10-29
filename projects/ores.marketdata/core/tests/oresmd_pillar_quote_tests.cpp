/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
/**
 * @file oresmd_pillar_quote_tests.cpp
 * @brief Which rung of a series a curve pillar reads: its term.
 *
 * The expectations are the text a reader compares an observation's datum term
 * against, written out, so a change to the term a pillar reads is a change to
 * this file and not a silent one. No case here reads a database, NATS or a feed.
 */
#include "ores.marketdata.core/oresmd/pillar_quote.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <stdexcept>
#include <string>

namespace {

using ores::marketdata::core::pillar_term_text;

const auto first_day = std::chrono::year{2026} / 1 / 1;
const auto later_day = std::chrono::year{2026} / 3 / 2;

}

TEST_CASE("the_term_of_a_pillar_that_names_a_period_is_that_period", "[pillar_quote]") {
    // A spot-starting pillar and one that starts at a period both take the tenor
    // their template names, so the quote is the same on every day it is read.
    CHECK(pillar_term_text("SPOT", "1M", first_day, later_day) == "1M");
    CHECK(pillar_term_text("3M", "6M", first_day, later_day) == "6M");
    CHECK(pillar_term_text("SPOT", "2Y", first_day, later_day) == "2Y");
}

TEST_CASE("the_term_of_a_meeting_dated_pillar_is_the_date_its_end_resolves_to",
          "[pillar_quote]") {
    // A meeting code names no period, so the term is the date the conventions
    // resolved the pillar's end to.
    CHECK(pillar_term_text("1F", "2F", first_day, later_day) == "20260302");
    CHECK(pillar_term_text("SPOT", "", first_day, later_day) == "60D");
}

TEST_CASE("a_spot_pillar_whose_template_names_no_end_counts_the_days_it_spans",
          "[pillar_quote]") {
    const auto end =
        std::chrono::year_month_day{std::chrono::sys_days{first_day} + std::chrono::days{50}};
    CHECK(pillar_term_text("SPOT", "", first_day, end) == "50D");
    CHECK_THROWS_AS(pillar_term_text("SPOT", "", later_day, first_day), std::invalid_argument);
}
