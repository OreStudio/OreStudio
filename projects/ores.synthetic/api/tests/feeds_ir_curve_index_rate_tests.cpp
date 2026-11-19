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
#include "ores.synthetic.api/feeds/ir_curve_template_resolver.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

namespace {

const std::string index_rate_tags("[feeds][ir_curve][index_rate]");

using namespace ores::synthetic::feed;

const std::chrono::year_month_day spot{
    std::chrono::year{2026}, std::chrono::November, std::chrono::day{16}};

ir_curve_resolved_entry
entry(const std::string& role, const std::string& start_tenor, const std::string& end_tenor) {
    return {.sequence_index = 0,
            .point_id = end_tenor,
            .start_tenor_code = start_tenor,
            .curve_role = role,
            .ticks_ahead_start = 0,
            .ticks_ahead_end = 1,
            .year_fraction = 0.25,
            .fixed_leg_schedule = {},
            .start_date = spot,
            .end_date = spot};
}

}

TEST_CASE("the_index_rate_is_the_deposit_to_the_index_tenor", index_rate_tags) {
    const std::vector<ir_curve_resolved_entry> entries{entry("DEPOSIT", "SPOT", "3M"),
                                                       entry("FRA", "3M", "6M")};
    const auto at = index_entry_of(entries, "3M");
    REQUIRE(at.has_value());
    CHECK(*at == 0);
}

TEST_CASE("an_overnight_curve_states_its_index_at_the_overnight_deposit", index_rate_tags) {
    const std::vector<ir_curve_resolved_entry> entries{entry("DEPOSIT", "SPOT", "1D"),
                                                       entry("DEPOSIT", "1D", "1W")};
    const auto at = index_entry_of(entries, "1D");
    REQUIRE(at.has_value());
    CHECK(*at == 0);
}

TEST_CASE("an_agreement_that_ends_on_the_index_tenor_states_its_rate", index_rate_tags) {
    const std::vector<ir_curve_resolved_entry> entries{
        entry("DEPOSIT", "SPOT", "1M"), entry("DEPOSIT", "SPOT", "3M"), entry("FRA", "3M", "6M")};
    CHECK(index_entry_of(entries, "6M").value() == 2);
}

TEST_CASE("a_deposit_states_the_index_rate_ahead_of_an_agreement_on_the_same_tenor",
          index_rate_tags) {
    const std::vector<ir_curve_resolved_entry> entries{entry("FRA", "SPOT", "3M"),
                                                       entry("DEPOSIT", "SPOT", "3M")};
    CHECK(index_entry_of(entries, "3M").value() == 1);
}

TEST_CASE("a_curve_that_ends_no_entry_on_the_index_tenor_states_no_index_rate", index_rate_tags) {
    const std::vector<ir_curve_resolved_entry> entries{entry("DEPOSIT", "SPOT", "1M"),
                                                       entry("IRS", "SPOT", "2Y")};
    CHECK_FALSE(index_entry_of(entries, "3M").has_value());
    CHECK(index_entry_of(entries, "1M").value() == 0);
}
