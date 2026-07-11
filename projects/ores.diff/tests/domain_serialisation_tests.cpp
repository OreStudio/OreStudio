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
#include "ores.diff/domain/diff_result.hpp"
#include "ores.diff/domain/field_value.hpp"
#include <catch2/catch_test_macros.hpp>
#include <rfl/json.hpp>

namespace {

const std::string tags("[domain]");

}

using ores::diff::domain::diff_entry;
using ores::diff::domain::diff_result;
using ores::diff::domain::field_value;

TEST_CASE("diff_result_round_trips_through_rfl_json", tags) {
    const diff_result original{
        .entries = {
            {.field_name = "Name", .old_value = "US Dollar", .new_value = "United States Dollar"},
            {.field_name = "Market Tier", .old_value = "", .new_value = "Major"}}};

    const auto json = rfl::json::write(original);
    const auto restored = rfl::json::read<diff_result>(json);

    REQUIRE(restored);
    CHECK(*restored == original);
}

TEST_CASE("field_value_round_trips_through_rfl_json", tags) {
    const field_value original{.name = "ISO Code", .value = "USD"};

    const auto json = rfl::json::write(original);
    const auto restored = rfl::json::read<field_value>(json);

    REQUIRE(restored);
    CHECK(*restored == original);
}

TEST_CASE("diff_result_with_populated_spans_round_trips_through_rfl_json", tags) {
    const diff_result original{
        .entries = {{.field_name = "Year",
                    .old_value = "2025",
                    .new_value = "2026",
                    .old_spans = {{.offset = 3, .length = 1}},
                    .new_spans = {{.offset = 3, .length = 1}}}}};

    const auto json = rfl::json::write(original);
    const auto restored = rfl::json::read<diff_result>(json);

    REQUIRE(restored);
    CHECK(*restored == original);
    REQUIRE(restored->entries[0].old_spans.size() == 1);
    CHECK(restored->entries[0].old_spans[0].offset == 3);
    CHECK(restored->entries[0].old_spans[0].length == 1);
}
