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
#include "ores.diff/engine/compare.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[engine]");

}

using ores::diff::domain::field_value;
using ores::diff::engine::compute;

TEST_CASE("compare_identical_inputs_yields_empty_result", tags) {
    const std::vector<field_value> fields{{.name = "ISO Code", .value = "USD"},
                                          {.name = "Name", .value = "US Dollar"}};

    const auto result = compute(fields, fields);

    CHECK(result.entries.empty());
}

TEST_CASE("compare_both_lists_empty_yields_empty_result", tags) {
    const auto result = compute({}, {});

    CHECK(result.entries.empty());
}

TEST_CASE("compare_changed_value_yields_entry_with_both_sides", tags) {
    const std::vector<field_value> previous{{.name = "Name", .value = "US Dollar"}};
    const std::vector<field_value> current{{.name = "Name", .value = "United States Dollar"}};

    const auto result = compute(previous, current);

    REQUIRE(result.entries.size() == 1);
    CHECK(result.entries[0].field_name == "Name");
    CHECK(result.entries[0].old_value == "US Dollar");
    CHECK(result.entries[0].new_value == "United States Dollar");
}

TEST_CASE("compare_unchanged_fields_are_omitted", tags) {
    const std::vector<field_value> previous{{.name = "ISO Code", .value = "USD"},
                                            {.name = "Name", .value = "US Dollar"},
                                            {.name = "Symbol", .value = "$"}};
    const std::vector<field_value> current{{.name = "ISO Code", .value = "USD"},
                                           {.name = "Name", .value = "United States Dollar"},
                                           {.name = "Symbol", .value = "$"}};

    const auto result = compute(previous, current);

    REQUIRE(result.entries.size() == 1);
    CHECK(result.entries[0].field_name == "Name");
}

TEST_CASE("compare_added_field_has_empty_old_value", tags) {
    const std::vector<field_value> previous{{.name = "ISO Code", .value = "USD"}};
    const std::vector<field_value> current{{.name = "ISO Code", .value = "USD"},
                                           {.name = "Market Tier", .value = "Major"}};

    const auto result = compute(previous, current);

    REQUIRE(result.entries.size() == 1);
    CHECK(result.entries[0].field_name == "Market Tier");
    CHECK(result.entries[0].old_value == "");
    CHECK(result.entries[0].new_value == "Major");
}

TEST_CASE("compare_removed_field_has_empty_new_value", tags) {
    const std::vector<field_value> previous{{.name = "ISO Code", .value = "USD"},
                                            {.name = "Legacy Flag", .value = "true"}};
    const std::vector<field_value> current{{.name = "ISO Code", .value = "USD"}};

    const auto result = compute(previous, current);

    REQUIRE(result.entries.size() == 1);
    CHECK(result.entries[0].field_name == "Legacy Flag");
    CHECK(result.entries[0].old_value == "true");
    CHECK(result.entries[0].new_value == "");
}

TEST_CASE("compare_previous_empty_reports_all_fields_added", tags) {
    const std::vector<field_value> current{{.name = "ISO Code", .value = "USD"},
                                           {.name = "Name", .value = "US Dollar"}};

    const auto result = compute({}, current);

    REQUIRE(result.entries.size() == 2);
    CHECK(result.entries[0].field_name == "ISO Code");
    CHECK(result.entries[0].old_value == "");
    CHECK(result.entries[0].new_value == "USD");
    CHECK(result.entries[1].field_name == "Name");
    CHECK(result.entries[1].old_value == "");
    CHECK(result.entries[1].new_value == "US Dollar");
}

TEST_CASE("compare_current_empty_reports_all_fields_removed", tags) {
    const std::vector<field_value> previous{{.name = "ISO Code", .value = "USD"},
                                            {.name = "Name", .value = "US Dollar"}};

    const auto result = compute(previous, {});

    REQUIRE(result.entries.size() == 2);
    CHECK(result.entries[0].field_name == "ISO Code");
    CHECK(result.entries[0].old_value == "USD");
    CHECK(result.entries[0].new_value == "");
    CHECK(result.entries[1].field_name == "Name");
    CHECK(result.entries[1].old_value == "US Dollar");
    CHECK(result.entries[1].new_value == "");
}

TEST_CASE("compare_preserves_current_order_for_changes", tags) {
    // Previous deliberately lists fields in a different order; the
    // output must follow the current (mapper) order.
    const std::vector<field_value> previous{
        {.name = "C", .value = "3"}, {.name = "A", .value = "1"}, {.name = "B", .value = "2"}};
    const std::vector<field_value> current{{.name = "A", .value = "one"},
                                           {.name = "B", .value = "two"},
                                           {.name = "C", .value = "three"}};

    const auto result = compute(previous, current);

    REQUIRE(result.entries.size() == 3);
    CHECK(result.entries[0].field_name == "A");
    CHECK(result.entries[1].field_name == "B");
    CHECK(result.entries[2].field_name == "C");
}

TEST_CASE("compare_removed_fields_follow_in_previous_order", tags) {
    const std::vector<field_value> previous{{.name = "Kept", .value = "old"},
                                            {.name = "Removed Last", .value = "x"},
                                            {.name = "Removed First", .value = "y"}};
    const std::vector<field_value> current{{.name = "Added", .value = "new"},
                                           {.name = "Kept", .value = "new"}};

    const auto result = compute(previous, current);

    // Current order first (Added, Kept), then removed fields in the
    // previous list's order.
    REQUIRE(result.entries.size() == 4);
    CHECK(result.entries[0].field_name == "Added");
    CHECK(result.entries[1].field_name == "Kept");
    CHECK(result.entries[2].field_name == "Removed Last");
    CHECK(result.entries[3].field_name == "Removed First");
}

TEST_CASE("compare_empty_values_on_both_sides_are_equal", tags) {
    const std::vector<field_value> previous{{.name = "Notes", .value = ""}};
    const std::vector<field_value> current{{.name = "Notes", .value = ""}};

    const auto result = compute(previous, current);

    CHECK(result.entries.empty());
}

TEST_CASE("compare_duplicate_names_first_occurrence_wins", tags) {
    const std::vector<field_value> previous{{.name = "Name", .value = "first"},
                                            {.name = "Name", .value = "second"}};
    const std::vector<field_value> current{{.name = "Name", .value = "changed"},
                                           {.name = "Name", .value = "ignored"}};

    const auto result = compute(previous, current);

    REQUIRE(result.entries.size() == 1);
    CHECK(result.entries[0].field_name == "Name");
    CHECK(result.entries[0].old_value == "first");
    CHECK(result.entries[0].new_value == "changed");
}
