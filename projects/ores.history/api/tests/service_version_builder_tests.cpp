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
#include "ores.history.api/service/version_builder.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[service][version_builder]");

struct fake_version {
    std::string name;
    std::string modified_by;
    std::chrono::system_clock::time_point recorded_at;
};

std::vector<ores::diff::domain::field_value> render_fake(const fake_version& v) {
    ores::diff::domain::field_value fv;
    fv.name = "name";
    fv.value = v.name;
    return {fv};
}

}

using ores::history::service::build_entity_history_versions;

TEST_CASE("build_entity_history_versions_returns_empty_for_an_empty_input", tags) {
    const auto result = build_entity_history_versions(std::vector<fake_version>{}, render_fake);
    CHECK(result.empty());
}

TEST_CASE("build_entity_history_versions_numbers_versions_descending_from_the_count", tags) {
    const std::vector<fake_version> versions{
        {.name = "v3", .modified_by = "carol", .recorded_at = {}},
        {.name = "v2", .modified_by = "bob", .recorded_at = {}},
        {.name = "v1", .modified_by = "alice", .recorded_at = {}},
    };

    const auto result = build_entity_history_versions(versions, render_fake);

    REQUIRE(result.size() == 3);
    CHECK(result[0].version == 3);
    CHECK(result[0].modified_by == "carol");
    CHECK(result[1].version == 2);
    CHECK(result[2].version == 1);
    CHECK(result[2].modified_by == "alice");
}

TEST_CASE("build_entity_history_versions_leaves_the_oldest_versions_diff_empty", tags) {
    const std::vector<fake_version> versions{
        {.name = "v2", .modified_by = "bob", .recorded_at = {}},
        {.name = "v1", .modified_by = "alice", .recorded_at = {}},
    };

    const auto result = build_entity_history_versions(versions, render_fake);

    REQUIRE(result.size() == 2);
    CHECK(result.back().changes.entries.empty());
}

TEST_CASE("build_entity_history_versions_diffs_each_version_against_the_next_older_one", tags) {
    const std::vector<fake_version> versions{
        {.name = "v2", .modified_by = "bob", .recorded_at = {}},
        {.name = "v1", .modified_by = "alice", .recorded_at = {}},
    };

    const auto result = build_entity_history_versions(versions, render_fake);

    REQUIRE(result.size() == 2);
    REQUIRE(result.front().changes.entries.size() == 1);
    CHECK(result.front().changes.entries.front().field_name == "name");
}

TEST_CASE("build_entity_history_versions_preserves_rendered_fields_per_version", tags) {
    const std::vector<fake_version> versions{
        {.name = "v1", .modified_by = "alice", .recorded_at = {}},
    };

    const auto result = build_entity_history_versions(versions, render_fake);

    REQUIRE(result.size() == 1);
    REQUIRE(result[0].fields.size() == 1);
    CHECK(result[0].fields[0].value == "v1");
}
