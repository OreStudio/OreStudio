/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.dq/domain/publication_mode.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[domain]");

using namespace ores::dq::domain;

}

TEST_CASE("to_string_converts_all_publication_modes", tags) {
    CHECK(to_string(publication_mode::upsert) == "upsert");
    CHECK(to_string(publication_mode::insert_only) == "insert_only");
    CHECK(to_string(publication_mode::replace_all) == "replace_all");
}

TEST_CASE("publication_mode_from_string_converts_all_valid_strings", tags) {
    auto upsert = publication_mode_from_string("upsert");
    REQUIRE(upsert.has_value());
    CHECK(*upsert == publication_mode::upsert);

    auto insert_only = publication_mode_from_string("insert_only");
    REQUIRE(insert_only.has_value());
    CHECK(*insert_only == publication_mode::insert_only);

    auto replace_all = publication_mode_from_string("replace_all");
    REQUIRE(replace_all.has_value());
    CHECK(*replace_all == publication_mode::replace_all);
}

TEST_CASE("publication_mode_from_string_returns_nullopt_for_invalid", tags) {
    CHECK(!publication_mode_from_string("").has_value());
    CHECK(!publication_mode_from_string("invalid").has_value());
    CHECK(!publication_mode_from_string("UPSERT").has_value());
    CHECK(!publication_mode_from_string("Upsert").has_value());
}

TEST_CASE("publication_mode_round_trips_through_string", tags) {
    auto modes = {
        publication_mode::upsert,
        publication_mode::insert_only,
        publication_mode::replace_all
    };

    for (auto mode : modes) {
        auto str = to_string(mode);
        auto parsed = publication_mode_from_string(str);
        REQUIRE(parsed.has_value());
        CHECK(*parsed == mode);
    }
}

TEST_CASE("publication_mode_stream_insertion_matches_to_string", tags) {
    auto modes = {
        publication_mode::upsert,
        publication_mode::insert_only,
        publication_mode::replace_all
    };

    for (auto mode : modes) {
        std::ostringstream ss;
        ss << mode;
        CHECK(ss.str() == to_string(mode));
    }
}
