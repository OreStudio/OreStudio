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
#include "ores.logging/make_logger.hpp"
#include "ores.storage.api/net/object_keys.hpp"
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>
#include <string_view>

using ores::storage::api::object_keys;
using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.storage.tests");
const std::string tags("[net]");

const std::string run_id("0b6f1b2c-3d4e-5f60-7182-93a4b5c6d7e8");

}

TEST_CASE("the_platform_has_one_bucket", tags) {
    auto lg(make_logger(test_suite));

    BOOST_LOG_SEV(lg, info) << "Bucket: " << object_keys::ores_bucket;
    CHECK(object_keys::ores_bucket == "ores");
}

TEST_CASE("make_builds_a_key_from_service_purpose_and_id", tags) {
    auto lg(make_logger(test_suite));

    const auto key = object_keys::make("compute", "packages", run_id);

    BOOST_LOG_SEV(lg, info) << "Key: " << key;
    CHECK(key == "compute/packages/0b6f1b2c-3d4e-5f60-7182-93a4b5c6d7e8");
}

TEST_CASE("make_appends_the_file_name_when_one_is_given", tags) {
    auto lg(make_logger(test_suite));

    const auto key =
        object_keys::make("reporting", "runs", run_id, "trades.msgpack");

    BOOST_LOG_SEV(lg, info) << "Key: " << key;
    CHECK(key == "reporting/runs/0b6f1b2c-3d4e-5f60-7182-93a4b5c6d7e8/"
                 "trades.msgpack");
}

TEST_CASE("parse_is_the_inverse_of_make", tags) {
    auto lg(make_logger(test_suite));

    const auto key =
        object_keys::make("ore", "imports", run_id, "ore_package.tar.gz");
    const auto parsed = object_keys::parse(key);

    REQUIRE(parsed.has_value());
    BOOST_LOG_SEV(lg, info) << "Service: " << parsed->service
                            << " purpose: " << parsed->purpose
                            << " id: " << parsed->id
                            << " name: " << parsed->name;
    CHECK(parsed->service == "ore");
    CHECK(parsed->purpose == "imports");
    CHECK(parsed->id == run_id);
    CHECK(parsed->name == "ore_package.tar.gz");
}

TEST_CASE("parse_leaves_the_name_empty_when_the_key_names_no_file", tags) {
    auto lg(make_logger(test_suite));

    const auto parsed = object_keys::parse("compute/input/" + run_id);

    REQUIRE(parsed.has_value());
    CHECK(parsed->service == "compute");
    CHECK(parsed->purpose == "input");
    CHECK(parsed->id == run_id);
    CHECK(parsed->name.empty());
}

TEST_CASE("parse_refuses_a_key_that_leaves_the_protocol", tags) {
    auto lg(make_logger(test_suite));

    // Too few segments, too many, an empty segment, a segment that walks up
    // the tree, and one that is not lower snake case.
    CHECK_FALSE(object_keys::parse("compute/packages").has_value());
    CHECK_FALSE(object_keys::parse("compute/packages/" + run_id + "/a/b")
                    .has_value());
    CHECK_FALSE(object_keys::parse("compute//" + run_id).has_value());
    CHECK_FALSE(object_keys::parse("compute/packages/../etc").has_value());
    CHECK_FALSE(object_keys::parse("Compute/packages/" + run_id).has_value());

    BOOST_LOG_SEV(lg, info) << "Malformed keys refused";
}

TEST_CASE("make_refuses_a_part_that_is_not_a_valid_segment", tags) {
    auto lg(make_logger(test_suite));

    CHECK_THROWS_AS(object_keys::make("Compute", "packages", run_id),
                    std::invalid_argument);
    CHECK_THROWS_AS(object_keys::make("compute", "pack ages", run_id),
                    std::invalid_argument);
    CHECK_THROWS_AS(object_keys::make("compute", "packages", "../etc"),
                    std::invalid_argument);
    CHECK_THROWS_AS(
        object_keys::make("compute", "packages", run_id, "../escape"),
        std::invalid_argument);

    BOOST_LOG_SEV(lg, info) << "Malformed parts refused";
}
