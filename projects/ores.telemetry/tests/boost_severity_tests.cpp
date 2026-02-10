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
#include "ores.logging/boost_severity.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[boost_severity]");

using namespace ores::logging;

}

TEST_CASE("to_boost_severity_from_string_converts_all_valid_levels", tags) {
    REQUIRE(to_boost_severity("trace") == boost_severity::trace);
    REQUIRE(to_boost_severity("debug") == boost_severity::debug);
    REQUIRE(to_boost_severity("info") == boost_severity::info);
    REQUIRE(to_boost_severity("warn") == boost_severity::warn);
    REQUIRE(to_boost_severity("error") == boost_severity::error);
}

TEST_CASE("to_boost_severity_from_string_throws_on_invalid_input", tags) {
    REQUIRE_THROWS_AS(to_boost_severity(""), std::invalid_argument);
    REQUIRE_THROWS_AS(to_boost_severity("invalid"), std::invalid_argument);
    REQUIRE_THROWS_AS(to_boost_severity("TRACE"), std::invalid_argument);
    REQUIRE_THROWS_AS(to_boost_severity("fatal"), std::invalid_argument);
}

TEST_CASE("to_boost_severity_from_domain_converts_all_levels", tags) {
    REQUIRE(to_boost_severity(severity_level::trace) == boost_severity::trace);
    REQUIRE(to_boost_severity(severity_level::debug) == boost_severity::debug);
    REQUIRE(to_boost_severity(severity_level::info) == boost_severity::info);
    REQUIRE(to_boost_severity(severity_level::warn) == boost_severity::warn);
    REQUIRE(to_boost_severity(severity_level::error) == boost_severity::error);
}

TEST_CASE("to_boost_severity_maps_fatal_to_error", tags) {
    REQUIRE(to_boost_severity(severity_level::fatal) == boost_severity::error);
}

TEST_CASE("to_domain_severity_converts_all_boost_levels", tags) {
    REQUIRE(to_domain_severity(boost_severity::trace) == severity_level::trace);
    REQUIRE(to_domain_severity(boost_severity::debug) == severity_level::debug);
    REQUIRE(to_domain_severity(boost_severity::info) == severity_level::info);
    REQUIRE(to_domain_severity(boost_severity::warn) == severity_level::warn);
    REQUIRE(to_domain_severity(boost_severity::error) == severity_level::error);
}

TEST_CASE("boost_severity_stream_insertion_produces_expected_strings", tags) {
    auto to_string = [](boost_severity sev) {
        std::ostringstream ss;
        ss << sev;
        return ss.str();
    };

    REQUIRE(to_string(boost_severity::trace) == "TRACE");
    REQUIRE(to_string(boost_severity::debug) == "DEBUG");
    REQUIRE(to_string(boost_severity::info) == "INFO");
    REQUIRE(to_string(boost_severity::warn) == "WARN");
    REQUIRE(to_string(boost_severity::error) == "ERROR");
}
