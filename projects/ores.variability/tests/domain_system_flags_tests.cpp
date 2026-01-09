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
#include "ores.variability/domain/system_flags.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <magic_enum/magic_enum.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.variability.tests");
const std::string tags("[domain][system_flags]");

}

using ores::variability::domain::system_flag;
using ores::variability::domain::system_flag_definition;
using ores::variability::domain::system_flag_definitions;
using ores::variability::domain::to_flag_name;
using ores::variability::domain::from_flag_name;
using ores::variability::domain::get_definition;
using namespace ores::logging;

TEST_CASE("to_flag_name_converts_bootstrap_mode", tags) {
    auto lg(make_logger(test_suite));

    const auto name = to_flag_name(system_flag::bootstrap_mode);
    BOOST_LOG_SEV(lg, info) << "bootstrap_mode flag name: " << name;

    CHECK(name == "system.bootstrap_mode");
}

TEST_CASE("to_flag_name_converts_user_signups", tags) {
    auto lg(make_logger(test_suite));

    const auto name = to_flag_name(system_flag::user_signups);
    BOOST_LOG_SEV(lg, info) << "user_signups flag name: " << name;

    CHECK(name == "system.user_signups");
}

TEST_CASE("from_flag_name_parses_valid_bootstrap_mode", tags) {
    auto lg(make_logger(test_suite));

    const auto result = from_flag_name("system.bootstrap_mode");
    BOOST_LOG_SEV(lg, info) << "Parsed flag: "
        << (result ? magic_enum::enum_name(*result) : "nullopt");

    REQUIRE(result.has_value());
    CHECK(*result == system_flag::bootstrap_mode);
}

TEST_CASE("from_flag_name_parses_valid_user_signups", tags) {
    auto lg(make_logger(test_suite));

    const auto result = from_flag_name("system.user_signups");
    BOOST_LOG_SEV(lg, info) << "Parsed flag: "
        << (result ? magic_enum::enum_name(*result) : "nullopt");

    REQUIRE(result.has_value());
    CHECK(*result == system_flag::user_signups);
}

TEST_CASE("from_flag_name_returns_nullopt_for_invalid_prefix", tags) {
    auto lg(make_logger(test_suite));

    const auto result = from_flag_name("feature.bootstrap_mode");
    BOOST_LOG_SEV(lg, info) << "Result for invalid prefix: "
        << (result ? "has value" : "nullopt");

    CHECK_FALSE(result.has_value());
}

TEST_CASE("from_flag_name_returns_nullopt_for_unknown_flag", tags) {
    auto lg(make_logger(test_suite));

    const auto result = from_flag_name("system.unknown_flag");
    BOOST_LOG_SEV(lg, info) << "Result for unknown flag: "
        << (result ? "has value" : "nullopt");

    CHECK_FALSE(result.has_value());
}

TEST_CASE("from_flag_name_returns_nullopt_for_empty_string", tags) {
    auto lg(make_logger(test_suite));

    const auto result = from_flag_name("");
    BOOST_LOG_SEV(lg, info) << "Result for empty string: "
        << (result ? "has value" : "nullopt");

    CHECK_FALSE(result.has_value());
}

TEST_CASE("roundtrip_conversion_for_all_system_flags", tags) {
    auto lg(make_logger(test_suite));

    for (const auto flag : magic_enum::enum_values<system_flag>()) {
        const auto name = to_flag_name(flag);
        const auto parsed = from_flag_name(name);

        BOOST_LOG_SEV(lg, info) << "Roundtrip for " << magic_enum::enum_name(flag)
            << ": " << name << " -> "
            << (parsed ? magic_enum::enum_name(*parsed) : "nullopt");

        REQUIRE(parsed.has_value());
        CHECK(*parsed == flag);
    }
}

TEST_CASE("get_definition_returns_correct_bootstrap_mode_defaults", tags) {
    auto lg(make_logger(test_suite));

    const auto& def = get_definition(system_flag::bootstrap_mode);

    BOOST_LOG_SEV(lg, info) << "bootstrap_mode definition:"
        << " default_enabled=" << def.default_enabled
        << " description=" << def.description;

    CHECK(def.flag == system_flag::bootstrap_mode);
    CHECK(def.default_enabled == true);
    CHECK_FALSE(def.description.empty());
}

TEST_CASE("get_definition_returns_correct_user_signups_defaults", tags) {
    auto lg(make_logger(test_suite));

    const auto& def = get_definition(system_flag::user_signups);

    BOOST_LOG_SEV(lg, info) << "user_signups definition:"
        << " default_enabled=" << def.default_enabled
        << " description=" << def.description;

    CHECK(def.flag == system_flag::user_signups);
    CHECK(def.default_enabled == false);
    CHECK_FALSE(def.description.empty());
}

TEST_CASE("to_flag_name_converts_signup_requires_authorization", tags) {
    auto lg(make_logger(test_suite));

    const auto name = to_flag_name(system_flag::signup_requires_authorization);
    BOOST_LOG_SEV(lg, info) << "signup_requires_authorization flag name: " << name;

    CHECK(name == "system.signup_requires_authorization");
}

TEST_CASE("from_flag_name_parses_valid_signup_requires_authorization", tags) {
    auto lg(make_logger(test_suite));

    const auto result = from_flag_name("system.signup_requires_authorization");
    BOOST_LOG_SEV(lg, info) << "Parsed flag: "
        << (result ? magic_enum::enum_name(*result) : "nullopt");

    REQUIRE(result.has_value());
    CHECK(*result == system_flag::signup_requires_authorization);
}

TEST_CASE("get_definition_returns_correct_signup_requires_authorization_defaults", tags) {
    auto lg(make_logger(test_suite));

    const auto& def = get_definition(system_flag::signup_requires_authorization);

    BOOST_LOG_SEV(lg, info) << "signup_requires_authorization definition:"
        << " default_enabled=" << def.default_enabled
        << " description=" << def.description;

    CHECK(def.flag == system_flag::signup_requires_authorization);
    CHECK(def.default_enabled == false);
    CHECK_FALSE(def.description.empty());
}

TEST_CASE("system_flag_definitions_contains_all_enum_values", tags) {
    auto lg(make_logger(test_suite));

    const auto enum_count = magic_enum::enum_count<system_flag>();
    BOOST_LOG_SEV(lg, info) << "Enum count: " << enum_count
        << ", definitions count: " << system_flag_definitions.size();

    CHECK(system_flag_definitions.size() == enum_count);

    // Verify each enum value has a definition
    for (const auto flag : magic_enum::enum_values<system_flag>()) {
        bool found = false;
        for (const auto& def : system_flag_definitions) {
            if (def.flag == flag) {
                found = true;
                break;
            }
        }
        CHECK(found);
    }
}

TEST_CASE("stream_operator_outputs_flag_name", tags) {
    auto lg(make_logger(test_suite));

    std::ostringstream os;
    os << system_flag::bootstrap_mode;

    BOOST_LOG_SEV(lg, info) << "Stream output: " << os.str();

    CHECK(os.str() == "system.bootstrap_mode");
}
