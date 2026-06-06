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
#include "ores.shell/app/command_args.hpp"
#include "ores.logging/make_logger.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string_view test_suite("ores.shell.tests");
const std::string tags("[app]");

}

using ores::shell::app::flag_spec;
using ores::shell::app::parse_args;
using ores::shell::app::parse_positive_seconds;
using namespace ores::logging;

TEST_CASE("parse_args_positionals_only", tags) {
    auto lg(make_logger(test_suite));

    auto r = parse_args({"alpha", "beta"}, {});
    REQUIRE(r.has_value());
    CHECK(r->positionals == std::vector<std::string>{"alpha", "beta"});
    CHECK(r->flags.empty());
}

TEST_CASE("parse_args_switch_defaults_to_false", tags) {
    auto lg(make_logger(test_suite));

    auto r = parse_args({"file.ores"}, {{.name = "continue-on-error"}});
    REQUIRE(r.has_value());
    CHECK_FALSE(r->flag_set("continue-on-error"));
    CHECK(r->positionals == std::vector<std::string>{"file.ores"});
}

TEST_CASE("parse_args_switch_present", tags) {
    auto lg(make_logger(test_suite));

    auto r = parse_args({"file.ores", "--continue-on-error"},
                        {{.name = "continue-on-error"}});
    REQUIRE(r.has_value());
    CHECK(r->flag_set("continue-on-error"));
}

TEST_CASE("parse_args_switch_anywhere_among_positionals", tags) {
    auto lg(make_logger(test_suite));

    auto r = parse_args({"--continue-on-error", "file.ores"},
                        {{.name = "continue-on-error"}});
    REQUIRE(r.has_value());
    CHECK(r->flag_set("continue-on-error"));
    CHECK(r->positionals == std::vector<std::string>{"file.ores"});
}

TEST_CASE("parse_args_value_flag_with_separate_value", tags) {
    auto lg(make_logger(test_suite));

    auto r = parse_args({"--seed", "42"},
                        {{.name = "seed", .requires_value = true,
                          .default_value = ""}});
    REQUIRE(r.has_value());
    CHECK(r->flag("seed") == "42");
}

TEST_CASE("parse_args_value_flag_with_inline_value", tags) {
    auto lg(make_logger(test_suite));

    auto r = parse_args({"--seed=42"},
                        {{.name = "seed", .requires_value = true,
                          .default_value = ""}});
    REQUIRE(r.has_value());
    CHECK(r->flag("seed") == "42");
}

TEST_CASE("parse_args_value_flag_uses_default_when_absent", tags) {
    auto lg(make_logger(test_suite));

    auto r = parse_args({}, {{.name = "dataset-size", .requires_value = true,
                              .default_value = "small"}});
    REQUIRE(r.has_value());
    CHECK(r->flag("dataset-size") == "small");
}

TEST_CASE("parse_args_unknown_flag_is_error", tags) {
    auto lg(make_logger(test_suite));

    auto r = parse_args({"--bogus"}, {{.name = "continue-on-error"}});
    REQUIRE_FALSE(r.has_value());
    BOOST_LOG_SEV(lg, info) << "Error: " << r.error();
    CHECK(r.error().find("--bogus") != std::string::npos);
}

TEST_CASE("parse_args_value_flag_missing_value_is_error", tags) {
    auto lg(make_logger(test_suite));

    auto r = parse_args({"--seed"},
                        {{.name = "seed", .requires_value = true,
                          .default_value = ""}});
    REQUIRE_FALSE(r.has_value());
    CHECK(r.error().find("--seed") != std::string::npos);
}

TEST_CASE("parse_args_value_flag_followed_by_flag_is_error", tags) {
    auto lg(make_logger(test_suite));

    auto r = parse_args({"--seed", "--continue-on-error"},
                        {{.name = "seed", .requires_value = true,
                          .default_value = ""},
                         {.name = "continue-on-error"}});
    REQUIRE_FALSE(r.has_value());
    CHECK(r.error().find("--seed") != std::string::npos);
}

TEST_CASE("parse_args_switch_with_inline_value_is_error", tags) {
    auto lg(make_logger(test_suite));

    auto r = parse_args({"--continue-on-error=yes"},
                        {{.name = "continue-on-error"}});
    REQUIRE_FALSE(r.has_value());
    CHECK(r.error().find("continue-on-error") != std::string::npos);
}

TEST_CASE("parse_positive_seconds_accepts_positive_integers", tags) {
    auto lg(make_logger(test_suite));

    auto r = parse_positive_seconds("300");
    REQUIRE(r.has_value());
    CHECK(r->count() == 300);
}

TEST_CASE("parse_positive_seconds_rejects_bad_input", tags) {
    auto lg(make_logger(test_suite));

    CHECK_FALSE(parse_positive_seconds("0").has_value());
    CHECK_FALSE(parse_positive_seconds("-5").has_value());
    CHECK_FALSE(parse_positive_seconds("30x").has_value());
    CHECK_FALSE(parse_positive_seconds("abc").has_value());
    CHECK_FALSE(parse_positive_seconds("").has_value());
}
