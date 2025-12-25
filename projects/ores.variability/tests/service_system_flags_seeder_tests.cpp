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
#include "ores.variability/service/system_flags_seeder.hpp"
#include "ores.variability/service/feature_flags_service.hpp"
#include "ores.variability/domain/system_flags.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"

namespace {

const std::string_view test_suite("ores.variability.tests");
const std::string tags("[service][system_flags_seeder]");
const std::string table_name("ores.feature_flags");

}

using namespace ores::utility::log;
using namespace ores::variability::service;
using namespace ores::variability::domain;

TEST_CASE("system_flags_seeder_creates_all_flags_when_database_is_empty", tags) {
    auto lg(make_logger(test_suite));

    ores::testing::scoped_database_helper db_helper(table_name);
    system_flags_seeder sut(db_helper.context());

    const auto created = sut.seed();
    BOOST_LOG_SEV(lg, info) << "Created " << created << " flags";

    CHECK(created == system_flag_definitions.size());
}

TEST_CASE("system_flags_seeder_all_system_flags_exist_after_seeding", tags) {
    auto lg(make_logger(test_suite));

    ores::testing::scoped_database_helper db_helper(table_name);
    system_flags_seeder seeder(db_helper.context());
    seeder.seed();

    // Verify using feature_flags_service
    feature_flags_service svc(db_helper.context());

    for (const auto& def : system_flag_definitions) {
        const auto flag_name = to_flag_name(def.flag);
        auto flag_opt = svc.get_feature_flag(flag_name);

        INFO("Checking flag: " << flag_name);
        REQUIRE(flag_opt.has_value());
        CHECK(flag_opt->enabled == def.default_enabled);
        CHECK(!flag_opt->recorded_by.empty());
    }
}

TEST_CASE("system_flags_seeder_does_not_overwrite_existing_flag_values", tags) {
    auto lg(make_logger(test_suite));

    ores::testing::scoped_database_helper db_helper(table_name);

    // Pre-create bootstrap_mode with non-default value
    {
        feature_flags_service svc(db_helper.context());
        feature_flags ff{
            .enabled = false,  // Default is true
            .name = to_flag_name(system_flag::bootstrap_mode),
            .description = "Custom description",
            .recorded_by = "admin"
        };
        svc.save_feature_flag(ff);
    }

    // Run seeder
    system_flags_seeder seeder(db_helper.context());
    const auto created = seeder.seed();

    // Should only create one flag (user_signups), not two
    BOOST_LOG_SEV(lg, info) << "Created " << created << " flags (expected 1)";
    CHECK(created == system_flag_definitions.size() - 1);

    // Verify bootstrap_mode retained its custom value
    feature_flags_service svc(db_helper.context());
    auto bootstrap_flag = svc.get_feature_flag(
        to_flag_name(system_flag::bootstrap_mode));

    REQUIRE(bootstrap_flag.has_value());
    CHECK(bootstrap_flag->enabled == false);  // Should NOT be overwritten
}

TEST_CASE("system_flags_seeder_returns_zero_when_all_flags_already_exist", tags) {
    auto lg(make_logger(test_suite));

    ores::testing::scoped_database_helper db_helper(table_name);
    system_flags_seeder seeder(db_helper.context());

    // First run creates all flags
    const auto first_run = seeder.seed();
    CHECK(first_run == system_flag_definitions.size());

    // Second run creates nothing
    const auto second_run = seeder.seed();
    BOOST_LOG_SEV(lg, info) << "Second run created " << second_run << " flags";
    CHECK(second_run == 0);
}

TEST_CASE("system_flags_seeder_multiple_seeds_produce_same_result", tags) {
    auto lg(make_logger(test_suite));

    ores::testing::scoped_database_helper db_helper(table_name);
    system_flags_seeder seeder(db_helper.context());

    // Run multiple times
    seeder.seed();
    seeder.seed();
    seeder.seed();

    // Verify correct number of flags exist
    feature_flags_service svc(db_helper.context());
    auto all_flags = svc.get_all_feature_flags();

    BOOST_LOG_SEV(lg, info) << "Total flags after 3 seeds: "
        << all_flags.size();
    CHECK(all_flags.size() == system_flag_definitions.size());
}
