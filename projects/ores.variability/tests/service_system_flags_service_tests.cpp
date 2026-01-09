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
#include "ores.variability/service/system_flags_service.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"

namespace {

const std::string_view test_suite("ores.variability.tests");
const std::string tags("[service][system_flags]");
const std::string table_name("ores.feature_flags");

}

using namespace ores::logging;
using namespace ores::variability::service;
using namespace ores::variability::domain;

TEST_CASE("system_flags_service_default_values", tags) {
    auto lg(make_logger(test_suite));

    SECTION("bootstrap_mode returns default true when flag not in database") {
        ores::testing::scoped_database_helper db_helper(table_name);
        system_flags_service sut(db_helper.context());

        // Flag doesn't exist, should return default from definition (true)
        const bool result = sut.is_bootstrap_mode_enabled();
        BOOST_LOG_SEV(lg, info) << "bootstrap_mode default: " << result;

        CHECK(result == true);
    }

    SECTION("user_signups returns default false when flag not in database") {
        ores::testing::scoped_database_helper db_helper(table_name);
        system_flags_service sut(db_helper.context());

        // Flag doesn't exist, should return default from definition (false)
        const bool result = sut.is_user_signups_enabled();
        BOOST_LOG_SEV(lg, info) << "user_signups default: " << result;

        CHECK(result == false);
    }

    SECTION("is_enabled returns default for any system flag not in database") {
        ores::testing::scoped_database_helper db_helper(table_name);
        system_flags_service sut(db_helper.context());

        // Test generic is_enabled method
        CHECK(sut.is_enabled(system_flag::bootstrap_mode) == true);
        CHECK(sut.is_enabled(system_flag::user_signups) == false);
    }
}

TEST_CASE("system_flags_service_set_and_get", tags) {
    auto lg(make_logger(test_suite));

    SECTION("set_bootstrap_mode persists value") {
        ores::testing::scoped_database_helper db_helper(table_name);
        system_flags_service sut(db_helper.context());

        // Set to false (opposite of default)
        sut.set_bootstrap_mode(false, "test_user");

        const bool result = sut.is_bootstrap_mode_enabled();
        BOOST_LOG_SEV(lg, info) << "bootstrap_mode after set: " << result;

        CHECK(result == false);
    }

    SECTION("set_user_signups persists value") {
        ores::testing::scoped_database_helper db_helper(table_name);
        system_flags_service sut(db_helper.context());

        // Set to true (opposite of default)
        sut.set_user_signups(true, "admin");

        const bool result = sut.is_user_signups_enabled();
        BOOST_LOG_SEV(lg, info) << "user_signups after set: " << result;

        CHECK(result == true);
    }

    SECTION("set_enabled with generic method persists value") {
        ores::testing::scoped_database_helper db_helper(table_name);
        system_flags_service sut(db_helper.context());

        sut.set_enabled(system_flag::bootstrap_mode, false, "system");
        sut.set_enabled(system_flag::user_signups, true, "system");

        CHECK(sut.is_enabled(system_flag::bootstrap_mode) == false);
        CHECK(sut.is_enabled(system_flag::user_signups) == true);
    }
}

TEST_CASE("system_flags_service_update_existing", tags) {
    auto lg(make_logger(test_suite));

    SECTION("updating bootstrap_mode toggles value") {
        ores::testing::scoped_database_helper db_helper(table_name);
        system_flags_service sut(db_helper.context());

        // Set initial value
        sut.set_bootstrap_mode(true, "initial");
        CHECK(sut.is_bootstrap_mode_enabled() == true);

        // Update to opposite
        sut.set_bootstrap_mode(false, "update");
        CHECK(sut.is_bootstrap_mode_enabled() == false);

        // Update back
        sut.set_bootstrap_mode(true, "revert");
        CHECK(sut.is_bootstrap_mode_enabled() == true);
    }

    SECTION("updating user_signups toggles value") {
        ores::testing::scoped_database_helper db_helper(table_name);
        system_flags_service sut(db_helper.context());

        // Set initial value
        sut.set_user_signups(false, "initial");
        CHECK(sut.is_user_signups_enabled() == false);

        // Update to opposite
        sut.set_user_signups(true, "update");
        CHECK(sut.is_user_signups_enabled() == true);

        // Update back
        sut.set_user_signups(false, "revert");
        CHECK(sut.is_user_signups_enabled() == false);
    }
}

TEST_CASE("system_flags_service_multiple_flags_independent", tags) {
    auto lg(make_logger(test_suite));

    SECTION("setting one flag does not affect others") {
        ores::testing::scoped_database_helper db_helper(table_name);
        system_flags_service sut(db_helper.context());

        // Set bootstrap_mode to false
        sut.set_bootstrap_mode(false, "test");

        // user_signups should still return its default (false)
        CHECK(sut.is_bootstrap_mode_enabled() == false);
        CHECK(sut.is_user_signups_enabled() == false);

        // Now set user_signups to true
        sut.set_user_signups(true, "test");

        // bootstrap_mode should remain false
        CHECK(sut.is_bootstrap_mode_enabled() == false);
        CHECK(sut.is_user_signups_enabled() == true);
    }
}

TEST_CASE("system_flags_service_refresh", tags) {
    auto lg(make_logger(test_suite));

    SECTION("refresh loads flags from database") {
        ores::testing::scoped_database_helper db_helper(table_name);

        // First service writes to database
        {
            system_flags_service writer(db_helper.context());
            writer.set_bootstrap_mode(false, "writer");
            writer.set_user_signups(true, "writer");
        }

        // Second service should see defaults initially, then DB values after refresh
        system_flags_service reader(db_helper.context());

        // Before refresh - returns POD defaults
        CHECK(reader.is_bootstrap_mode_enabled() == true);
        CHECK(reader.is_user_signups_enabled() == false);

        // After refresh - returns database values
        reader.refresh();
        CHECK(reader.is_bootstrap_mode_enabled() == false);
        CHECK(reader.is_user_signups_enabled() == true);
    }
}

TEST_CASE("system_flags_service_cache_accessor", tags) {
    auto lg(make_logger(test_suite));

    SECTION("cache returns current flag values") {
        ores::testing::scoped_database_helper db_helper(table_name);
        system_flags_service sut(db_helper.context());

        // Set some values
        sut.set_bootstrap_mode(false, "test");
        sut.set_user_signups(true, "test");

        // Access via cache()
        const auto& cache = sut.cache();
        CHECK(cache.bootstrap_mode == false);
        CHECK(cache.user_signups == true);
    }

    SECTION("cache reflects refresh") {
        ores::testing::scoped_database_helper db_helper(table_name);

        // Write values with one service
        {
            system_flags_service writer(db_helper.context());
            writer.set_bootstrap_mode(false, "writer");
        }

        // Read with another service
        system_flags_service reader(db_helper.context());
        reader.refresh();

        const auto& cache = reader.cache();
        CHECK(cache.bootstrap_mode == false);
    }
}
