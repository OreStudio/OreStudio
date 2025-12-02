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
#include "ores.variability/service/feature_flags_service.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"

namespace {

const std::string test_suite("ores.variability.tests");
const std::string tags("[service]");
// Corrected table name to be fully qualified
const std::string table_name("oresdb.feature_flags"); 

}

using namespace ores::utility::log;
using namespace ores::variability::service;
using namespace ores::variability::domain;

TEST_CASE("feature_flags_service_crud_operations", tags) {
    auto lg(make_logger(test_suite));
    
    SECTION("get_feature_flag returns nullopt for non-existent flag") {
        ores::testing::scoped_database_helper db_helper(table_name); 
        feature_flags_service sut(db_helper.context());
        INFO("Initial feature flags count for this section: " << sut.get_all_feature_flags().size());
        CHECK(sut.get_all_feature_flags().empty()); // Should be empty due to db_helper
        auto result = sut.get_feature_flag("non_existent_flag");
        CHECK(!result.has_value());
    }

    SECTION("save_and_get_feature_flag") {
        ores::testing::scoped_database_helper db_helper(table_name); 
        feature_flags_service sut(db_helper.context());
        INFO("Initial feature flags count for this section: " << sut.get_all_feature_flags().size());
        CHECK(sut.get_all_feature_flags().empty());

        feature_flags flag;
        flag.name = "test_flag";
        flag.enabled = true;
        flag.description = "Test Description";
        flag.modified_by = "tester"; 

        sut.save_feature_flag(flag);

        auto result = sut.get_feature_flag("test_flag");
        REQUIRE(result.has_value());
        CHECK(result->name == "test_flag");
        CHECK(result->enabled == true);
        CHECK(result->description == "Test Description");
        INFO("Actual modified_by: " << result->modified_by); 
        CHECK(result->modified_by == "ores"); 
    }

    SECTION("update_feature_flag") {
        ores::testing::scoped_database_helper db_helper(table_name); 
        feature_flags_service sut(db_helper.context());
        INFO("Initial feature flags count for this section: " << sut.get_all_feature_flags().size());
        CHECK(sut.get_all_feature_flags().empty());

        // Initial save
        feature_flags flag;
        flag.name = "update_flag";
        flag.enabled = true;
        flag.description = "Initial Description";
        flag.modified_by = "tester";
        sut.save_feature_flag(flag);

        // Update
        flag.enabled = false;
        flag.description = "Updated Description";
        flag.modified_by = "updater"; 
        sut.save_feature_flag(flag);

        // Verify update
        auto result = sut.get_feature_flag("update_flag");
        REQUIRE(result.has_value());
        CHECK(result->enabled == false);
        CHECK(result->description == "Updated Description");
        INFO("Actual modified_by after update: " << result->modified_by);
        CHECK(result->modified_by == "ores");
    }

    SECTION("delete_feature_flag") {
        ores::testing::scoped_database_helper db_helper(table_name); 
        feature_flags_service sut(db_helper.context());
        INFO("Initial feature flags count for this section: " << sut.get_all_feature_flags().size());
        CHECK(sut.get_all_feature_flags().empty());

        feature_flags flag;
        flag.name = "delete_flag";
        flag.enabled = true;
        flag.modified_by = "deleter";
        sut.save_feature_flag(flag);

        REQUIRE(sut.get_feature_flag("delete_flag").has_value());

        sut.delete_feature_flag("delete_flag");

        CHECK(!sut.get_feature_flag("delete_flag").has_value());
    }

    SECTION("get_all_feature_flags") {
        ores::testing::scoped_database_helper db_helper(table_name); 
        feature_flags_service sut(db_helper.context());
        INFO("Initial feature flags count for this section: " << sut.get_all_feature_flags().size());
        CHECK(sut.get_all_feature_flags().empty());

        feature_flags flag1;
        flag1.name = "flag1";
        flag1.enabled = true;
        flag1.modified_by = "tester";
        sut.save_feature_flag(flag1);

        feature_flags flag2;
        flag2.name = "flag2";
        flag2.enabled = false;
        flag2.modified_by = "tester";
        sut.save_feature_flag(flag2);

        auto results = sut.get_all_feature_flags();
        INFO("Actual number of flags: " << results.size()); 
        CHECK(results.size() == 2);
        
        // Basic check to ensure both are present
        bool found1 = false;
        bool found2 = false;
        for (const auto& f : results) {
            if (f.name == "flag1") found1 = true;
            if (f.name == "flag2") found2 = true;
        }
        CHECK(found1);
        CHECK(found2);
    }
}
