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

#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"

namespace {

const std::string_view test_suite("ores.variability.tests");
const std::string tags("[service]");

}

using namespace ores::logging;
using namespace ores::variability::service;
using namespace ores::variability::domain;

TEST_CASE("feature_flags_service_get_returns_nullopt_for_non_existent_flag", tags) {
    auto lg(make_logger(test_suite));
    ores::testing::scoped_database_helper db_helper;
    feature_flags_service sut(db_helper.context());
    INFO("Initial feature flags count: " << sut.get_all_feature_flags().size());
    // Database has pre-seeded flags from template
    auto result = sut.get_feature_flag("non_existent_flag_xyz123");
    CHECK(!result.has_value());
}

TEST_CASE("feature_flags_service_save_and_get_feature_flag", tags) {
    auto lg(make_logger(test_suite));
    ores::testing::scoped_database_helper db_helper;
    feature_flags_service sut(db_helper.context());
    INFO("Initial feature flags count: " << sut.get_all_feature_flags().size());
    // Database has pre-seeded flags from template

    feature_flags flag;
    flag.tenant_id = db_helper.tenant_id().to_string();
    flag.name = "test_flag";
    flag.enabled = true;
    flag.description = "Test Description";
    flag.modified_by = "tester";
    flag.change_reason_code = "system.test";
    flag.change_commentary = "Test data";

    sut.save_feature_flag(flag);

    auto result = sut.get_feature_flag("test_flag");
    REQUIRE(result.has_value());
    CHECK(result->name == "test_flag");
    CHECK(result->enabled == true);
    CHECK(result->description == "Test Description");
    INFO("Actual modified_by: " << result->modified_by);
    CHECK(result->modified_by == "tester");
}

TEST_CASE("feature_flags_service_update_feature_flag", tags) {
    auto lg(make_logger(test_suite));
    ores::testing::scoped_database_helper db_helper;
    feature_flags_service sut(db_helper.context());
    INFO("Initial feature flags count: " << sut.get_all_feature_flags().size());
    // Database has pre-seeded flags from template

    // Initial save
    feature_flags flag;
    flag.tenant_id = db_helper.tenant_id().to_string();
    flag.name = "update_flag";
    flag.enabled = true;
    flag.description = "Initial Description";
    flag.modified_by = "tester";
    flag.change_reason_code = "system.test";
    flag.change_commentary = "Test data";
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
    CHECK(result->modified_by == "updater");
}

TEST_CASE("feature_flags_service_delete_feature_flag", tags) {
    auto lg(make_logger(test_suite));
    ores::testing::scoped_database_helper db_helper;
    feature_flags_service sut(db_helper.context());
    INFO("Initial feature flags count: " << sut.get_all_feature_flags().size());
    // Database has pre-seeded flags from template

    feature_flags flag;
    flag.tenant_id = db_helper.tenant_id().to_string();
    flag.name = "delete_flag_unique_test";
    flag.enabled = true;
    flag.modified_by = "deleter";
    flag.change_reason_code = "system.test";
    flag.change_commentary = "Test data";
    sut.save_feature_flag(flag);

    REQUIRE(sut.get_feature_flag("delete_flag_unique_test").has_value());

    sut.delete_feature_flag("delete_flag_unique_test");

    CHECK(!sut.get_feature_flag("delete_flag_unique_test").has_value());
}

TEST_CASE("feature_flags_service_get_all_feature_flags", tags) {
    auto lg(make_logger(test_suite));
    ores::testing::scoped_database_helper db_helper;
    feature_flags_service sut(db_helper.context());
    const auto initial_count = sut.get_all_feature_flags().size();
    INFO("Initial feature flags count: " << initial_count);
    // Database has pre-seeded flags from template

    feature_flags flag1;
    flag1.tenant_id = db_helper.tenant_id().to_string();
    flag1.name = "unique_flag1_xyz";
    flag1.enabled = true;
    flag1.modified_by = "tester";
    flag1.change_reason_code = "system.test";
    flag1.change_commentary = "Test data";
    sut.save_feature_flag(flag1);

    feature_flags flag2;
    flag2.tenant_id = db_helper.tenant_id().to_string();
    flag2.name = "unique_flag2_xyz";
    flag2.enabled = false;
    flag2.modified_by = "tester";
    flag2.change_reason_code = "system.test";
    flag2.change_commentary = "Test data";
    sut.save_feature_flag(flag2);

    auto results = sut.get_all_feature_flags();
    INFO("Actual number of flags: " << results.size());
    CHECK(results.size() == initial_count + 2);

    // Basic check to ensure both are present using std::any_of
    CHECK(std::ranges::any_of(results, [](const auto& f) {
        return f.name == "unique_flag1_xyz"; }));
    CHECK(std::ranges::any_of(results, [](const auto& f) {
        return f.name == "unique_flag2_xyz"; }));
}
