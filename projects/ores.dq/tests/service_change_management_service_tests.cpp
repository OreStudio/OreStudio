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
#include "ores.dq/service/change_management_service.hpp"

#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/change_reason_category_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/change_reason_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/generators/change_reason_category_generator.hpp"
#include "ores.dq/generators/change_reason_generator.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[service][change_management]");

using ores::dq::domain::change_reason_category;
using ores::dq::domain::change_reason;

change_reason_category make_unique_category(
    ores::utility::generation::generation_context& ctx,
    ores::testing::database_helper& h) {
    auto cat = ores::dq::generators::generate_synthetic_change_reason_category(ctx);
    cat.tenant_id = h.tenant_id().to_string();
    cat.code = cat.code + "_" + std::string(faker::string::alphanumeric(8));
    return cat;
}

change_reason make_unique_reason(
    ores::utility::generation::generation_context& ctx,
    ores::testing::database_helper& h,
    const std::string& category_code) {
    auto reason = ores::dq::generators::generate_synthetic_change_reason(ctx);
    reason.tenant_id = h.tenant_id().to_string();
    reason.code = reason.code + "_" + std::string(faker::string::alphanumeric(8));
    reason.category_code = category_code;
    return reason;
}

}

using namespace ores::logging;

using ores::testing::database_helper;
using ores::dq::service::change_management_service;
using ores::utility::generation::generation_context;

// ============================================================================
// Category Management
// ============================================================================

TEST_CASE("service_list_categories_returns_results", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    generation_context ctx;
    change_management_service svc(h.context());

    auto cat = make_unique_category(ctx, h);
    svc.save_category(cat);

    auto categories = svc.list_categories();
    BOOST_LOG_SEV(lg, debug) << "Categories: " << categories;

    CHECK(!categories.empty());
}

TEST_CASE("service_save_and_find_category_roundtrip", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    generation_context ctx;
    change_management_service svc(h.context());

    auto cat = make_unique_category(ctx, h);
    const auto target_code = cat.code;

    BOOST_LOG_SEV(lg, debug) << "Saving category: " << cat;
    svc.save_category(cat);

    auto found = svc.find_category(target_code);
    BOOST_LOG_SEV(lg, debug) << "Found category: " << found.has_value();

    REQUIRE(found.has_value());
    CHECK(found->code == target_code);
    CHECK(found->description == cat.description);
}

TEST_CASE("service_find_category_returns_nullopt_for_nonexistent", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    change_management_service svc(h.context());

    auto found = svc.find_category("nonexistent_category_xyz_99999");
    CHECK(!found.has_value());
}

TEST_CASE("service_save_category_throws_for_empty_code", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    generation_context ctx;
    change_management_service svc(h.context());

    auto cat = make_unique_category(ctx, h);
    cat.code = "";

    CHECK_THROWS_AS(svc.save_category(cat), std::invalid_argument);
}

TEST_CASE("service_remove_category", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    generation_context ctx;
    change_management_service svc(h.context());

    auto cat = make_unique_category(ctx, h);
    const auto target_code = cat.code;

    svc.save_category(cat);
    REQUIRE(svc.find_category(target_code).has_value());

    BOOST_LOG_SEV(lg, debug) << "Removing category: " << target_code;
    CHECK_NOTHROW(svc.remove_category(target_code));

    auto found = svc.find_category(target_code);
    CHECK(!found.has_value());
}

TEST_CASE("service_remove_category_throws_when_reasons_exist", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    generation_context ctx;
    change_management_service svc(h.context());

    auto cat = make_unique_category(ctx, h);
    const auto cat_code = cat.code;
    svc.save_category(cat);

    auto reason = make_unique_reason(ctx, h, cat_code);
    svc.save_reason(reason);

    BOOST_LOG_SEV(lg, debug) << "Attempting to remove category with reasons: "
                             << cat_code;
    CHECK_THROWS_AS(svc.remove_category(cat_code), std::runtime_error);
}

TEST_CASE("service_get_category_count", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    generation_context ctx;
    change_management_service svc(h.context());

    auto initial_count = svc.get_category_count();
    BOOST_LOG_SEV(lg, debug) << "Initial category count: " << initial_count;

    auto cat = make_unique_category(ctx, h);
    svc.save_category(cat);

    auto new_count = svc.get_category_count();
    BOOST_LOG_SEV(lg, debug) << "New category count: " << new_count;

    CHECK(new_count == initial_count + 1);
}

// ============================================================================
// Reason Management
// ============================================================================

TEST_CASE("service_list_reasons_returns_results", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    generation_context ctx;
    change_management_service svc(h.context());

    auto cat = make_unique_category(ctx, h);
    svc.save_category(cat);

    auto reason = make_unique_reason(ctx, h, cat.code);
    svc.save_reason(reason);

    auto reasons = svc.list_reasons();
    BOOST_LOG_SEV(lg, debug) << "Reasons: " << reasons;

    CHECK(!reasons.empty());
}

TEST_CASE("service_save_and_find_reason_roundtrip", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    generation_context ctx;
    change_management_service svc(h.context());

    auto cat = make_unique_category(ctx, h);
    svc.save_category(cat);

    auto reason = make_unique_reason(ctx, h, cat.code);
    const auto target_code = reason.code;

    BOOST_LOG_SEV(lg, debug) << "Saving reason: " << reason;
    svc.save_reason(reason);

    auto found = svc.find_reason(target_code);
    BOOST_LOG_SEV(lg, debug) << "Found reason: " << found.has_value();

    REQUIRE(found.has_value());
    CHECK(found->code == target_code);
    CHECK(found->description == reason.description);
    CHECK(found->category_code == cat.code);
}

TEST_CASE("service_find_reason_returns_nullopt_for_nonexistent", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    change_management_service svc(h.context());

    auto found = svc.find_reason("nonexistent_reason_xyz_99999");
    CHECK(!found.has_value());
}

TEST_CASE("service_save_reason_throws_for_empty_code", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    generation_context ctx;
    change_management_service svc(h.context());

    auto cat = make_unique_category(ctx, h);
    svc.save_category(cat);

    auto reason = make_unique_reason(ctx, h, cat.code);
    reason.code = "";

    CHECK_THROWS_AS(svc.save_reason(reason), std::invalid_argument);
}

TEST_CASE("service_remove_reason", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    generation_context ctx;
    change_management_service svc(h.context());

    auto cat = make_unique_category(ctx, h);
    svc.save_category(cat);

    auto reason = make_unique_reason(ctx, h, cat.code);
    const auto target_code = reason.code;

    svc.save_reason(reason);
    REQUIRE(svc.find_reason(target_code).has_value());

    BOOST_LOG_SEV(lg, debug) << "Removing reason: " << target_code;
    CHECK_NOTHROW(svc.remove_reason(target_code));

    auto found = svc.find_reason(target_code);
    CHECK(!found.has_value());
}

TEST_CASE("service_list_reasons_by_category", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    generation_context ctx;
    change_management_service svc(h.context());

    auto cat = make_unique_category(ctx, h);
    svc.save_category(cat);

    auto r1 = make_unique_reason(ctx, h, cat.code);
    auto r2 = make_unique_reason(ctx, h, cat.code);
    svc.save_reason(r1);
    svc.save_reason(r2);

    auto reasons = svc.list_reasons_by_category(cat.code);
    BOOST_LOG_SEV(lg, debug) << "Reasons for category " << cat.code
                             << ": " << reasons;

    CHECK(reasons.size() >= 2);
    for (const auto& r : reasons) {
        CHECK(r.category_code == cat.code);
    }
}

// ============================================================================
// Validation
// ============================================================================

TEST_CASE("service_is_valid_reason_code", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    generation_context ctx;
    change_management_service svc(h.context());

    auto cat = make_unique_category(ctx, h);
    svc.save_category(cat);

    auto reason = make_unique_reason(ctx, h, cat.code);
    svc.save_reason(reason);

    CHECK(svc.is_valid_reason_code(reason.code));
    CHECK(!svc.is_valid_reason_code("nonexistent_reason_xyz_99999"));
}

TEST_CASE("service_is_valid_category_code", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    generation_context ctx;
    change_management_service svc(h.context());

    auto cat = make_unique_category(ctx, h);
    svc.save_category(cat);

    CHECK(svc.is_valid_category_code(cat.code));
    CHECK(!svc.is_valid_category_code("nonexistent_category_xyz_99999"));
}
