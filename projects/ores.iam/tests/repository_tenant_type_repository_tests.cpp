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
#include "ores.iam/repository/tenant_type_repository.hpp"

#include <atomic>
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.iam/domain/tenant_type.hpp"
#include "ores.iam/domain/tenant_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.testing/database_helper.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[repository]");

using ores::iam::domain::tenant_type;

tenant_type make_tenant_type(ores::testing::database_helper& h) {
    static std::atomic<int> counter{0};
    const auto idx = ++counter;

    tenant_type tt;
    tt.version = 1;
    tt.type = std::string(faker::string::alphanumeric(8)) + "_" + std::to_string(idx);
    tt.name = std::string(faker::word::noun()) + " " + std::to_string(idx);
    tt.description = std::string(faker::lorem::sentence());
    tt.display_order = idx;
    tt.modified_by = h.db_user();
    tt.change_reason_code = "system.test";
    tt.change_commentary = "Synthetic test data";
    tt.performed_by = h.db_user();
    return tt;
}

}

using namespace ores::logging;

using ores::testing::database_helper;
using ores::iam::repository::tenant_type_repository;

TEST_CASE("write_single_tenant_type", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto sys_ctx = h.context().with_tenant(
        ores::utility::uuid::tenant_id::system());

    tenant_type_repository repo;
    auto tt = make_tenant_type(h);

    BOOST_LOG_SEV(lg, debug) << "Tenant type: " << tt;
    CHECK_NOTHROW(repo.write(sys_ctx, tt));
}

TEST_CASE("write_multiple_tenant_types", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto sys_ctx = h.context().with_tenant(
        ores::utility::uuid::tenant_id::system());

    tenant_type_repository repo;
    std::vector<tenant_type> types;
    for (int i = 0; i < 3; ++i)
        types.push_back(make_tenant_type(h));

    BOOST_LOG_SEV(lg, debug) << "Tenant types: " << types;
    CHECK_NOTHROW(repo.write(sys_ctx, types));
}

TEST_CASE("read_latest_tenant_types", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto sys_ctx = h.context().with_tenant(
        ores::utility::uuid::tenant_id::system());

    tenant_type_repository repo;
    std::vector<tenant_type> written;
    for (int i = 0; i < 3; ++i)
        written.push_back(make_tenant_type(h));

    BOOST_LOG_SEV(lg, debug) << "Written types: " << written;
    repo.write(sys_ctx, written);

    auto read_types = repo.read_latest(sys_ctx);
    BOOST_LOG_SEV(lg, debug) << "Read types: " << read_types;

    CHECK(!read_types.empty());
    CHECK(read_types.size() >= written.size());
}

TEST_CASE("read_latest_tenant_type_by_type", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto sys_ctx = h.context().with_tenant(
        ores::utility::uuid::tenant_id::system());

    tenant_type_repository repo;
    auto tt = make_tenant_type(h);
    const auto target_type = tt.type;

    BOOST_LOG_SEV(lg, debug) << "Tenant type: " << tt;
    repo.write(sys_ctx, tt);

    BOOST_LOG_SEV(lg, debug) << "Target type: " << target_type;

    auto read_types = repo.read_latest(sys_ctx, target_type);
    BOOST_LOG_SEV(lg, debug) << "Read types: " << read_types;

    REQUIRE(read_types.size() == 1);
    CHECK(read_types[0].type == target_type);
    CHECK(read_types[0].name == tt.name);
}

TEST_CASE("read_nonexistent_tenant_type", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto sys_ctx = h.context().with_tenant(
        ores::utility::uuid::tenant_id::system());

    tenant_type_repository repo;

    const std::string nonexistent("nonexistent_type_xyz_99999");
    BOOST_LOG_SEV(lg, debug) << "Non-existent type: " << nonexistent;

    auto read_types = repo.read_latest(sys_ctx, nonexistent);
    BOOST_LOG_SEV(lg, debug) << "Read types: " << read_types;

    CHECK(read_types.size() == 0);
}
