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
#include "ores.iam/repository/tenant_status_repository.hpp"

#include <atomic>
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.iam/domain/tenant_status.hpp"
#include "ores.iam/domain/tenant_status_json_io.hpp" // IWYU pragma: keep.
#include "ores.testing/database_helper.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[repository]");

using ores::iam::domain::tenant_status;

tenant_status make_tenant_status(ores::testing::database_helper& h) {
    static std::atomic<int> counter{0};
    const auto idx = ++counter;

    tenant_status ts;
    ts.version = 1;
    ts.status = std::string(faker::string::alphanumeric(8)) + "_" + std::to_string(idx);
    ts.name = std::string(faker::word::noun()) + " " + std::to_string(idx);
    ts.description = std::string(faker::lorem::sentence());
    ts.display_order = idx;
    ts.modified_by = h.db_user();
    ts.change_reason_code = "system.test";
    ts.change_commentary = "Synthetic test data";
    ts.performed_by = h.db_user();
    return ts;
}

}

using namespace ores::logging;

using ores::testing::database_helper;
using ores::iam::repository::tenant_status_repository;

TEST_CASE("write_single_tenant_status", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto sys_ctx = h.context().with_tenant(
        ores::utility::uuid::tenant_id::system());

    tenant_status_repository repo;
    auto ts = make_tenant_status(h);

    BOOST_LOG_SEV(lg, debug) << "Tenant status: " << ts;
    CHECK_NOTHROW(repo.write(sys_ctx, ts));
}

TEST_CASE("write_multiple_tenant_statuses", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto sys_ctx = h.context().with_tenant(
        ores::utility::uuid::tenant_id::system());

    tenant_status_repository repo;
    std::vector<tenant_status> statuses;
    for (int i = 0; i < 3; ++i)
        statuses.push_back(make_tenant_status(h));

    BOOST_LOG_SEV(lg, debug) << "Tenant statuses: " << statuses;
    CHECK_NOTHROW(repo.write(sys_ctx, statuses));
}

TEST_CASE("read_latest_tenant_statuses", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto sys_ctx = h.context().with_tenant(
        ores::utility::uuid::tenant_id::system());

    tenant_status_repository repo;
    std::vector<tenant_status> written;
    for (int i = 0; i < 3; ++i)
        written.push_back(make_tenant_status(h));

    BOOST_LOG_SEV(lg, debug) << "Written statuses: " << written;
    repo.write(sys_ctx, written);

    auto read_statuses = repo.read_latest(sys_ctx);
    BOOST_LOG_SEV(lg, debug) << "Read statuses: " << read_statuses;

    CHECK(!read_statuses.empty());
    CHECK(read_statuses.size() >= written.size());
}

TEST_CASE("read_latest_tenant_status_by_status", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto sys_ctx = h.context().with_tenant(
        ores::utility::uuid::tenant_id::system());

    tenant_status_repository repo;
    auto ts = make_tenant_status(h);
    const auto target_status = ts.status;

    BOOST_LOG_SEV(lg, debug) << "Tenant status: " << ts;
    repo.write(sys_ctx, ts);

    BOOST_LOG_SEV(lg, debug) << "Target status: " << target_status;

    auto read_statuses = repo.read_latest(sys_ctx, target_status);
    BOOST_LOG_SEV(lg, debug) << "Read statuses: " << read_statuses;

    REQUIRE(read_statuses.size() == 1);
    CHECK(read_statuses[0].status == target_status);
    CHECK(read_statuses[0].name == ts.name);
}

TEST_CASE("read_nonexistent_tenant_status", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto sys_ctx = h.context().with_tenant(
        ores::utility::uuid::tenant_id::system());

    tenant_status_repository repo;

    const std::string nonexistent("nonexistent_status_xyz_99999");
    BOOST_LOG_SEV(lg, debug) << "Non-existent status: " << nonexistent;

    auto read_statuses = repo.read_latest(sys_ctx, nonexistent);
    BOOST_LOG_SEV(lg, debug) << "Read statuses: " << read_statuses;

    CHECK(read_statuses.size() == 0);
}
