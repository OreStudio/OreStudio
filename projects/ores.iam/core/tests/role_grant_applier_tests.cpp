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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.iam.core/repository/role_grant_request_repository.hpp"
#include "ores.iam.core/repository/role_grant_request_role_repository.hpp"
#include "ores.iam.core/repository/role_repository.hpp"
#include "ores.iam.core/service/role_grant_applier.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[role_request]");

auto& lg() {
    static auto instance = ores::logging::make_logger("ores.iam.core.tests.role_grant_applier");
    return instance;
}

// The database stamps every row with the account that wrote it, as a signed-in
// context always names one.
ores::database::context acting(ores::testing::scoped_database_helper& h) {
    return h.context().with_tenant(h.tenant_id(), h.db_user());
}

ores::iam::domain::account seed_account(ores::testing::scoped_database_helper& h) {
    auto ctx = ores::testing::make_generation_context(h);
    auto a = ores::iam::generators::generate_synthetic_account(ctx);
    a.change_reason_code = "system.test";
    ores::iam::repository::account_repository repo;
    repo.write(acting(h), a);
    return a;
}

void run(ores::testing::scoped_database_helper& h,
         const std::string& sql,
         const std::vector<std::string>& params) {
    ores::database::repository::execute_parameterized_command(acting(h), sql, params, lg(), sql);
}

/**
 * @brief An iam.role_grant request for a role, already approved by another
 * person, as the inbox leaves one after its decision.
 */
std::string approved_request(ores::testing::scoped_database_helper& h,
                             const ores::iam::domain::account& asker,
                             const ores::iam::domain::account& approver,
                             const boost::uuids::uuid& role_id) {
    const auto id = boost::uuids::to_string(ores::utility::uuid::uuid_v7_generator{}());
    const auto tenant = h.tenant_id().to_string();
    run(h,
        "insert into ores_inbox_approval_requests_tbl (id, tenant_id, version, kind_code,"
        " state_code, requested_by, requested_at, reason, modified_by, performed_by,"
        " change_reason_code, change_commentary) values ($1::uuid, $2::uuid, 0,"
        " 'iam.role_grant', 'approved', $3::uuid, now(), 'Covers the desk', $4, $4,"
        " 'system.test', '')",
        {id, tenant, boost::uuids::to_string(asker.id), h.db_user()});
    run(h,
        "insert into ores_inbox_approval_decisions_tbl (id, tenant_id, version, request_id,"
        " decision_code, decided_by, decided_at, comment, modified_by, performed_by,"
        " change_reason_code, change_commentary) values (gen_random_uuid(), $1::uuid, 0,"
        " $2::uuid, 'approve', $3::uuid, now(), '', $4, $4, 'system.test', '')",
        {tenant, id, boost::uuids::to_string(approver.id), h.db_user()});

    ores::iam::domain::role_grant_request detail;
    detail.tenant_id = h.tenant_id();
    detail.request_id = boost::uuids::string_generator{}(id);
    detail.account_id = asker.id;
    detail.modified_by = h.db_user();
    detail.change_reason_code = "system.test";
    ores::iam::repository::role_grant_request_repository requests;
    requests.write(acting(h), detail, ores::utility::domain::precondition{});

    ores::iam::domain::role_grant_request_role role;
    role.tenant_id = tenant;
    role.request_id = detail.request_id;
    role.role_id = role_id;
    role.modified_by = h.db_user();
    role.change_reason_code = "system.test";
    ores::iam::repository::role_grant_request_role_repository roles(acting(h));
    roles.write(role);
    return id;
}

bool holds(ores::testing::scoped_database_helper& h,
           const ores::iam::domain::account& account,
           const boost::uuids::uuid& role_id) {
    return !ores::database::repository::execute_parameterized_string_query(
                acting(h),
                "select 1::text from ores_iam_account_roles_tbl where account_id = $1::uuid"
                " and role_id = $2::uuid and valid_to = ores_utility_infinity_timestamp_fn()",
                {boost::uuids::to_string(account.id), boost::uuids::to_string(role_id)},
                lg(),
                "Reading a held role")
                .empty();
}

}

using ores::iam::service::role_grant_applier;

TEST_CASE("an_approved_role_request_is_granted_once", tags) {
    ores::testing::scoped_database_helper h(true);
    const auto asker = seed_account(h);
    const auto approver = seed_account(h);
    ores::iam::repository::role_repository roles;
    const auto all = roles.read_latest(acting(h));
    REQUIRE_FALSE(all.empty());
    const auto role_id = all.front().id;
    REQUIRE_FALSE(holds(h, asker, role_id));

    approved_request(h, asker, approver, role_id);

    role_grant_applier applier(h.context(), nullptr);
    CHECK(applier.apply() >= 1);
    CHECK(holds(h, asker, role_id));

    const auto before = ores::database::repository::execute_parameterized_string_query(
        acting(h),
        "select count(*)::text from ores_iam_account_roles_tbl where account_id = $1::uuid",
        {boost::uuids::to_string(asker.id)},
        lg(),
        "Counting role versions");
    applier.apply();
    const auto after = ores::database::repository::execute_parameterized_string_query(
        acting(h),
        "select count(*)::text from ores_iam_account_roles_tbl where account_id = $1::uuid",
        {boost::uuids::to_string(asker.id)},
        lg(),
        "Counting role versions");
    CHECK(before == after);
}

TEST_CASE("a_role_already_held_is_not_granted_again", tags) {
    ores::testing::scoped_database_helper h(true);
    const auto asker = seed_account(h);
    const auto approver = seed_account(h);
    ores::iam::repository::role_repository roles;
    const auto role_id = roles.read_latest(acting(h)).front().id;

    approved_request(h, asker, approver, role_id);
    role_grant_applier applier(h.context(), nullptr);
    applier.apply();
    REQUIRE(holds(h, asker, role_id));

    approved_request(h, asker, approver, role_id);
    const auto before = ores::database::repository::execute_parameterized_string_query(
        acting(h),
        "select count(*)::text from ores_iam_account_roles_tbl where account_id = $1::uuid",
        {boost::uuids::to_string(asker.id)},
        lg(),
        "Counting role versions");
    applier.apply();
    const auto after = ores::database::repository::execute_parameterized_string_query(
        acting(h),
        "select count(*)::text from ores_iam_account_roles_tbl where account_id = $1::uuid",
        {boost::uuids::to_string(asker.id)},
        lg(),
        "Counting role versions");
    CHECK(before == after);
}
