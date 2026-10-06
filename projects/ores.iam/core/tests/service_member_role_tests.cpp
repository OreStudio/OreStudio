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
#include "ores.logging/make_logger.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[member_role]");

auto& lg() {
    static auto instance = ores::logging::make_logger("ores.iam.core.tests.member_role");
    return instance;
}

ores::database::context acting(ores::testing::scoped_database_helper& h) {
    return h.context().with_tenant(h.tenant_id(), h.db_user());
}

std::vector<std::string> query(ores::testing::scoped_database_helper& h,
                               const std::string& sql,
                               const std::vector<std::string>& params) {
    return ores::database::repository::execute_parameterized_string_query(
        acting(h), sql, params, lg(), sql);
}

void run(ores::testing::scoped_database_helper& h,
         const std::string& sql,
         const std::vector<std::string>& params) {
    ores::database::repository::execute_parameterized_command(acting(h), sql, params, lg(), sql);
}

/**
 * @brief Makes sure the tenant has a Member role, as provisioning copies one
 * into every tenant.
 */
void ensure_member(ores::testing::scoped_database_helper& h) {
    run(h,
        "insert into ores_iam_roles_tbl (id, tenant_id, version, name, description,"
        " modified_by, performed_by, change_reason_code, change_commentary)"
        " select gen_random_uuid(), $1::uuid, 0, 'Member', 'Member', $2, $2,"
        " 'system.test', '' where not exists (select 1 from ores_iam_roles_tbl"
        " where tenant_id = $1::uuid and name = 'Member'"
        " and valid_to = ores_utility_infinity_timestamp_fn())",
        {h.tenant_id().to_string(), h.db_user()});
}

ores::iam::domain::account write_account(ores::testing::scoped_database_helper& h,
                                         const std::string& account_type) {
    auto ctx = ores::testing::make_generation_context(h);
    auto a = ores::iam::generators::generate_synthetic_account(ctx);
    a.account_type = account_type;
    a.change_reason_code = "system.test";
    ores::iam::repository::account_repository repo;
    repo.write(acting(h), a);
    return a;
}

bool holds_member(ores::testing::scoped_database_helper& h,
                  const ores::iam::domain::account& account) {
    return !query(h,
                  "select 1::text from ores_iam_account_roles_tbl ar"
                  " join ores_iam_roles_tbl r on r.id = ar.role_id and r.tenant_id = ar.tenant_id"
                  " and r.valid_to = ores_utility_infinity_timestamp_fn()"
                  " where ar.account_id = $1::uuid and r.name = 'Member'"
                  " and ar.valid_to = ores_utility_infinity_timestamp_fn()",
                  {boost::uuids::to_string(account.id)})
                .empty();
}

}

TEST_CASE("a_person_account_holds_member_from_its_first_version", tags) {
    ores::testing::scoped_database_helper h(true);
    ensure_member(h);

    const auto person = write_account(h, "user");

    CHECK(holds_member(h, person));
}

TEST_CASE("a_service_account_does_not_hold_member", tags) {
    ores::testing::scoped_database_helper h(true);
    ensure_member(h);

    const auto service = write_account(h, "service");

    CHECK_FALSE(holds_member(h, service));
}

TEST_CASE("member_taken_away_is_not_given_back_by_the_next_version", tags) {
    ores::testing::scoped_database_helper h(true);
    ensure_member(h);
    auto person = write_account(h, "user");
    REQUIRE(holds_member(h, person));

    run(h,
        "delete from ores_iam_account_roles_tbl where account_id = $1::uuid and role_id ="
        " (select id from ores_iam_roles_tbl where tenant_id = $2::uuid and name = 'Member'"
        " and valid_to = ores_utility_infinity_timestamp_fn())",
        {boost::uuids::to_string(person.id), h.tenant_id().to_string()});
    REQUIRE_FALSE(holds_member(h, person));

    person.full_name = person.full_name + " Jr";
    person.version = 1;
    ores::iam::repository::account_repository repo;
    repo.write(acting(h), person);

    CHECK_FALSE(holds_member(h, person));
}
