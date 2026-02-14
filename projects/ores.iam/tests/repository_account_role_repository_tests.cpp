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
#include "ores.iam/repository/account_role_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.iam/domain/account_role.hpp"
#include "ores.iam/domain/account.hpp"
#include "ores.iam/domain/role.hpp"
#include "ores.iam/repository/account_repository.hpp"
#include "ores.iam/repository/role_repository.hpp"
#include "ores.iam/generators/account_generator.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[repository]");

using ores::iam::domain::role;
using ores::iam::domain::account_role;

role make_role(ores::testing::database_helper& h) {
    role r;
    r.version = 1;
    r.tenant_id = h.tenant_id();
    r.id = boost::uuids::random_generator()();
    r.name = std::string(faker::word::noun()) + "_"
        + std::string(faker::string::alphanumeric(6));
    r.description = std::string(faker::lorem::sentence());
    r.modified_by = h.db_user();
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.performed_by = h.db_user();
    return r;
}

}

using namespace ores::logging;
using namespace ores::iam::generators;

using ores::testing::database_helper;
using ores::iam::repository::account_role_repository;
using ores::iam::repository::account_repository;
using ores::iam::repository::role_repository;

TEST_CASE("write_single_account_role", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    account_repository acc_repo(h.context());
    role_repository role_repo(h.context());
    account_role_repository repo(h.context());

    auto acc = generate_synthetic_account(ctx);
    auto r = make_role(h);
    acc_repo.write(acc);
    role_repo.write(r);

    account_role ar;
    ar.tenant_id = h.tenant_id();
    ar.account_id = acc.id;
    ar.role_id = r.id;
    ar.assigned_by = h.db_user();
    ar.change_reason_code = "system.test";
    ar.change_commentary = "Synthetic test data";

    BOOST_LOG_SEV(lg, debug) << "Account role - account_id: " << ar.account_id
                             << " role_id: " << ar.role_id;
    CHECK_NOTHROW(repo.write(ar));
}

TEST_CASE("read_latest_account_roles", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    account_repository acc_repo(h.context());
    role_repository role_repo(h.context());
    account_role_repository repo(h.context());

    auto acc = generate_synthetic_account(ctx);
    auto r = make_role(h);
    acc_repo.write(acc);
    role_repo.write(r);

    account_role ar;
    ar.tenant_id = h.tenant_id();
    ar.account_id = acc.id;
    ar.role_id = r.id;
    ar.assigned_by = h.db_user();
    ar.change_reason_code = "system.test";
    ar.change_commentary = "Synthetic test data";
    repo.write(ar);

    auto read_ars = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read account roles count: "
                             << read_ars.size();

    CHECK(!read_ars.empty());
}

TEST_CASE("read_latest_account_roles_by_account", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    account_repository acc_repo(h.context());
    role_repository role_repo(h.context());
    account_role_repository repo(h.context());

    auto acc = generate_synthetic_account(ctx);
    auto r1 = make_role(h);
    auto r2 = make_role(h);
    acc_repo.write(acc);
    role_repo.write(r1);
    role_repo.write(r2);

    account_role ar1;
    ar1.tenant_id = h.tenant_id();
    ar1.account_id = acc.id;
    ar1.role_id = r1.id;
    ar1.assigned_by = h.db_user();
    ar1.change_reason_code = "system.test";
    ar1.change_commentary = "Synthetic test data";

    account_role ar2;
    ar2.tenant_id = h.tenant_id();
    ar2.account_id = acc.id;
    ar2.role_id = r2.id;
    ar2.assigned_by = h.db_user();
    ar2.change_reason_code = "system.test";
    ar2.change_commentary = "Synthetic test data";

    repo.write(ar1);
    repo.write(ar2);

    auto read_ars = repo.read_latest_by_account(acc.id);
    BOOST_LOG_SEV(lg, debug) << "Read account roles for account "
                             << acc.id << " count: " << read_ars.size();

    CHECK(read_ars.size() >= 2);
    for (const auto& ar : read_ars) {
        CHECK(ar.account_id == acc.id);
    }
}
