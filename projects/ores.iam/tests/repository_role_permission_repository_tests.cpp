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
#include "ores.iam/repository/role_permission_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.iam/domain/role_permission.hpp"
#include "ores.iam/domain/role.hpp"
#include "ores.iam/domain/permission.hpp"
#include "ores.iam/repository/role_repository.hpp"
#include "ores.iam/repository/permission_repository.hpp"
#include "ores.iam/generators/role_generator.hpp"
#include "ores.iam/generators/permission_generator.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[repository]");

using ores::iam::domain::role_permission;

}

using namespace ores::logging;
using namespace ores::iam::generators;

using ores::testing::database_helper;
using ores::iam::repository::role_permission_repository;
using ores::iam::repository::role_repository;
using ores::iam::repository::permission_repository;

TEST_CASE("write_single_role_permission", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    role_repository role_repo(h.context());
    permission_repository perm_repo(h.context());
    role_permission_repository repo(h.context());

    auto r = generate_synthetic_role(ctx);
    auto p = generate_synthetic_permission(ctx);
    role_repo.write(r);
    perm_repo.write(p);

    role_permission rp;
    rp.tenant_id = h.tenant_id();
    rp.role_id = r.id;
    rp.permission_id = p.id;

    BOOST_LOG_SEV(lg, debug) << "Role permission - role_id: " << rp.role_id
                             << " permission_id: " << rp.permission_id;
    CHECK_NOTHROW(repo.write(rp));
}

TEST_CASE("read_latest_role_permissions", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    role_repository role_repo(h.context());
    permission_repository perm_repo(h.context());
    role_permission_repository repo(h.context());

    auto r = generate_synthetic_role(ctx);
    auto p = generate_synthetic_permission(ctx);
    role_repo.write(r);
    perm_repo.write(p);

    role_permission rp;
    rp.tenant_id = h.tenant_id();
    rp.role_id = r.id;
    rp.permission_id = p.id;
    repo.write(rp);

    auto read_rps = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read role permissions count: "
                             << read_rps.size();

    CHECK(!read_rps.empty());
}

TEST_CASE("read_latest_role_permissions_by_role", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    role_repository role_repo(h.context());
    permission_repository perm_repo(h.context());
    role_permission_repository repo(h.context());

    auto r = generate_synthetic_role(ctx);
    auto p1 = generate_synthetic_permission(ctx);
    auto p2 = generate_synthetic_permission(ctx);
    role_repo.write(r);
    perm_repo.write(p1);
    perm_repo.write(p2);

    role_permission rp1;
    rp1.tenant_id = h.tenant_id();
    rp1.role_id = r.id;
    rp1.permission_id = p1.id;

    role_permission rp2;
    rp2.tenant_id = h.tenant_id();
    rp2.role_id = r.id;
    rp2.permission_id = p2.id;

    repo.write(rp1);
    repo.write(rp2);

    auto read_rps = repo.read_latest_by_role(r.id);
    BOOST_LOG_SEV(lg, debug) << "Read role permissions for role "
                             << r.id << " count: " << read_rps.size();

    CHECK(read_rps.size() >= 2);
    for (const auto& rp : read_rps) {
        CHECK(rp.role_id == r.id);
    }
}
