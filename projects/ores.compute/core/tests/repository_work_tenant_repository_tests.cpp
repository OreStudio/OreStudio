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
#include "ores.compute.core/repository/work_tenant_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

using ores::compute::repository::work_tenant_repository;
using ores::testing::database_helper;
using ores::utility::uuid::tenant_id;

/// Removes the tenant a test noted, so the development database does not keep it.
struct noted_tenant {
    database_helper& h;
    boost::uuids::uuid id = boost::uuids::random_generator()();

    ores::database::context system() const {
        return h.context().with_tenant(tenant_id::system(), h.db_user());
    }

    ~noted_tenant() {
        try {
            static auto lg = ores::logging::make_logger("ores.compute.tests.work_tenant");
            ores::database::repository::execute_parameterized_command(
                system(),
                "DELETE FROM ores_compute_work_tenants_tbl WHERE work_tenant_id = $1::uuid",
                {boost::uuids::to_string(id)},
                lg,
                "deleting a test tenant");
        } catch (const std::exception&) {
            // A failed delete leaves a row behind and must not fail the test.
        }
    }
};

}

TEST_CASE("note_lists_a_tenant_once_however_often_it_is_noted", "[repository][work_tenant]") {
    database_helper h;
    noted_tenant noted{h};
    work_tenant_repository repo;

    repo.note(noted.system(), noted.id);
    repo.note(noted.system(), noted.id);

    const auto tenants = repo.list(noted.system());
    CHECK(std::ranges::count(tenants, noted.id) == 1);
}

TEST_CASE("list_is_empty_of_a_tenant_that_was_not_noted", "[repository][work_tenant]") {
    database_helper h;
    noted_tenant noted{h};

    const auto tenants = work_tenant_repository().list(noted.system());

    CHECK(std::ranges::count(tenants, noted.id) == 0);
}

TEST_CASE("a_tenants_session_reads_none_of_the_list", "[repository][work_tenant]") {
    database_helper h;
    noted_tenant noted{h};
    work_tenant_repository repo;
    repo.note(noted.system(), noted.id);

    // The list belongs to the installation: a tenant's own session sees no row of it.
    CHECK(repo.list(h.context()).empty());
}
