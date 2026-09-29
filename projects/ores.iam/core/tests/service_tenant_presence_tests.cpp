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

#include "ores.iam.api/domain/tenant.hpp"
#include "ores.iam.core/service/tenant_presence.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[tenant]");

using ores::iam::domain::tenant;
using ores::iam::service::has_tenant_of_its_own;
using ores::utility::uuid::tenant_id;

/**
 * A row of the tenant registry, in the shape the table holds.
 *
 * The table's check requires every row to carry the system tenant in tenant_id,
 * because the registry is the system tenant's own: a row names the tenant it
 * describes in id and never in tenant_id. A test that put a tenant's own id in
 * tenant_id would describe a table the deployment does not have, and would pass
 * against a predicate that reads the wrong column.
 */
tenant registry_row(const std::string& id, const std::string& code) {
    tenant row;
    row.id = boost::uuids::string_generator()(id);
    row.tenant_id = tenant_id::system();
    row.code = code;
    return row;
}

const std::string system_row_id("ffffffff-ffff-ffff-ffff-ffffffffffff");
const std::string set_up_tenant_row_id("719e8f00-aa85-456e-a43b-735a6f1ce4e0");

}

TEST_CASE("has_tenant_of_its_own_is_false_for_a_deployment_with_only_the_system_tenant", tags) {
    CHECK_FALSE(has_tenant_of_its_own({}));
    CHECK_FALSE(has_tenant_of_its_own({registry_row(system_row_id, "system")}));
}

TEST_CASE("has_tenant_of_its_own_is_true_for_a_tenant_somebody_set_up", tags) {
    const std::vector<tenant> registry = {
        registry_row(system_row_id, "system"),
        registry_row(set_up_tenant_row_id, "barclays_plc"),
    };

    CHECK(has_tenant_of_its_own(registry));
}
