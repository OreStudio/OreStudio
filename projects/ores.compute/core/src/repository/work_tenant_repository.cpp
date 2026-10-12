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
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <string>

namespace ores::compute::repository {

using namespace ores::logging;
using namespace ores::database::repository;

void work_tenant_repository::note(context ctx, const boost::uuids::uuid& work_tenant_id) {
    static const std::string sql = R"(
        INSERT INTO ores_compute_work_tenants_tbl (work_tenant_id)
        VALUES ($1::uuid)
        ON CONFLICT (work_tenant_id) DO NOTHING
    )";
    execute_parameterized_command(ctx,
                                  sql,
                                  {boost::uuids::to_string(work_tenant_id)},
                                  lg(),
                                  "noting a tenant that dispatches work");
}

std::vector<boost::uuids::uuid> work_tenant_repository::list(context ctx) {
    static const std::string sql =
        "SELECT work_tenant_id::text FROM ores_compute_work_tenants_tbl ORDER BY first_seen_at";
    const auto rows =
        execute_parameterized_multi_column_query(ctx, sql, {}, lg(), "listing the tenants with work");
    std::vector<boost::uuids::uuid> tenants;
    for (const auto& row : rows) {
        if (!row.empty() && row.front())
            tenants.push_back(boost::lexical_cast<boost::uuids::uuid>(*row.front()));
    }
    return tenants;
}

}
