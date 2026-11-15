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
#include "ores.marketdata.core/repository/live_tenant_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include <boost/uuid/string_generator.hpp>

namespace ores::marketdata::repository {


std::set<boost::uuids::uuid> live_tenant_repository::read(const context& ctx) {
    std::set<boost::uuids::uuid> live;
    const boost::uuids::string_generator parse;
    for (const auto& row : ores::database::query_builder::execute_parameterized_multi_column_query(
             ctx, "select id from ores_iam_live_tenant_ids_fn()", {}, lg(), "Reading live tenants"))
        if (!row.empty() && row[0])
            live.insert(parse(*row[0]));
    return live;
}

}
