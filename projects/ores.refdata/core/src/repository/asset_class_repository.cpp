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
#include "ores.refdata.core/repository/asset_class_repository.hpp"

#include "ores.database/repository/bitemporal_operations.hpp"

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

std::vector<domain::asset_class_info>
asset_class_repository::read_latest(context ctx,
    const std::string& coding_scheme,
    std::uint32_t offset, std::uint32_t limit) {

    BOOST_LOG_SEV(lg(), debug)
        << "Reading asset classes. scheme=" << coding_scheme
        << " offset=" << offset << " limit=" << limit;

    const auto tid = ctx.tenant_id().to_string();

    std::string sql =
        "SELECT code, description, coding_scheme_code "
        "FROM ores_refdata_asset_classes_tbl "
        "WHERE tenant_id = $1 "
        "  AND valid_to = ores_utility_infinity_timestamp_fn()";

    std::vector<std::string> params = {tid};

    if (!coding_scheme.empty()) {
        sql += " AND coding_scheme_code = $2";
        params.push_back(coding_scheme);
        sql += " ORDER BY code"
               " OFFSET $3 LIMIT $4";
        params.push_back(std::to_string(offset));
        params.push_back(std::to_string(limit));
    } else {
        sql += " ORDER BY coding_scheme_code, code"
               " OFFSET $2 LIMIT $3";
        params.push_back(std::to_string(offset));
        params.push_back(std::to_string(limit));
    }

    const auto rows = execute_parameterized_multi_column_query(
        ctx, sql, params, lg(), "Reading asset classes");

    std::vector<domain::asset_class_info> result;
    result.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() < 3) continue;
        domain::asset_class_info info;
        info.code = row[0].value_or("");
        info.description = row[1].value_or("");
        info.coding_scheme_code = row[2].value_or("");
        result.push_back(std::move(info));
    }

    BOOST_LOG_SEV(lg(), debug) << "Read " << result.size() << " asset classes";
    return result;
}

std::uint32_t
asset_class_repository::count_latest(context ctx,
    const std::string& coding_scheme) {

    const auto tid = ctx.tenant_id().to_string();

    std::string sql =
        "SELECT count(*) FROM ores_refdata_asset_classes_tbl "
        "WHERE tenant_id = $1 "
        "  AND valid_to = ores_utility_infinity_timestamp_fn()";

    std::vector<std::string> params = {tid};
    if (!coding_scheme.empty()) {
        sql += " AND coding_scheme_code = $2";
        params.push_back(coding_scheme);
    }

    const auto rows = execute_parameterized_multi_column_query(
        ctx, sql, params, lg(), "Counting asset classes");

    if (rows.empty() || rows[0].empty() || !rows[0][0].has_value())
        return 0;
    return static_cast<std::uint32_t>(std::stoll(*rows[0][0]));
}

}
