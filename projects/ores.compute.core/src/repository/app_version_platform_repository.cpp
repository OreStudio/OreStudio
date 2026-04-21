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
#include "ores.compute.core/repository/app_version_platform_repository.hpp"

#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::compute::repository {

namespace {

auto& lg() {
    using namespace ores::logging;
    static auto instance = make_logger(
        "ores.compute.repository.app_version_platform_repository");
    return instance;
}

}

std::vector<domain::app_version_platform>
app_version_platform_repository::list_for_version(database::context ctx,
    const std::string& app_version_id) {

    const std::string sql =
        "SELECT avp.app_version_id::text,"
        "       avp.platform_id::text,"
        "       p.code,"
        "       avp.package_uri"
        "  FROM ores_compute_app_version_platforms_tbl avp"
        "  JOIN ores_compute_platforms_tbl p"
        "    ON p.id = avp.platform_id"
        "   AND p.valid_to = ores_utility_infinity_timestamp_fn()"
        " WHERE avp.app_version_id = $1::uuid"
        "   AND avp.valid_to = ores_utility_infinity_timestamp_fn()";

    const auto rows = ores::database::repository::
        execute_parameterized_multi_column_query(
            ctx, sql, {app_version_id}, lg(),
            "Listing platforms for app version " + app_version_id);

    std::vector<domain::app_version_platform> r;
    r.reserve(rows.size());
    for (const auto& row : rows) {
        domain::app_version_platform avp;
        avp.tenant_id = ctx.tenant_id();
        avp.app_version_id = boost::lexical_cast<boost::uuids::uuid>(
            row[0].value_or(""));
        avp.platform_id = boost::lexical_cast<boost::uuids::uuid>(
            row[1].value_or(""));
        avp.platform_code = row[2].value_or("");
        avp.package_uri = row[3].value_or("");
        r.push_back(std::move(avp));
    }
    return r;
}

void app_version_platform_repository::replace_for_version(
    database::context ctx, const std::string& app_version_id,
    const std::vector<domain::app_version_platform>& rows,
    const std::string& modified_by, const std::string& performed_by,
    const std::string& change_reason_code,
    const std::string& change_commentary) {

    const auto tenant_id_str = ctx.tenant_id().to_string();

    // Soft-close all current active rows for this app version so platforms
    // removed from @p rows disappear from the active set. Rows in @p rows
    // are re-inserted below; the insert trigger takes care of bitemporal
    // bookkeeping.
    ores::database::repository::execute_parameterized_command(ctx,
        "UPDATE ores_compute_app_version_platforms_tbl"
        "   SET valid_to = current_timestamp"
        " WHERE tenant_id = $1::uuid"
        "   AND app_version_id = $2::uuid"
        "   AND valid_to = ores_utility_infinity_timestamp_fn()",
        {tenant_id_str, app_version_id}, lg(),
        "Closing existing platform rows for app version " + app_version_id);

    for (const auto& row : rows) {
        const auto platform_id_str = boost::uuids::to_string(row.platform_id);
        ores::database::repository::execute_parameterized_command(ctx,
            "INSERT INTO ores_compute_app_version_platforms_tbl"
            " (tenant_id, app_version_id, platform_id, package_uri,"
            "  modified_by, performed_by,"
            "  change_reason_code, change_commentary,"
            "  valid_from, valid_to)"
            " VALUES ($1::uuid, $2::uuid, $3::uuid, $4,"
            "         $5, $6, $7, $8,"
            "         now(), ores_utility_infinity_timestamp_fn())",
            {tenant_id_str, app_version_id, platform_id_str, row.package_uri,
             modified_by, performed_by,
             change_reason_code, change_commentary},
            lg(), "Inserting platform row for app version " + app_version_id);
    }
}

}
