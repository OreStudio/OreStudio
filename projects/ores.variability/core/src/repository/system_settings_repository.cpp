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
#include "ores.variability.core/repository/system_settings_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.variability.api/domain/system_setting_json_io.hpp" // IWYU pragma: keep.
#include "ores.variability.core/repository/system_setting_entity.hpp"
#include "ores.variability.core/repository/system_setting_mapper.hpp"
#include <unordered_map>

namespace ores::variability::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string system_settings_repository::sql() {
    return generate_create_table_sql<system_setting_entity>(lg());
}

void system_settings_repository::write(context ctx, const domain::system_setting& setting) {
    BOOST_LOG_SEV(lg(), debug) << "Writing system setting to database: " << setting;

    execute_write_query(
        ctx, system_setting_mapper::map(setting), lg(), "writing system setting to database");
}

void system_settings_repository::write(context ctx,
                                       const std::vector<domain::system_setting>& settings) {
    BOOST_LOG_SEV(lg(), debug) << "Writing system settings to database. Count: " << settings.size();

    execute_write_query(
        ctx, system_setting_mapper::map(settings), lg(), "writing system settings to database");
}

std::vector<domain::system_setting> system_settings_repository::read_latest(context ctx) {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<system_setting_entity>> |
                       where("valid_to"_c == max.value()) | order_by("valid_from"_c.desc());

    return execute_read_query<system_setting_entity, domain::system_setting>(
        ctx,
        query,
        [](const auto& entities) { return system_setting_mapper::map(entities); },
        lg(),
        "Reading latest system settings");
}

std::vector<domain::system_setting>
system_settings_repository::read_latest(context ctx, const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest system setting by name: " << name;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<system_setting_entity>> |
                       where("name"_c == name && "valid_to"_c == max.value()) |
                       order_by("valid_from"_c.desc());

    return execute_read_query<system_setting_entity, domain::system_setting>(
        ctx,
        query,
        [](const auto& entities) { return system_setting_mapper::map(entities); },
        lg(),
        "Reading latest system setting by name");
}

std::vector<domain::system_setting>
system_settings_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest system settings with offset: " << offset
                               << " limit: " << limit;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<system_setting_entity>> |
                       where("valid_to"_c == max.value()) | order_by("valid_from"_c.desc()) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<system_setting_entity, domain::system_setting>(
        ctx,
        query,
        [](const auto& entities) { return system_setting_mapper::map(entities); },
        lg(),
        "Reading latest system settings with pagination");
}

std::uint32_t system_settings_repository::get_total_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active system settings count";

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<system_setting_entity>(sqlgen::count().as<"count">()) |
                       where("valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active system settings count: " << count;
    return count;
}

std::vector<domain::system_setting> system_settings_repository::read_all(context ctx) {
    const auto query =
        sqlgen::read<std::vector<system_setting_entity>> | order_by("valid_from"_c.desc());

    return execute_read_query<system_setting_entity, domain::system_setting>(
        ctx,
        query,
        [](const auto& entities) { return system_setting_mapper::map(entities); },
        lg(),
        "Reading all system settings");
}

std::vector<domain::system_setting> system_settings_repository::read_all(context ctx,
                                                                         const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all versions of system setting by name: " << name;

    const auto query = sqlgen::read<std::vector<system_setting_entity>> | where("name"_c == name) |
                       order_by("valid_from"_c.desc());

    return execute_read_query<system_setting_entity, domain::system_setting>(
        ctx,
        query,
        [](const auto& entities) { return system_setting_mapper::map(entities); },
        lg(),
        "Reading all system settings by name");
}

std::unordered_map<std::string, std::string>
system_settings_repository::read_for_tenant(context ctx, const std::string& tenant_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading system settings for tenant: " << tenant_id;

    const auto rows = execute_parameterized_multi_column_query(
        ctx,
        "SELECT setting_name, setting_value"
        " FROM ores_variability_get_system_settings_fn($1)",
        {tenant_id},
        lg(),
        "Reading system settings for tenant via SECURITY DEFINER");

    std::unordered_map<std::string, std::string> result;
    result.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() >= 2 && row[0] && row[1])
            result[*row[0]] = *row[1];
    }

    BOOST_LOG_SEV(lg(), debug) << "Loaded " << result.size() << " settings for tenant "
                               << tenant_id;
    return result;
}

std::unordered_map<std::string, std::string>
system_settings_repository::read_for_party(context ctx,
                                           const std::string& tenant_id,
                                           const std::string& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading system settings for tenant: " << tenant_id
                               << " party: " << party_id;

    const auto rows = execute_parameterized_multi_column_query(
        ctx,
        "SELECT setting_name, setting_value"
        " FROM ores_variability_get_system_settings_fn($1, $2)",
        {tenant_id, party_id},
        lg(),
        "Reading system settings for tenant+party via SECURITY DEFINER");

    std::unordered_map<std::string, std::string> result;
    result.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() >= 2 && row[0] && row[1])
            result[*row[0]] = *row[1];
    }

    BOOST_LOG_SEV(lg(), debug) << "Loaded " << result.size() << " settings for tenant "
                               << tenant_id << " party " << party_id;
    return result;
}

void system_settings_repository::remove(context ctx, const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Removing system setting from database: " << name;

    // The database trigger closes the temporal record rather than deleting it.
    const auto query = sqlgen::delete_from<system_setting_entity> | where("name"_c == name);

    execute_delete_query(ctx, query, lg(), "Removing system setting from database");
}

}
