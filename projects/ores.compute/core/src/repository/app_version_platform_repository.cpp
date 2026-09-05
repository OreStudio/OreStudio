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
#include "ores.compute.api/domain/app_version_platform_json_io.hpp" // IWYU pragma: keep.
#include "ores.compute.core/repository/app_version_platform_entity.hpp"
#include "ores.compute.core/repository/app_version_platform_mapper.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/postgres.hpp>
#include <unordered_map>

namespace ores::compute::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string app_version_platform_repository::sql() {
    return generate_create_table_sql<app_version_platform_entity>(lg());
}

app_version_platform_repository::app_version_platform_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void app_version_platform_repository::write(
    const domain::app_version_platform& app_version_platform) {
    BOOST_LOG_SEV(lg(), debug) << "Writing app version platform to database: "
                               << app_version_platform.app_version_id << "/"
                               << app_version_platform.platform_id;
    execute_write_query(ctx_,
                        app_version_platform_mapper::map(app_version_platform),
                        lg(),
                        "writing app version platform to database");
}

void app_version_platform_repository::write(
    const std::vector<domain::app_version_platform>& app_version_platforms) {
    BOOST_LOG_SEV(lg(), debug) << "Writing app version platforms to database. Count: "
                               << app_version_platforms.size();
    execute_write_query(ctx_,
                        app_version_platform_mapper::map(app_version_platforms),
                        lg(),
                        "writing app version platforms to database");
}

std::vector<domain::app_version_platform> app_version_platform_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<app_version_platform_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("app_version_id"_c, "platform_id"_c);

    return execute_read_query<app_version_platform_entity, domain::app_version_platform>(
        ctx_,
        query,
        [](const auto& entities) { return app_version_platform_mapper::map(entities); },
        lg(),
        "Reading latest app version platforms");
}

std::vector<domain::app_version_platform>
app_version_platform_repository::read_latest_by_app_version(
    const boost::uuids::uuid& app_version_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest app version platforms. App Version: "
                               << app_version_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto app_version_id_str = boost::uuids::to_string(app_version_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<app_version_platform_entity>> |
                       where("tenant_id"_c == tid && "app_version_id"_c == app_version_id_str &&
                             "valid_to"_c == max.value()) |
                       order_by("platform_id"_c);

    auto rows = execute_read_query<app_version_platform_entity, domain::app_version_platform>(
        ctx_,
        query,
        [](const auto& entities) { return app_version_platform_mapper::map(entities); },
        lg(),
        "Reading latest app version platforms by app_version.");

    if (!rows.empty()) {
        std::unordered_map<std::string, std::string> code_by_id;
        std::string ids = "{";
        for (std::size_t i = 0; i < rows.size(); ++i) {
            if (i > 0)
                ids += ",";
            ids += boost::uuids::to_string(rows[i].platform_id);
        }
        ids += "}";
        const auto codes = execute_parameterized_multi_column_query(
            ctx_,
            "SELECT \"id\"::text, \"code\"::text"
            "  FROM ores_compute_platforms_tbl"
            " WHERE \"valid_to\" = ores_utility_infinity_timestamp_fn()"
            "   AND \"id\" = ANY($1::uuid[])",
            {ids},
            lg(),
            "Reading platform codes for app version platforms");
        code_by_id.reserve(codes.size());
        for (const auto& code_row : codes) {
            if (code_row[0] && code_row[1])
                code_by_id.emplace(*code_row[0], *code_row[1]);
        }
        for (auto& row : rows) {
            if (const auto it = code_by_id.find(boost::uuids::to_string(row.platform_id));
                it != code_by_id.end())
                row.platform_code = it->second;
        }
    }
    return rows;
}

std::vector<domain::app_version_platform>
app_version_platform_repository::read_latest_by_platform(const boost::uuids::uuid& platform_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest app version platforms. Platform: " << platform_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto platform_id_str = boost::uuids::to_string(platform_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<app_version_platform_entity>> |
                       where("tenant_id"_c == tid && "platform_id"_c == platform_id_str &&
                             "valid_to"_c == max.value()) |
                       order_by("app_version_id"_c);

    auto rows = execute_read_query<app_version_platform_entity, domain::app_version_platform>(
        ctx_,
        query,
        [](const auto& entities) { return app_version_platform_mapper::map(entities); },
        lg(),
        "Reading latest app version platforms by platform.");

    return rows;
}

std::vector<domain::app_version_platform>
app_version_platform_repository::read_latest_by_app_version(
    const boost::uuids::uuid& app_version_id, std::uint32_t offset, std::uint32_t limit) {
    const auto app_version_id_str = boost::uuids::to_string(app_version_id);
    BOOST_LOG_SEV(lg(), debug) << "Reading latest app version platforms. App Version: "
                               << app_version_id << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<app_version_platform_entity>> |
                       where("tenant_id"_c == tid && "app_version_id"_c == app_version_id_str &&
                             "valid_to"_c == max.value()) |
                       order_by("platform_id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    auto rows = execute_read_query<app_version_platform_entity, domain::app_version_platform>(
        ctx_,
        query,
        [](const auto& entities) { return app_version_platform_mapper::map(entities); },
        lg(),
        "Reading latest app version platforms by app_version (paginated).");

    if (!rows.empty()) {
        std::unordered_map<std::string, std::string> code_by_id;
        std::string ids = "{";
        for (std::size_t i = 0; i < rows.size(); ++i) {
            if (i > 0)
                ids += ",";
            ids += boost::uuids::to_string(rows[i].platform_id);
        }
        ids += "}";
        const auto codes = execute_parameterized_multi_column_query(
            ctx_,
            "SELECT \"id\"::text, \"code\"::text"
            "  FROM ores_compute_platforms_tbl"
            " WHERE \"valid_to\" = ores_utility_infinity_timestamp_fn()"
            "   AND \"id\" = ANY($1::uuid[])",
            {ids},
            lg(),
            "Reading platform codes for app version platforms");
        code_by_id.reserve(codes.size());
        for (const auto& code_row : codes) {
            if (code_row[0] && code_row[1])
                code_by_id.emplace(*code_row[0], *code_row[1]);
        }
        for (auto& row : rows) {
            if (const auto it = code_by_id.find(boost::uuids::to_string(row.platform_id));
                it != code_by_id.end())
                row.platform_code = it->second;
        }
    }
    return rows;
}

std::uint32_t app_version_platform_repository::get_total_app_version_platform_count_by_app_version(
    const boost::uuids::uuid& app_version_id) {
    const auto app_version_id_str = boost::uuids::to_string(app_version_id);
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active app version platforms count. App Version: " << app_version_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<app_version_platform_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "app_version_id"_c == app_version_id_str &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active app version platforms count by app_version: "
                               << count;
    return count;
}

void app_version_platform_repository::remove(const boost::uuids::uuid& app_version_id,
                                             const boost::uuids::uuid& platform_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing app version platform from database: " << app_version_id
                               << "/" << platform_id;

    const auto app_version_id_str = boost::uuids::to_string(app_version_id);
    const auto platform_id_str = boost::uuids::to_string(platform_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<app_version_platform_entity> |
                       where("tenant_id"_c == tid && "app_version_id"_c == app_version_id_str &&
                             "platform_id"_c == platform_id_str);

    execute_delete_query(ctx_, query, lg(), "removing app version platform from database");
}

void app_version_platform_repository::remove_by_app_version(
    const boost::uuids::uuid& app_version_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all app version platforms from database: "
                               << app_version_id;

    const auto app_version_id_str = boost::uuids::to_string(app_version_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<app_version_platform_entity> |
                       where("tenant_id"_c == tid && "app_version_id"_c == app_version_id_str);

    execute_delete_query(ctx_, query, lg(), "removing all app version platforms from database");
}

void app_version_platform_repository::replace_by_app_version(
    const boost::uuids::uuid& app_version_id,
    const std::vector<domain::app_version_platform>& app_version_platforms,
    const std::string& modified_by,
    const std::string& performed_by,
    const std::string& change_reason_code,
    const std::string& change_commentary) {

    BOOST_LOG_SEV(lg(), debug) << "Replacing app version platforms for app version: "
                               << app_version_id;
    const auto app_version_id_str = boost::uuids::to_string(app_version_id);
    const auto tid = ctx_.tenant_id().to_string();

    // Soft-close the currently active rows for this side so rows absent
    // from the new set disappear from the active set. Rows in @p
    // app_version_platforms are re-inserted below; the insert trigger takes care
    // of the bitemporal bookkeeping.
    execute_parameterized_command(ctx_,
                                  "UPDATE ores_compute_app_version_platforms_tbl"
                                  "   SET valid_to = current_timestamp"
                                  " WHERE tenant_id = $1::uuid"
                                  "   AND app_version_id = $2::uuid"
                                  "   AND valid_to = ores_utility_infinity_timestamp_fn()",
                                  {tid, app_version_id_str},
                                  lg(),
                                  "Closing existing app version platforms for app version " +
                                      app_version_id_str);

    for (auto app_version_platform : app_version_platforms) {
        app_version_platform.modified_by = modified_by;
        app_version_platform.performed_by = performed_by;
        app_version_platform.change_reason_code = change_reason_code;
        app_version_platform.change_commentary = change_commentary;
        write(app_version_platform);
    }
}
}
