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
#include "ores.refdata.core/repository/business_unit_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/business_unit_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/business_unit_entity.hpp"
#include "ores.refdata.core/repository/business_unit_mapper.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string business_unit_repository::sql() {
    return generate_create_table_sql<business_unit_entity>(lg());
}

void business_unit_repository::write(context ctx, const domain::business_unit& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing business unit. " << "id: " << v.id;
    execute_write_query(
        ctx, business_unit_mapper::map(v), lg(), "Writing business unit to database.");
}

void business_unit_repository::write(context ctx, const std::vector<domain::business_unit>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing business units. Count: " << v.size();
    execute_write_query(
        ctx, business_unit_mapper::map(v), lg(), "Writing business units to database.");
}

std::vector<domain::business_unit> business_unit_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<business_unit_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<business_unit_entity, domain::business_unit>(
        ctx,
        query,
        [](const auto& entities) { return business_unit_mapper::map(entities); },
        lg(),
        "Reading latest business units");
}

std::vector<domain::business_unit> business_unit_repository::read_latest(context ctx,
                                                                         const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest business unit. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<business_unit_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<business_unit_entity, domain::business_unit>(
        ctx,
        query,
        [](const auto& entities) { return business_unit_mapper::map(entities); },
        lg(),
        "Reading latest business unit by id.");
}

std::vector<domain::business_unit> business_unit_repository::read_latest_by_code(
    context ctx, const std::string& party_id, const std::string& unit_name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest business unit by party_id/unit_name: " << party_id
                               << "/" << unit_name;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<business_unit_entity>> |
                       where("tenant_id"_c == tid && "party_id"_c == party_id &&
                             "unit_name"_c == unit_name && "valid_to"_c == max.value());

    return execute_read_query<business_unit_entity, domain::business_unit>(
        ctx,
        query,
        [](const auto& entities) { return business_unit_mapper::map(entities); },
        lg(),
        "Reading latest business unit by unit_name.");
}

std::vector<domain::business_unit> business_unit_repository::read_all(context ctx,
                                                                      const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all business unit versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<business_unit_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<business_unit_entity, domain::business_unit>(
        ctx,
        query,
        [](const auto& entities) { return business_unit_mapper::map(entities); },
        lg(),
        "Reading all business unit versions by id.");
}

std::optional<domain::business_unit> business_unit_repository::read_at_version(
    context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading business unit at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<business_unit_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<business_unit_entity, domain::business_unit>(
        ctx,
        query,
        [](const auto& entities) { return business_unit_mapper::map(entities); },
        lg(),
        "Reading business unit at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}


void business_unit_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing business unit. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<business_unit_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing business unit from database.");
}

std::vector<domain::business_unit>
business_unit_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest business units with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<business_unit_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<business_unit_entity, domain::business_unit>(
        ctx,
        query,
        [](const auto& entities) { return business_unit_mapper::map(entities); },
        lg(),
        "Reading latest business units with pagination.");
}

std::uint32_t business_unit_repository::get_total_business_unit_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active business unit count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<business_unit_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active business unit count: " << count;
    return count;
}

void business_unit_repository::remove(context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<business_unit_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing business units.");
}

std::vector<ores::utility::domain::hierarchy_flat_row> business_unit_repository::get_hierarchy(
    context ctx, const boost::uuids::uuid& root_id, bool from_root) {
    BOOST_LOG_SEV(lg(), debug) << "Reading business_unit hierarchy. Root: " << root_id
                               << " from_root: " << from_root;

    const auto tenant_str = boost::uuids::to_string(ctx.tenant_id().to_uuid());
    const auto root_str = boost::uuids::to_string(root_id);
    const std::string sql = "SELECT * FROM ores_refdata_business_units_hierarchy_fn('" +
                            tenant_str + "'::uuid, '" + root_str + "'::uuid, " +
                            (from_root ? "true" : "false") + ")";

    const auto rows =
        execute_raw_multi_column_query(ctx, sql, lg(), "Reading business_unit hierarchy");

    std::vector<ores::utility::domain::hierarchy_flat_row> result;
    result.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() >= 3 && row[0]) {
            ores::utility::domain::hierarchy_flat_row r;
            r.id = boost::lexical_cast<boost::uuids::uuid>(*row[0]);
            if (row[1])
                r.parent_id = boost::lexical_cast<boost::uuids::uuid>(*row[1]);
            if (row[2])
                r.name = *row[2];
            result.push_back(std::move(r));
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Read " << result.size() << " business_unit hierarchy rows.";
    return result;
}


}
