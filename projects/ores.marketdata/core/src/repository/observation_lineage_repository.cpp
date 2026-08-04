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
#include "ores.marketdata.core/repository/observation_lineage_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.marketdata.api/domain/observation_lineage_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/observation_lineage_entity.hpp"
#include "ores.marketdata.core/repository/observation_lineage_mapper.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <sqlgen/postgres.hpp>

namespace ores::marketdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string observation_lineage_repository::sql() {
    return generate_create_table_sql<observation_lineage_entity>(lg());
}

void observation_lineage_repository::write(context ctx, const domain::observation_lineage& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing observation lineage. " << "id: " << v.id;
    execute_write_query(
        ctx, observation_lineage_mapper::map(v), lg(), "Writing observation lineage to database.");
}

void observation_lineage_repository::write(context ctx,
                                           const std::vector<domain::observation_lineage>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing observation lineages. Count: " << v.size();
    execute_write_query(
        ctx, observation_lineage_mapper::map(v), lg(), "Writing observation lineages to database.");
}

std::vector<domain::observation_lineage> observation_lineage_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<observation_lineage_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<observation_lineage_entity, domain::observation_lineage>(
        ctx,
        query,
        [](const auto& entities) { return observation_lineage_mapper::map(entities); },
        lg(),
        "Reading latest observation lineages");
}

std::vector<domain::observation_lineage>
observation_lineage_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest observation lineage. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<observation_lineage_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<observation_lineage_entity, domain::observation_lineage>(
        ctx,
        query,
        [](const auto& entities) { return observation_lineage_mapper::map(entities); },
        lg(),
        "Reading latest observation lineage by id.");
}


std::vector<domain::observation_lineage>
observation_lineage_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all observation lineage versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<observation_lineage_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<observation_lineage_entity, domain::observation_lineage>(
        ctx,
        query,
        [](const auto& entities) { return observation_lineage_mapper::map(entities); },
        lg(),
        "Reading all observation lineage versions by id.");
}

std::optional<domain::observation_lineage> observation_lineage_repository::read_at_version(
    context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading observation lineage at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<observation_lineage_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities =
        execute_read_query<observation_lineage_entity, domain::observation_lineage>(
            ctx,
            query,
            [](const auto& entities) { return observation_lineage_mapper::map(entities); },
            lg(),
            "Reading observation lineage at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void observation_lineage_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing observation lineage. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<observation_lineage_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing observation lineage from database.");
}

std::vector<domain::observation_lineage> observation_lineage_repository::read_latest(
    context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest observation lineages with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<observation_lineage_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<observation_lineage_entity, domain::observation_lineage>(
        ctx,
        query,
        [](const auto& entities) { return observation_lineage_mapper::map(entities); },
        lg(),
        "Reading latest observation lineages with pagination.");
}

std::uint32_t observation_lineage_repository::get_total_observation_lineage_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active observation lineage count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<observation_lineage_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active observation lineage count: " << count;
    return count;
}

void observation_lineage_repository::remove(context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<observation_lineage_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing observation lineages.");
}


std::optional<domain::observation_lineage>
observation_lineage_repository::read_latest_by_observation(
    context ctx,
    const boost::uuids::uuid& series_id,
    std::chrono::system_clock::time_point observation_datetime,
    const std::string& point_id) {
    using ores::platform::time::datetime;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto sid = boost::uuids::to_string(series_id);
    const auto odt_str = datetime::to_iso8601_utc(observation_datetime);

    const auto query =
        sqlgen::read<std::vector<observation_lineage_entity>> |
        where("tenant_id"_c == tid && "series_id"_c == sid && "observation_datetime"_c == odt_str &&
              "point_id"_c == point_id && "valid_to"_c == max.value()) |
        order_by("id"_c);
    const auto results =
        execute_read_query<observation_lineage_entity, domain::observation_lineage>(
            ctx,
            query,
            [](const auto& entities) { return observation_lineage_mapper::map(entities); },
            lg(),
            "Reading latest observation lineage by observation");
    if (results.empty())
        return std::nullopt;
    return results.front();
}

}
