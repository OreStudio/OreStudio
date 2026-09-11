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
#include "ores.trading.core/repository/instrument_schedule_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.trading.api/domain/instrument_schedule_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/instrument_schedule_entity.hpp"
#include "ores.trading.core/repository/instrument_schedule_mapper.hpp"
#include <sqlgen/postgres.hpp>
#include <stdexcept>

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string instrument_schedule_repository::sql() {
    return generate_create_table_sql<instrument_schedule_entity>(lg());
}

void instrument_schedule_repository::write(context ctx, const domain::instrument_schedule& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing instrument schedule. "
                               << "instrument_id: " << v.instrument_id
                               << " owner_role: " << v.owner_role
                               << " owner_number: " << v.owner_number
                               << " schedule_role: " << v.schedule_role
                               << " sequence_number: " << v.sequence_number;
    execute_write_query(
        ctx, instrument_schedule_mapper::map(v), lg(), "Writing instrument schedule to database.");
}

void instrument_schedule_repository::write(context ctx,
                                           const std::vector<domain::instrument_schedule>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing instrument schedules. Count: " << v.size();
    execute_write_query(
        ctx, instrument_schedule_mapper::map(v), lg(), "Writing instrument schedules to database.");
}

std::vector<domain::instrument_schedule> instrument_schedule_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<instrument_schedule_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("instrument_id"_c,
                                "owner_role"_c,
                                "owner_number"_c,
                                "schedule_role"_c,
                                "sequence_number"_c);

    return execute_read_query<instrument_schedule_entity, domain::instrument_schedule>(
        ctx,
        query,
        [](const auto& entities) { return instrument_schedule_mapper::map(entities); },
        lg(),
        "Reading latest instrument schedules");
}

std::vector<domain::instrument_schedule>
instrument_schedule_repository::read_latest(context ctx,
                                            const std::string& instrument_id,
                                            const std::string& owner_role,
                                            const std::string& owner_number,
                                            const std::string& schedule_role,
                                            const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest instrument schedule. "
                               << "instrument_id: " << instrument_id
                               << " owner_role: " << owner_role << " owner_number: " << owner_number
                               << " schedule_role: " << schedule_role
                               << " sequence_number: " << sequence_number;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<instrument_schedule_entity>> |
                       where("tenant_id"_c == tid && "instrument_id"_c == instrument_id &&
                             "owner_role"_c == owner_role && "owner_number"_c == owner_number &&
                             "schedule_role"_c == schedule_role &&
                             "sequence_number"_c == sequence_number && "valid_to"_c == max.value());

    return execute_read_query<instrument_schedule_entity, domain::instrument_schedule>(
        ctx,
        query,
        [](const auto& entities) { return instrument_schedule_mapper::map(entities); },
        lg(),
        "Reading latest instrument schedule by instrument_id.");
}


std::vector<domain::instrument_schedule>
instrument_schedule_repository::read_all(context ctx,
                                         const std::string& instrument_id,
                                         const std::string& owner_role,
                                         const std::string& owner_number,
                                         const std::string& schedule_role,
                                         const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all instrument schedule versions. "
                               << "instrument_id: " << instrument_id
                               << " owner_role: " << owner_role << " owner_number: " << owner_number
                               << " schedule_role: " << schedule_role
                               << " sequence_number: " << sequence_number;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<instrument_schedule_entity>> |
        where("tenant_id"_c == tid && "instrument_id"_c == instrument_id &&
              "owner_role"_c == owner_role && "owner_number"_c == owner_number &&
              "schedule_role"_c == schedule_role && "sequence_number"_c == sequence_number) |
        order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<instrument_schedule_entity, domain::instrument_schedule>(
        ctx,
        query,
        [](const auto& entities) { return instrument_schedule_mapper::map(entities); },
        lg(),
        "Reading all instrument schedule versions by instrument_id.");
}

std::optional<domain::instrument_schedule>
instrument_schedule_repository::read_at_version(context ctx,
                                                const std::string& instrument_id,
                                                const std::string& owner_role,
                                                const std::string& owner_number,
                                                const std::string& schedule_role,
                                                const std::string& sequence_number,
                                                std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading instrument schedule at version. "
                               << "instrument_id: " << instrument_id
                               << " owner_role: " << owner_role << " owner_number: " << owner_number
                               << " schedule_role: " << schedule_role
                               << " sequence_number: " << sequence_number
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<instrument_schedule_entity>> |
                       where("tenant_id"_c == tid && "instrument_id"_c == instrument_id &&
                             "owner_role"_c == owner_role && "owner_number"_c == owner_number &&
                             "schedule_role"_c == schedule_role &&
                             "sequence_number"_c == sequence_number && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities =
        execute_read_query<instrument_schedule_entity, domain::instrument_schedule>(
            ctx,
            query,
            [](const auto& entities) { return instrument_schedule_mapper::map(entities); },
            lg(),
            "Reading instrument schedule at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void instrument_schedule_repository::remove(context ctx,
                                            const std::string& instrument_id,
                                            const std::string& owner_role,
                                            const std::string& owner_number,
                                            const std::string& schedule_role,
                                            const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Removing instrument schedule. "
                               << "instrument_id: " << instrument_id
                               << " owner_role: " << owner_role << " owner_number: " << owner_number
                               << " schedule_role: " << schedule_role
                               << " sequence_number: " << sequence_number;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<instrument_schedule_entity> |
                       where("tenant_id"_c == tid && "instrument_id"_c == instrument_id &&
                             "owner_role"_c == owner_role && "owner_number"_c == owner_number &&
                             "schedule_role"_c == schedule_role &&
                             "sequence_number"_c == sequence_number && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing instrument schedule from database.");
}

std::vector<domain::instrument_schedule> instrument_schedule_repository::read_latest(
    context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest instrument schedules with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<instrument_schedule_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("instrument_id"_c,
                                "owner_role"_c,
                                "owner_number"_c,
                                "schedule_role"_c,
                                "sequence_number"_c) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<instrument_schedule_entity, domain::instrument_schedule>(
        ctx,
        query,
        [](const auto& entities) { return instrument_schedule_mapper::map(entities); },
        lg(),
        "Reading latest instrument schedules with pagination.");
}

std::uint32_t instrument_schedule_repository::get_total_instrument_schedule_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active instrument schedule count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<instrument_schedule_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active instrument schedule count: " << count;
    return count;
}

void instrument_schedule_repository::remove(context ctx,
                                            const std::vector<std::string>& instrument_ids,
                                            const std::vector<std::string>& owner_roles,
                                            const std::vector<std::string>& owner_numbers,
                                            const std::vector<std::string>& schedule_roles,
                                            const std::vector<std::string>& sequence_numbers) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (owner_roles.size() != instrument_ids.size() ||
        owner_numbers.size() != instrument_ids.size() ||
        schedule_roles.size() != instrument_ids.size() ||
        sequence_numbers.size() != instrument_ids.size())
        throw std::invalid_argument(
            "instrument_schedule_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < instrument_ids.size(); ++i)
        remove(ctx,
               instrument_ids[i],
               owner_roles[i],
               owner_numbers[i],
               schedule_roles[i],
               sequence_numbers[i]);
}


}
