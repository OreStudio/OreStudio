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
#include "ores.trading.core/repository/bond_issue_conversion_target_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.trading.api/domain/bond_issue_conversion_target_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/bond_issue_conversion_target_entity.hpp"
#include "ores.trading.core/repository/bond_issue_conversion_target_mapper.hpp"
#include <sqlgen/postgres.hpp>
#include <stdexcept>

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string bond_issue_conversion_target_repository::sql() {
    return generate_create_table_sql<bond_issue_conversion_target_entity>(lg());
}

void bond_issue_conversion_target_repository::write(context ctx,
                                                    const domain::bond_issue_conversion_target& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing bond issue conversion target. "
                               << "issue_id: " << v.issue_id
                               << " sequence_number: " << v.sequence_number;
    execute_write_query(ctx,
                        bond_issue_conversion_target_mapper::map(v),
                        lg(),
                        "Writing bond issue conversion target to database.");
}

void bond_issue_conversion_target_repository::write(
    context ctx, const std::vector<domain::bond_issue_conversion_target>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing bond issue conversion targets. Count: " << v.size();
    execute_write_query(ctx,
                        bond_issue_conversion_target_mapper::map(v),
                        lg(),
                        "Writing bond issue conversion targets to database.");
}

std::vector<domain::bond_issue_conversion_target>
bond_issue_conversion_target_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<bond_issue_conversion_target_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("issue_id"_c, "sequence_number"_c);

    return execute_read_query<bond_issue_conversion_target_entity,
                              domain::bond_issue_conversion_target>(
        ctx,
        query,
        [](const auto& entities) { return bond_issue_conversion_target_mapper::map(entities); },
        lg(),
        "Reading latest bond issue conversion targets");
}

std::vector<domain::bond_issue_conversion_target>
bond_issue_conversion_target_repository::read_latest(context ctx,
                                                     const std::string& issue_id,
                                                     const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest bond issue conversion target. "
                               << "issue_id: " << issue_id
                               << " sequence_number: " << sequence_number;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<bond_issue_conversion_target_entity>> |
                       where("tenant_id"_c == tid && "issue_id"_c == issue_id &&
                             "sequence_number"_c == sequence_number && "valid_to"_c == max.value());

    return execute_read_query<bond_issue_conversion_target_entity,
                              domain::bond_issue_conversion_target>(
        ctx,
        query,
        [](const auto& entities) { return bond_issue_conversion_target_mapper::map(entities); },
        lg(),
        "Reading latest bond issue conversion target by issue_id.");
}


std::vector<domain::bond_issue_conversion_target> bond_issue_conversion_target_repository::read_all(
    context ctx, const std::string& issue_id, const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all bond issue conversion target versions. "
                               << "issue_id: " << issue_id
                               << " sequence_number: " << sequence_number;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<bond_issue_conversion_target_entity>> |
                       where("tenant_id"_c == tid && "issue_id"_c == issue_id &&
                             "sequence_number"_c == sequence_number) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<bond_issue_conversion_target_entity,
                              domain::bond_issue_conversion_target>(
        ctx,
        query,
        [](const auto& entities) { return bond_issue_conversion_target_mapper::map(entities); },
        lg(),
        "Reading all bond issue conversion target versions by issue_id.");
}

std::optional<domain::bond_issue_conversion_target>
bond_issue_conversion_target_repository::read_at_version(context ctx,
                                                         const std::string& issue_id,
                                                         const std::string& sequence_number,
                                                         std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading bond issue conversion target at version. "
                               << "issue_id: " << issue_id
                               << " sequence_number: " << sequence_number
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<bond_issue_conversion_target_entity>> |
                       where("tenant_id"_c == tid && "issue_id"_c == issue_id &&
                             "sequence_number"_c == sequence_number && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<bond_issue_conversion_target_entity,
                                             domain::bond_issue_conversion_target>(
        ctx,
        query,
        [](const auto& entities) { return bond_issue_conversion_target_mapper::map(entities); },
        lg(),
        "Reading bond issue conversion target at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void bond_issue_conversion_target_repository::remove(context ctx,
                                                     const std::string& issue_id,
                                                     const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Removing bond issue conversion target. "
                               << "issue_id: " << issue_id
                               << " sequence_number: " << sequence_number;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<bond_issue_conversion_target_entity> |
                       where("tenant_id"_c == tid && "issue_id"_c == issue_id &&
                             "sequence_number"_c == sequence_number && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing bond issue conversion target from database.");
}

std::vector<domain::bond_issue_conversion_target>
bond_issue_conversion_target_repository::read_latest(context ctx,
                                                     std::uint32_t offset,
                                                     std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest bond issue conversion targets with offset: "
                               << offset << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<bond_issue_conversion_target_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("issue_id"_c, "sequence_number"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<bond_issue_conversion_target_entity,
                              domain::bond_issue_conversion_target>(
        ctx,
        query,
        [](const auto& entities) { return bond_issue_conversion_target_mapper::map(entities); },
        lg(),
        "Reading latest bond issue conversion targets with pagination.");
}

std::uint32_t
bond_issue_conversion_target_repository::get_total_conversion_target_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active bond issue conversion target count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<bond_issue_conversion_target_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active bond issue conversion target count: " << count;
    return count;
}

void bond_issue_conversion_target_repository::remove(
    context ctx,
    const std::vector<std::string>& issue_ids,
    const std::vector<std::string>& sequence_numbers) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (sequence_numbers.size() != issue_ids.size())
        throw std::invalid_argument("bond_issue_conversion_target_repository::remove: key column "
                                    "vectors must be the same length");
    for (std::size_t i = 0; i < issue_ids.size(); ++i)
        remove(ctx, issue_ids[i], sequence_numbers[i]);
}


}
