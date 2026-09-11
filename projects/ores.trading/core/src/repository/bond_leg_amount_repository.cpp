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
#include "ores.trading.core/repository/bond_leg_amount_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.trading.api/domain/bond_leg_amount_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/bond_leg_amount_entity.hpp"
#include "ores.trading.core/repository/bond_leg_amount_mapper.hpp"
#include <sqlgen/postgres.hpp>
#include <stdexcept>

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string bond_leg_amount_repository::sql() {
    return generate_create_table_sql<bond_leg_amount_entity>(lg());
}

void bond_leg_amount_repository::write(context ctx, const domain::bond_leg_amount& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing bond leg amount. "
                               << "instrument_id: " << v.instrument_id
                               << " leg_role: " << v.leg_role << " leg_number: " << v.leg_number
                               << " amount_role: " << v.amount_role
                               << " sequence_number: " << v.sequence_number;
    execute_write_query(
        ctx, bond_leg_amount_mapper::map(v), lg(), "Writing bond leg amount to database.");
}

void bond_leg_amount_repository::write(context ctx, const std::vector<domain::bond_leg_amount>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing bond leg amounts. Count: " << v.size();
    execute_write_query(
        ctx, bond_leg_amount_mapper::map(v), lg(), "Writing bond leg amounts to database.");
}

std::vector<domain::bond_leg_amount> bond_leg_amount_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<bond_leg_amount_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by(
            "instrument_id"_c, "leg_role"_c, "leg_number"_c, "amount_role"_c, "sequence_number"_c);

    return execute_read_query<bond_leg_amount_entity, domain::bond_leg_amount>(
        ctx,
        query,
        [](const auto& entities) { return bond_leg_amount_mapper::map(entities); },
        lg(),
        "Reading latest bond leg amounts");
}

std::vector<domain::bond_leg_amount>
bond_leg_amount_repository::read_latest(context ctx,
                                        const std::string& instrument_id,
                                        const std::string& leg_role,
                                        const std::string& leg_number,
                                        const std::string& amount_role,
                                        const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest bond leg amount. "
                               << "instrument_id: " << instrument_id << " leg_role: " << leg_role
                               << " leg_number: " << leg_number << " amount_role: " << amount_role
                               << " sequence_number: " << sequence_number;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<bond_leg_amount_entity>> |
                       where("tenant_id"_c == tid && "instrument_id"_c == instrument_id &&
                             "leg_role"_c == leg_role && "leg_number"_c == leg_number &&
                             "amount_role"_c == amount_role &&
                             "sequence_number"_c == sequence_number && "valid_to"_c == max.value());

    return execute_read_query<bond_leg_amount_entity, domain::bond_leg_amount>(
        ctx,
        query,
        [](const auto& entities) { return bond_leg_amount_mapper::map(entities); },
        lg(),
        "Reading latest bond leg amount by instrument_id.");
}


std::vector<domain::bond_leg_amount>
bond_leg_amount_repository::read_all(context ctx,
                                     const std::string& instrument_id,
                                     const std::string& leg_role,
                                     const std::string& leg_number,
                                     const std::string& amount_role,
                                     const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all bond leg amount versions. "
                               << "instrument_id: " << instrument_id << " leg_role: " << leg_role
                               << " leg_number: " << leg_number << " amount_role: " << amount_role
                               << " sequence_number: " << sequence_number;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<bond_leg_amount_entity>> |
        where("tenant_id"_c == tid && "instrument_id"_c == instrument_id &&
              "leg_role"_c == leg_role && "leg_number"_c == leg_number &&
              "amount_role"_c == amount_role && "sequence_number"_c == sequence_number) |
        order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<bond_leg_amount_entity, domain::bond_leg_amount>(
        ctx,
        query,
        [](const auto& entities) { return bond_leg_amount_mapper::map(entities); },
        lg(),
        "Reading all bond leg amount versions by instrument_id.");
}

std::optional<domain::bond_leg_amount>
bond_leg_amount_repository::read_at_version(context ctx,
                                            const std::string& instrument_id,
                                            const std::string& leg_role,
                                            const std::string& leg_number,
                                            const std::string& amount_role,
                                            const std::string& sequence_number,
                                            std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading bond leg amount at version. "
                               << "instrument_id: " << instrument_id << " leg_role: " << leg_role
                               << " leg_number: " << leg_number << " amount_role: " << amount_role
                               << " sequence_number: " << sequence_number
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<bond_leg_amount_entity>> |
                       where("tenant_id"_c == tid && "instrument_id"_c == instrument_id &&
                             "leg_role"_c == leg_role && "leg_number"_c == leg_number &&
                             "amount_role"_c == amount_role &&
                             "sequence_number"_c == sequence_number && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<bond_leg_amount_entity, domain::bond_leg_amount>(
        ctx,
        query,
        [](const auto& entities) { return bond_leg_amount_mapper::map(entities); },
        lg(),
        "Reading bond leg amount at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void bond_leg_amount_repository::remove(context ctx,
                                        const std::string& instrument_id,
                                        const std::string& leg_role,
                                        const std::string& leg_number,
                                        const std::string& amount_role,
                                        const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Removing bond leg amount. " << "instrument_id: " << instrument_id
                               << " leg_role: " << leg_role << " leg_number: " << leg_number
                               << " amount_role: " << amount_role
                               << " sequence_number: " << sequence_number;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<bond_leg_amount_entity> |
                       where("tenant_id"_c == tid && "instrument_id"_c == instrument_id &&
                             "leg_role"_c == leg_role && "leg_number"_c == leg_number &&
                             "amount_role"_c == amount_role &&
                             "sequence_number"_c == sequence_number && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing bond leg amount from database.");
}

std::vector<domain::bond_leg_amount>
bond_leg_amount_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest bond leg amounts with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<bond_leg_amount_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by(
            "instrument_id"_c, "leg_role"_c, "leg_number"_c, "amount_role"_c, "sequence_number"_c) |
        sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<bond_leg_amount_entity, domain::bond_leg_amount>(
        ctx,
        query,
        [](const auto& entities) { return bond_leg_amount_mapper::map(entities); },
        lg(),
        "Reading latest bond leg amounts with pagination.");
}

std::uint32_t bond_leg_amount_repository::get_total_bond_leg_amount_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active bond leg amount count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<bond_leg_amount_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active bond leg amount count: " << count;
    return count;
}

void bond_leg_amount_repository::remove(context ctx,
                                        const std::vector<std::string>& instrument_ids,
                                        const std::vector<std::string>& leg_roles,
                                        const std::vector<std::string>& leg_numbers,
                                        const std::vector<std::string>& amount_roles,
                                        const std::vector<std::string>& sequence_numbers) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (leg_roles.size() != instrument_ids.size() || leg_numbers.size() != instrument_ids.size() ||
        amount_roles.size() != instrument_ids.size() ||
        sequence_numbers.size() != instrument_ids.size())
        throw std::invalid_argument(
            "bond_leg_amount_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < instrument_ids.size(); ++i)
        remove(ctx,
               instrument_ids[i],
               leg_roles[i],
               leg_numbers[i],
               amount_roles[i],
               sequence_numbers[i]);
}


}
