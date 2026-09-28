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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_repository.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.trading.core/repository/instrument_schedule_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.trading.api/domain/instrument_schedule_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/instrument_schedule_entity.hpp"
#include "ores.trading.core/repository/instrument_schedule_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <set>
#include <sqlgen/postgres.hpp>
#include <stdexcept>
#include <tuple>

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string instrument_schedule_repository::sql() {
    return generate_create_table_sql<instrument_schedule_entity>(lg());
}

ores::utility::domain::precondition
instrument_schedule_repository::replace_claim(context ctx, const domain::instrument_schedule& v) {
    const auto current = read_latest(ctx,
                                     boost::uuids::to_string(v.trade_id),
                                     v.owner_role,
                                     std::to_string(v.owner_number),
                                     v.schedule_role,
                                     std::to_string(v.sequence_number));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::instrument_schedule
instrument_schedule_repository::apply_claim(context ctx,
                                            const domain::instrument_schedule& v,
                                            const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    switch (claim.kind) {
        case precondition_kind::must_not_exist:
            // Zero states that no current row exists, which is the one meaning the
            // store gives a zero version.
            t.version = 0;
            break;
        case precondition_kind::must_match_version:
            t.version = claim.version ? static_cast<int>(*claim.version) : 0;
            break;
        case precondition_kind::any: {
            // A caller that claims nothing still has to say what it replaces, so
            // the row is read and its version stated. A row that moved on between
            // this read and the write is a conflict the trigger raises, never a
            // silent overwrite.
            const auto current = read_latest(ctx,
                                             boost::uuids::to_string(v.trade_id),
                                             v.owner_role,
                                             std::to_string(v.owner_number),
                                             v.schedule_role,
                                             std::to_string(v.sequence_number));
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void instrument_schedule_repository::write(context ctx, const domain::instrument_schedule& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void instrument_schedule_repository::write(context ctx,
                                           const std::vector<domain::instrument_schedule>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void instrument_schedule_repository::write(context ctx,
                                           const domain::instrument_schedule& v,
                                           const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing instrument schedule. " << "trade_id: " << v.trade_id
                               << " owner_role: " << v.owner_role
                               << " owner_number: " << v.owner_number
                               << " schedule_role: " << v.schedule_role
                               << " sequence_number: " << v.sequence_number;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(
        ctx, instrument_schedule_mapper::map(t), lg(), "Writing instrument schedule to database.");
}

void instrument_schedule_repository::write(
    context ctx,
    const std::vector<domain::instrument_schedule>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing instrument schedules. Count: " << v.size();
    std::vector<domain::instrument_schedule> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(ctx,
                        instrument_schedule_mapper::map(batch),
                        lg(),
                        "Writing instrument schedules to database.");
}

std::vector<domain::instrument_schedule> instrument_schedule_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<instrument_schedule_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by(
            "trade_id"_c, "owner_role"_c, "owner_number"_c, "schedule_role"_c, "sequence_number"_c);

    return execute_read_query<instrument_schedule_entity, domain::instrument_schedule>(
        ctx,
        query,
        [](const auto& entities) { return instrument_schedule_mapper::map(entities); },
        lg(),
        "Reading latest instrument schedules");
}

std::vector<domain::instrument_schedule>
instrument_schedule_repository::read_latest(context ctx,
                                            const std::string& trade_id,
                                            const std::string& owner_role,
                                            const std::string& owner_number,
                                            const std::string& schedule_role,
                                            const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest instrument schedule. " << "trade_id: " << trade_id
                               << " owner_role: " << owner_role << " owner_number: " << owner_number
                               << " schedule_role: " << schedule_role
                               << " sequence_number: " << sequence_number;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<instrument_schedule_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c == trade_id && "owner_role"_c == owner_role &&
              "owner_number"_c == owner_number && "schedule_role"_c == schedule_role &&
              "sequence_number"_c == sequence_number && "valid_to"_c == max.value());

    return execute_read_query<instrument_schedule_entity, domain::instrument_schedule>(
        ctx,
        query,
        [](const auto& entities) { return instrument_schedule_mapper::map(entities); },
        lg(),
        "Reading latest instrument schedule by trade_id.");
}


std::vector<domain::instrument_schedule>
instrument_schedule_repository::read_all(context ctx,
                                         const std::string& trade_id,
                                         const std::string& owner_role,
                                         const std::string& owner_number,
                                         const std::string& schedule_role,
                                         const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all instrument schedule versions. "
                               << "trade_id: " << trade_id << " owner_role: " << owner_role
                               << " owner_number: " << owner_number
                               << " schedule_role: " << schedule_role
                               << " sequence_number: " << sequence_number;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<instrument_schedule_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c == trade_id && "owner_role"_c == owner_role &&
              "owner_number"_c == owner_number && "schedule_role"_c == schedule_role &&
              "sequence_number"_c == sequence_number) |
        order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<instrument_schedule_entity, domain::instrument_schedule>(
        ctx,
        query,
        [](const auto& entities) { return instrument_schedule_mapper::map(entities); },
        lg(),
        "Reading all instrument schedule versions by trade_id.");
}

std::optional<domain::instrument_schedule>
instrument_schedule_repository::read_at_version(context ctx,
                                                const std::string& trade_id,
                                                const std::string& owner_role,
                                                const std::string& owner_number,
                                                const std::string& schedule_role,
                                                const std::string& sequence_number,
                                                std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading instrument schedule at version. "
                               << "trade_id: " << trade_id << " owner_role: " << owner_role
                               << " owner_number: " << owner_number
                               << " schedule_role: " << schedule_role
                               << " sequence_number: " << sequence_number
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<instrument_schedule_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c == trade_id && "owner_role"_c == owner_role &&
              "owner_number"_c == owner_number && "schedule_role"_c == schedule_role &&
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

instrument_schedule_repository::remove_status
instrument_schedule_repository::remove(context ctx,
                                       const std::string& trade_id,
                                       const std::string& owner_role,
                                       const std::string& owner_number,
                                       const std::string& schedule_role,
                                       const std::string& sequence_number,
                                       std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing instrument schedule. " << "trade_id: " << trade_id
                               << " owner_role: " << owner_role << " owner_number: " << owner_number
                               << " schedule_role: " << schedule_role
                               << " sequence_number: " << sequence_number;
    const auto current =
        read_latest(ctx, trade_id, owner_role, owner_number, schedule_role, sequence_number);
    if (current.empty())
        return remove_status::missing;
    // The protocol states the version as a uint32 and the row carries it as an
    // int, so the comparison states the conversion rather than relying on one.
    if (version && static_cast<std::uint32_t>(current.front().version) != *version)
        return remove_status::conflicting;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    // The row is named by its version as well as by its key, so the removal
    // cannot close a row that replaced the one the caller read between the
    // read above and this statement.
    const auto expected = version ? static_cast<int>(*version) : current.front().version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<instrument_schedule_entity> |
        where("tenant_id"_c == tid && "trade_id"_c == trade_id && "owner_role"_c == owner_role &&
              "owner_number"_c == owner_number && "schedule_role"_c == schedule_role &&
              "sequence_number"_c == sequence_number && "valid_to"_c == max.value() &&
              "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing instrument schedule from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, trade_id, owner_role, owner_number, schedule_role, sequence_number)
             .empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void instrument_schedule_repository::remove(context ctx,
                                            const std::string& trade_id,
                                            const std::string& owner_role,
                                            const std::string& owner_number,
                                            const std::string& schedule_role,
                                            const std::string& sequence_number) {
    static_cast<void>(remove(
        ctx, trade_id, owner_role, owner_number, schedule_role, sequence_number, std::nullopt));
}

std::vector<domain::instrument_schedule> instrument_schedule_repository::read_latest(
    context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest instrument schedules with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<instrument_schedule_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("trade_id"_c,
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

std::vector<domain::instrument_schedule>
instrument_schedule_repository::read_latest(context ctx,
                                            const std::vector<std::string>& trade_ids,
                                            const std::vector<std::string>& owner_roles,
                                            const std::vector<std::string>& owner_numbers,
                                            const std::vector<std::string>& schedule_roles,
                                            const std::vector<std::string>& sequence_numbers) {
    if (trade_ids.empty() || owner_roles.empty() || owner_numbers.empty() ||
        schedule_roles.empty() || sequence_numbers.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<instrument_schedule_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) &&
              "owner_role"_c.in(owner_roles) && "owner_number"_c.in(owner_numbers) &&
              "schedule_role"_c.in(schedule_roles) && "sequence_number"_c.in(sequence_numbers) &&
              "valid_to"_c == max.value());
    auto result = execute_read_query<instrument_schedule_entity, domain::instrument_schedule>(
        ctx,
        query,
        [](const auto& entities) { return instrument_schedule_mapper::map(entities); },
        lg(),
        "Reading latest instrument schedules by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (sqlgen has no tuple/composite IN), so filter down to the
    // exact requested key-tuples here.
    if (owner_roles.size() != trade_ids.size() || owner_numbers.size() != trade_ids.size() ||
        schedule_roles.size() != trade_ids.size() || sequence_numbers.size() != trade_ids.size())
        throw std::invalid_argument("instrument_schedule_repository::read_latest: key column "
                                    "vectors must be the same length");
    std::set<std::tuple<std::string, std::string, std::string, std::string, std::string>> requested;
    for (std::size_t i = 0; i < trade_ids.size(); ++i)
        requested.emplace(
            trade_ids[i], owner_roles[i], owner_numbers[i], schedule_roles[i], sequence_numbers[i]);
    std::vector<domain::instrument_schedule> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(std::make_tuple(boost::uuids::to_string(item.trade_id),
                                               item.owner_role,
                                               std::to_string(item.owner_number),
                                               item.schedule_role,
                                               std::to_string(item.sequence_number))))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void instrument_schedule_repository::remove(context ctx,
                                            const std::vector<std::string>& trade_ids,
                                            const std::vector<std::string>& owner_roles,
                                            const std::vector<std::string>& owner_numbers,
                                            const std::vector<std::string>& schedule_roles,
                                            const std::vector<std::string>& sequence_numbers) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (owner_roles.size() != trade_ids.size() || owner_numbers.size() != trade_ids.size() ||
        schedule_roles.size() != trade_ids.size() || sequence_numbers.size() != trade_ids.size())
        throw std::invalid_argument(
            "instrument_schedule_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < trade_ids.size(); ++i)
        remove(ctx,
               trade_ids[i],
               owner_roles[i],
               owner_numbers[i],
               schedule_roles[i],
               sequence_numbers[i]);
}


}
