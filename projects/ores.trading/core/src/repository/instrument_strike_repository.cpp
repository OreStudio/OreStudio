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
#include "ores.trading.core/repository/instrument_strike_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/instrument_strike.hpp"
#include "ores.trading.api/domain/instrument_strike_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/instrument_strike_entity.hpp"
#include "ores.trading.core/repository/instrument_strike_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

namespace ores::trading::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string instrument_strike_repository::sql() {
    return query_builder::insert_statement_sql<instrument_strike_entity>(lg());
}

bool instrument_strike_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order
instrument_strike_list_order(const ores::utility::domain::order& order,
                             std::initializer_list<std::string> default_columns,
                             bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(default_columns,
                                         default_descending != order.descending,
                                         {"trade_id", "strike_role", "sequence_number"});
    if (!instrument_strike_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of instrument strikes cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order(
        {order.field}, order.descending, {"trade_id", "strike_role", "sequence_number"});
}

}

ores::utility::domain::precondition
instrument_strike_repository::replace_claim(context ctx, const domain::instrument_strike& v) {
    const auto current = read_latest(
        ctx, boost::uuids::to_string(v.trade_id), v.strike_role, std::to_string(v.sequence_number));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::instrument_strike
instrument_strike_repository::apply_claim(context ctx,
                                          const domain::instrument_strike& v,
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
                                             v.strike_role,
                                             std::to_string(v.sequence_number));
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void instrument_strike_repository::write(context ctx, const domain::instrument_strike& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void instrument_strike_repository::write(context ctx,
                                         const std::vector<domain::instrument_strike>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void instrument_strike_repository::write(context ctx,
                                         const domain::instrument_strike& v,
                                         const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing instrument strike. " << "trade_id: " << v.trade_id
                               << " strike_role: " << v.strike_role
                               << " sequence_number: " << v.sequence_number;
    const auto t = apply_claim(ctx, v, claim);
    query_builder::execute_write_query(
        ctx, instrument_strike_mapper::map(t), lg(), "Writing instrument strike to database.");
}

void instrument_strike_repository::write(
    context ctx,
    const std::vector<domain::instrument_strike>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing instrument strikes. Count: " << v.size();
    std::vector<domain::instrument_strike> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    query_builder::execute_write_query(
        ctx, instrument_strike_mapper::map(batch), lg(), "Writing instrument strikes to database.");
}

std::vector<domain::instrument_strike> instrument_strike_repository::read_latest(context ctx) {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<instrument_strike_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("trade_id"_c, "strike_role"_c, "sequence_number"_c);

    return query_builder::execute_read_query<instrument_strike_entity, domain::instrument_strike>(
        ctx,
        query,
        [](const auto& entities) { return instrument_strike_mapper::map(entities); },
        lg(),
        "Reading latest instrument strikes");
}

std::vector<domain::instrument_strike>
instrument_strike_repository::read_latest(context ctx,
                                          const std::string& trade_id,
                                          const std::string& strike_role,
                                          const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest instrument strike. " << "trade_id: " << trade_id
                               << " strike_role: " << strike_role
                               << " sequence_number: " << sequence_number;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<instrument_strike_entity>> |
        query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                             "strike_role"_c == strike_role &&
                             "sequence_number"_c == sequence_number && "valid_to"_c == max);

    return query_builder::execute_read_query<instrument_strike_entity, domain::instrument_strike>(
        ctx,
        query,
        [](const auto& entities) { return instrument_strike_mapper::map(entities); },
        lg(),
        "Reading latest instrument strike by trade_id.");
}


std::vector<domain::instrument_strike>
instrument_strike_repository::read_all(context ctx,
                                       const std::string& trade_id,
                                       const std::string& strike_role,
                                       const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all instrument strike versions. "
                               << "trade_id: " << trade_id << " strike_role: " << strike_role
                               << " sequence_number: " << sequence_number;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<instrument_strike_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "strike_role"_c == strike_role &&
                                            "sequence_number"_c == sequence_number) |
                       query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<instrument_strike_entity, domain::instrument_strike>(
        ctx,
        query,
        [](const auto& entities) { return instrument_strike_mapper::map(entities); },
        lg(),
        "Reading all instrument strike versions by trade_id.");
}

std::optional<domain::instrument_strike>
instrument_strike_repository::read_at_version(context ctx,
                                              const std::string& trade_id,
                                              const std::string& strike_role,
                                              const std::string& sequence_number,
                                              std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading instrument strike at version. "
                               << "trade_id: " << trade_id << " strike_role: " << strike_role
                               << " sequence_number: " << sequence_number
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<instrument_strike_entity>> |
        query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                             "strike_role"_c == strike_role &&
                             "sequence_number"_c == sequence_number && "version"_c == version) |
        query_builder::limit(1);

    const auto entities =
        query_builder::execute_read_query<instrument_strike_entity, domain::instrument_strike>(
            ctx,
            query,
            [](const auto& entities) { return instrument_strike_mapper::map(entities); },
            lg(),
            "Reading instrument strike at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}


instrument_strike_repository::remove_status
instrument_strike_repository::remove(context ctx,
                                     const std::string& trade_id,
                                     const std::string& strike_role,
                                     const std::string& sequence_number,
                                     std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing instrument strike. " << "trade_id: " << trade_id
                               << " strike_role: " << strike_role
                               << " sequence_number: " << sequence_number;
    const auto current = read_latest(ctx, trade_id, strike_role, sequence_number);
    if (current.empty())
        return remove_status::missing;
    // The protocol states the version as a uint32 and the row carries it as an
    // int, so the comparison states the conversion rather than relying on one.
    if (version && static_cast<std::uint32_t>(current.front().version) != *version)
        return remove_status::conflicting;
    static constexpr auto max = db_timestamp::max();
    // The row is named by its version as well as by its key, so the removal
    // cannot close a row that replaced the one the caller read between the
    // read above and this statement.
    const auto expected = version ? static_cast<int>(*version) : current.front().version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::delete_from<instrument_strike_entity> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "strike_role"_c == strike_role &&
                                            "sequence_number"_c == sequence_number &&
                                            "valid_to"_c == max && "version"_c == expected);

    query_builder::execute_delete_query(
        ctx, query, lg(), "Removing instrument strike from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, trade_id, strike_role, sequence_number).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void instrument_strike_repository::remove(context ctx,
                                          const std::string& trade_id,
                                          const std::string& strike_role,
                                          const std::string& sequence_number) {
    static_cast<void>(remove(ctx, trade_id, strike_role, sequence_number, std::nullopt));
}

std::vector<domain::instrument_strike>
instrument_strike_repository::read_latest(context ctx,
                                          std::uint32_t offset,
                                          std::uint32_t limit,
                                          const ores::utility::domain::order& order,
                                          const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest instrument strikes with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<instrument_strike_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<instrument_strike_entity,
                                                     domain::instrument_strike>(
        ctx,
        query,
        instrument_strike_list_order(order, {"trade_id", "strike_role", "sequence_number"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of), std::nullopt),
        [](const auto& entities) { return instrument_strike_mapper::map(entities); },
        lg(),
        "Reading latest instrument strikes with pagination.");
}

std::uint32_t instrument_strike_repository::get_total_instrument_strike_count(
    context ctx, const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active instrument strike count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<instrument_strike_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<instrument_strike_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of), std::nullopt),
        lg(),
        "Counting instrument strikes");
}

std::vector<domain::instrument_strike>
instrument_strike_repository::read_latest(context ctx,
                                          const std::vector<std::string>& trade_ids,
                                          const std::vector<std::string>& strike_roles,
                                          const std::vector<std::string>& sequence_numbers) {
    if (trade_ids.empty() || strike_roles.empty() || sequence_numbers.empty())
        return {};
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<instrument_strike_entity>> |
        query_builder::where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) &&
                             "strike_role"_c.in(strike_roles) &&
                             "sequence_number"_c.in(sequence_numbers) && "valid_to"_c == max);
    auto result =
        query_builder::execute_read_query<instrument_strike_entity, domain::instrument_strike>(
            ctx,
            query,
            [](const auto& entities) { return instrument_strike_mapper::map(entities); },
            lg(),
            "Reading latest instrument strikes by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (the query builder has no tuple IN), so filter down to the
    // exact requested key-tuples here.
    if (strike_roles.size() != trade_ids.size() || sequence_numbers.size() != trade_ids.size())
        throw std::invalid_argument("instrument_strike_repository::read_latest: key column vectors "
                                    "must be the same length");
    std::set<std::tuple<std::string, std::string, std::string>> requested;
    for (std::size_t i = 0; i < trade_ids.size(); ++i)
        requested.emplace(trade_ids[i], strike_roles[i], sequence_numbers[i]);
    std::vector<domain::instrument_strike> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(std::make_tuple(boost::uuids::to_string(item.trade_id),
                                               item.strike_role,
                                               std::to_string(item.sequence_number))))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void instrument_strike_repository::remove(context ctx,
                                          const std::vector<std::string>& trade_ids,
                                          const std::vector<std::string>& strike_roles,
                                          const std::vector<std::string>& sequence_numbers) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (strike_roles.size() != trade_ids.size() || sequence_numbers.size() != trade_ids.size())
        throw std::invalid_argument(
            "instrument_strike_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < trade_ids.size(); ++i)
        remove(ctx, trade_ids[i], strike_roles[i], sequence_numbers[i]);
}


}
