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
#include "ores.trading.core/repository/commodity_basket_constituent_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.trading.api/domain/commodity_basket_constituent_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/commodity_basket_constituent_entity.hpp"
#include "ores.trading.core/repository/commodity_basket_constituent_mapper.hpp"
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

std::string commodity_basket_constituent_repository::sql() {
    return generate_create_table_sql<commodity_basket_constituent_entity>(lg());
}

ores::utility::domain::precondition commodity_basket_constituent_repository::replace_claim(
    context ctx, const domain::commodity_basket_constituent& v) {
    const auto current =
        read_latest(ctx, boost::uuids::to_string(v.trade_id), std::to_string(v.sequence_number));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::commodity_basket_constituent commodity_basket_constituent_repository::apply_claim(
    context ctx,
    const domain::commodity_basket_constituent& v,
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
            const auto current = read_latest(
                ctx, boost::uuids::to_string(v.trade_id), std::to_string(v.sequence_number));
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void commodity_basket_constituent_repository::write(context ctx,
                                                    const domain::commodity_basket_constituent& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void commodity_basket_constituent_repository::write(
    context ctx, const std::vector<domain::commodity_basket_constituent>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void commodity_basket_constituent_repository::write(
    context ctx,
    const domain::commodity_basket_constituent& v,
    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing commodity basket constituent. "
                               << "trade_id: " << v.trade_id
                               << " sequence_number: " << v.sequence_number;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(ctx,
                        commodity_basket_constituent_mapper::map(t),
                        lg(),
                        "Writing commodity basket constituent to database.");
}

void commodity_basket_constituent_repository::write(
    context ctx,
    const std::vector<domain::commodity_basket_constituent>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing commodity basket constituents. Count: " << v.size();
    std::vector<domain::commodity_basket_constituent> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(ctx,
                        commodity_basket_constituent_mapper::map(batch),
                        lg(),
                        "Writing commodity basket constituents to database.");
}

std::vector<domain::commodity_basket_constituent>
commodity_basket_constituent_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<commodity_basket_constituent_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("trade_id"_c, "sequence_number"_c);

    return execute_read_query<commodity_basket_constituent_entity,
                              domain::commodity_basket_constituent>(
        ctx,
        query,
        [](const auto& entities) { return commodity_basket_constituent_mapper::map(entities); },
        lg(),
        "Reading latest commodity basket constituents");
}

std::vector<domain::commodity_basket_constituent>
commodity_basket_constituent_repository::read_latest(context ctx,
                                                     const std::string& trade_id,
                                                     const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest commodity basket constituent. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<commodity_basket_constituent_entity>> |
                       where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                             "sequence_number"_c == sequence_number && "valid_to"_c == max.value());

    return execute_read_query<commodity_basket_constituent_entity,
                              domain::commodity_basket_constituent>(
        ctx,
        query,
        [](const auto& entities) { return commodity_basket_constituent_mapper::map(entities); },
        lg(),
        "Reading latest commodity basket constituent by trade_id.");
}


std::vector<domain::commodity_basket_constituent> commodity_basket_constituent_repository::read_all(
    context ctx, const std::string& trade_id, const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all commodity basket constituent versions. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<commodity_basket_constituent_entity>> |
                       where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                             "sequence_number"_c == sequence_number) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<commodity_basket_constituent_entity,
                              domain::commodity_basket_constituent>(
        ctx,
        query,
        [](const auto& entities) { return commodity_basket_constituent_mapper::map(entities); },
        lg(),
        "Reading all commodity basket constituent versions by trade_id.");
}

std::optional<domain::commodity_basket_constituent>
commodity_basket_constituent_repository::read_at_version(context ctx,
                                                         const std::string& trade_id,
                                                         const std::string& sequence_number,
                                                         std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading commodity basket constituent at version. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<commodity_basket_constituent_entity>> |
                       where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                             "sequence_number"_c == sequence_number && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<commodity_basket_constituent_entity,
                                             domain::commodity_basket_constituent>(
        ctx,
        query,
        [](const auto& entities) { return commodity_basket_constituent_mapper::map(entities); },
        lg(),
        "Reading commodity basket constituent at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}


commodity_basket_constituent_repository::remove_status
commodity_basket_constituent_repository::remove(context ctx,
                                                const std::string& trade_id,
                                                const std::string& sequence_number,
                                                std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing commodity basket constituent. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number;
    const auto current = read_latest(ctx, trade_id, sequence_number);
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
    const auto query = sqlgen::delete_from<commodity_basket_constituent_entity> |
                       where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                             "sequence_number"_c == sequence_number &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing commodity basket constituent from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, trade_id, sequence_number).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void commodity_basket_constituent_repository::remove(context ctx,
                                                     const std::string& trade_id,
                                                     const std::string& sequence_number) {
    static_cast<void>(remove(ctx, trade_id, sequence_number, std::nullopt));
}

std::vector<domain::commodity_basket_constituent>
commodity_basket_constituent_repository::read_latest(context ctx,
                                                     std::uint32_t offset,
                                                     std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest commodity basket constituents with offset: "
                               << offset << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<commodity_basket_constituent_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("trade_id"_c, "sequence_number"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<commodity_basket_constituent_entity,
                              domain::commodity_basket_constituent>(
        ctx,
        query,
        [](const auto& entities) { return commodity_basket_constituent_mapper::map(entities); },
        lg(),
        "Reading latest commodity basket constituents with pagination.");
}

std::uint32_t
commodity_basket_constituent_repository::get_total_commodity_basket_constituent_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active commodity basket constituent count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<commodity_basket_constituent_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active commodity basket constituent count: " << count;
    return count;
}

std::vector<domain::commodity_basket_constituent>
commodity_basket_constituent_repository::read_latest(
    context ctx,
    const std::vector<std::string>& trade_ids,
    const std::vector<std::string>& sequence_numbers) {
    if (trade_ids.empty() || sequence_numbers.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<commodity_basket_constituent_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) &&
              "sequence_number"_c.in(sequence_numbers) && "valid_to"_c == max.value());
    auto result = execute_read_query<commodity_basket_constituent_entity,
                                     domain::commodity_basket_constituent>(
        ctx,
        query,
        [](const auto& entities) { return commodity_basket_constituent_mapper::map(entities); },
        lg(),
        "Reading latest commodity basket constituents by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (sqlgen has no tuple/composite IN), so filter down to the
    // exact requested key-tuples here.
    if (sequence_numbers.size() != trade_ids.size())
        throw std::invalid_argument("commodity_basket_constituent_repository::read_latest: key "
                                    "column vectors must be the same length");
    std::set<std::tuple<std::string, std::string>> requested;
    for (std::size_t i = 0; i < trade_ids.size(); ++i)
        requested.emplace(trade_ids[i], sequence_numbers[i]);
    std::vector<domain::commodity_basket_constituent> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(std::make_tuple(boost::uuids::to_string(item.trade_id),
                                               std::to_string(item.sequence_number))))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void commodity_basket_constituent_repository::remove(
    context ctx,
    const std::vector<std::string>& trade_ids,
    const std::vector<std::string>& sequence_numbers) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (sequence_numbers.size() != trade_ids.size())
        throw std::invalid_argument("commodity_basket_constituent_repository::remove: key column "
                                    "vectors must be the same length");
    for (std::size_t i = 0; i < trade_ids.size(); ++i)
        remove(ctx, trade_ids[i], sequence_numbers[i]);
}


std::vector<domain::commodity_basket_constituent>
commodity_basket_constituent_repository::read_by_instruments_batch(
    context ctx, const std::vector<std::string>& trade_ids) {
    if (trade_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<commodity_basket_constituent_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) && "valid_to"_c == max.value()) |
        order_by("trade_id"_c, "sequence_number"_c);
    return execute_read_query<commodity_basket_constituent_entity,
                              domain::commodity_basket_constituent>(
        ctx,
        query,
        [](const auto& entities) { return commodity_basket_constituent_mapper::map(entities); },
        lg(),
        "Reading commodity basket constituents for multiple instruments.");
}

}
