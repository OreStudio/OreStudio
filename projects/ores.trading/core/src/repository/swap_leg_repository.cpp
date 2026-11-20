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
#include "ores.trading.core/repository/swap_leg_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"
#include "ores.trading.api/domain/swap_leg_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/swap_leg_protocol.hpp"
#include "ores.trading.core/repository/swap_leg_entity.hpp"
#include "ores.trading.core/repository/swap_leg_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <iterator>
#include <map>
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

std::string swap_leg_repository::sql() {
    return query_builder::insert_statement_sql<swap_leg_entity>(lg());
}

bool swap_leg_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order swap_leg_list_order(const ores::utility::domain::order& order,
                                         std::initializer_list<std::string> default_columns,
                                         bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"trade_id", "leg_number"});
    if (!swap_leg_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of swap legs cannot be ordered by " + order.field +
                                    ".");
    return query_builder::make_order({order.field}, order.descending, {"trade_id", "leg_number"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition>
swap_leg_filter_condition(const std::optional<messaging::swap_legs_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->trade_id)
        r.push_back(
            query_builder::equals("trade_id", query_builder::filter_value(*filter->trade_id)));
    if (filter->trade_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->trade_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("trade_id", std::move(values)));
    }
    return query_builder::all_of(std::move(r));
}

}

namespace {

/**
 * @brief Whether a claim is checked against the row its key names now.
 */
bool swap_leg_claim_reads_current(const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    // The store checks a stated version itself. A claim of nothing still has
    // to state the version it replaces, so only that claim needs the row.
    return claim.kind == precondition_kind::any;
}

using swap_leg_rows_by_key =
    std::map<std::tuple<std::string, std::string>, std::vector<domain::swap_leg>>;

/**
 * @brief The current rows the objects name, by the text form of their key.
 *
 * One batch read for the set, so a batch checks its claims against one read
 * rather than one read per row.
 */
swap_leg_rows_by_key swap_leg_current_rows(swap_leg_repository& repo,
                                           const ores::database::context& ctx,
                                           const std::vector<domain::swap_leg>& of) {
    std::vector<std::string> trade_ids;
    trade_ids.reserve(of.size());
    std::vector<std::string> leg_numbers;
    leg_numbers.reserve(of.size());
    for (const auto& row : of) {
        trade_ids.push_back(boost::uuids::to_string(row.identity.trade_id));
        leg_numbers.push_back(std::to_string(row.identity.leg_number));
    }
    swap_leg_rows_by_key current;
    for (auto& item : repo.read_latest(ctx, trade_ids, leg_numbers))
        current[std::make_tuple(boost::uuids::to_string(item.identity.trade_id),
                                std::to_string(item.identity.leg_number))]
            .push_back(std::move(item));
    return current;
}

/**
 * @brief The current rows that the key of an object names, or none.
 */
const std::vector<domain::swap_leg>& swap_leg_rows_of(const swap_leg_rows_by_key& current,
                                                      const domain::swap_leg& item) {
    static const std::vector<domain::swap_leg> none;
    const auto found = current.find(std::make_tuple(boost::uuids::to_string(item.identity.trade_id),
                                                    std::to_string(item.identity.leg_number)));
    return found == current.end() ? none : found->second;
}

/**
 * @brief The claim a replace makes: the version the row carries now, or
 * that no row exists yet.
 */
ores::utility::domain::precondition
swap_leg_replace_claim(const std::vector<domain::swap_leg>& current) {
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().identity.version)};
}

/**
 * @brief The object with the claim's version stamped onto it.
 *
 * The caller passes the row the key names now whenever the claim reads it. A
 * claim the store cannot check is refused here rather than ignored.
 */
domain::swap_leg swap_leg_apply_claim(const domain::swap_leg& v,
                                      const ores::utility::domain::precondition& claim,
                                      const std::vector<domain::swap_leg>& current) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    switch (claim.kind) {
        case precondition_kind::must_not_exist:
            // Zero states that no current row exists, which is the one meaning the
            // store gives a zero version.
            t.identity.version = 0;
            break;
        case precondition_kind::must_match_version:
            t.identity.version = claim.version ? static_cast<int>(*claim.version) : 0;
            break;
        case precondition_kind::any:
            // A caller that claims nothing still has to say what it replaces, so
            // the row's current version is stated. A row that moved on between
            // the read and the write is a conflict the trigger raises, never a
            // silent overwrite.
            t.identity.version = current.empty() ? 0 : current.front().identity.version;
            break;
    }
    return t;
}

}

void swap_leg_repository::write(const context& ctx, const domain::swap_leg& v) {
    write(ctx,
          v,
          swap_leg_replace_claim(read_latest(ctx,
                                             boost::uuids::to_string(v.identity.trade_id),
                                             std::to_string(v.identity.leg_number))));
}

void swap_leg_repository::write(const context& ctx, const std::vector<domain::swap_leg>& v) {
    const auto current = swap_leg_current_rows(*this, ctx, v);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(swap_leg_replace_claim(swap_leg_rows_of(current, item)));
    write(ctx, v, claims);
}

void swap_leg_repository::write(const context& ctx,
                                const domain::swap_leg& v,
                                const ores::utility::domain::precondition& claim) {
    ORES_LOG_SEV(lg(), debug) << "Writing swap leg. " << "trade_id: " << v.identity.trade_id
                              << " leg_number: " << v.identity.leg_number;
    const auto t =
        swap_leg_apply_claim(v,
                             claim,
                             swap_leg_claim_reads_current(claim) ?
                                 read_latest(ctx,
                                             boost::uuids::to_string(v.identity.trade_id),
                                             std::to_string(v.identity.leg_number)) :
                                 std::vector<domain::swap_leg>{});
    query_builder::execute_write_query(
        ctx, swap_leg_mapper::map(t), lg(), "Writing swap leg to database.");
}

void swap_leg_repository::write(const context& ctx,
                                const std::vector<domain::swap_leg>& v,
                                const std::vector<ores::utility::domain::precondition>& claims) {
    ORES_LOG_SEV(lg(), debug) << "Writing swap legs. Count: " << v.size();
    if (claims.size() != v.size())
        throw std::invalid_argument("swap_leg_repository::write: one claim is needed for each row");
    std::vector<domain::swap_leg> to_read;
    for (std::size_t i = 0; i < v.size(); ++i) {
        if (swap_leg_claim_reads_current(claims[i]))
            to_read.push_back(v[i]);
    }
    const auto current = swap_leg_current_rows(*this, ctx, to_read);
    std::vector<domain::swap_leg> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(swap_leg_apply_claim(v[i], claims[i], swap_leg_rows_of(current, v[i])));
    query_builder::execute_write_query(
        ctx, swap_leg_mapper::map(batch), lg(), "Writing swap legs to database.");
}

std::vector<domain::swap_leg> swap_leg_repository::read_latest(const context& ctx) {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<swap_leg_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("trade_id"_c, "leg_number"_c);

    return query_builder::execute_read_query<swap_leg_entity, domain::swap_leg>(
        ctx,
        query,
        [](const auto& entities) { return swap_leg_mapper::map(entities); },
        lg(),
        "Reading latest swap legs");
}

std::vector<domain::swap_leg> swap_leg_repository::read_latest(const context& ctx,
                                                               const std::string& trade_id,
                                                               const std::string& leg_number) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest swap leg. " << "trade_id: " << trade_id
                              << " leg_number: " << leg_number;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<swap_leg_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "leg_number"_c == leg_number && "valid_to"_c == max);

    return query_builder::execute_read_query<swap_leg_entity, domain::swap_leg>(
        ctx,
        query,
        [](const auto& entities) { return swap_leg_mapper::map(entities); },
        lg(),
        "Reading latest swap leg by trade_id.");
}


std::vector<domain::swap_leg> swap_leg_repository::read_all(const context& ctx,
                                                            const std::string& trade_id,
                                                            const std::string& leg_number) {
    ORES_LOG_SEV(lg(), debug) << "Reading all swap leg versions. " << "trade_id: " << trade_id
                              << " leg_number: " << leg_number;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<swap_leg_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "leg_number"_c == leg_number) |
                       query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<swap_leg_entity, domain::swap_leg>(
        ctx,
        query,
        [](const auto& entities) { return swap_leg_mapper::map(entities); },
        lg(),
        "Reading all swap leg versions by trade_id.");
}

std::optional<domain::swap_leg> swap_leg_repository::read_at_version(const context& ctx,
                                                                     const std::string& trade_id,
                                                                     const std::string& leg_number,
                                                                     std::uint32_t version) {
    ORES_LOG_SEV(lg(), debug) << "Reading swap leg at version. " << "trade_id: " << trade_id
                              << " leg_number: " << leg_number << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<swap_leg_entity>> |
        query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                             "leg_number"_c == leg_number && "version"_c == version) |
        query_builder::limit(1);

    const auto entities = query_builder::execute_read_query<swap_leg_entity, domain::swap_leg>(
        ctx,
        query,
        [](const auto& entities) { return swap_leg_mapper::map(entities); },
        lg(),
        "Reading swap leg at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::swap_leg> swap_leg_repository::read_latest_by_trade_id(
    const context& ctx,
    const std::string& trade_id,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::swap_legs_filter>& filter) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest swap legs. trade_id: " << trade_id
                              << " offset: " << offset << " limit: " << limit;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<swap_leg_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "valid_to"_c == max) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<swap_leg_entity, domain::swap_leg>(
        ctx,
        query,
        swap_leg_list_order(order, {"trade_id", "leg_number"}, false),
        swap_leg_filter_condition(filter),
        [](const auto& entities) { return swap_leg_mapper::map(entities); },
        lg(),
        "Reading latest swap legs by trade_id.");
}

std::uint32_t swap_leg_repository::get_total_swap_leg_count_by_trade_id(
    const context& ctx,
    const std::string& trade_id,
    const std::optional<messaging::swap_legs_filter>& filter) {
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active swap legs count. trade_id: " << trade_id;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<swap_leg_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "valid_to"_c == max);

    return query_builder::execute_count_query<swap_leg_entity>(
        ctx, query, swap_leg_filter_condition(filter), lg(), "Counting swap legs by trade_id");
}


swap_leg_repository::remove_status
swap_leg_repository::remove(const context& ctx,
                            const std::string& trade_id,
                            const std::string& leg_number,
                            std::optional<std::uint32_t> version) {
    ORES_LOG_SEV(lg(), debug) << "Removing swap leg. " << "trade_id: " << trade_id
                              << " leg_number: " << leg_number;
    const auto current = read_latest(ctx, trade_id, leg_number);
    if (current.empty())
        return remove_status::missing;
    // The protocol states the version as a uint32 and the row carries it as an
    // int, so the comparison states the conversion rather than relying on one.
    if (version && static_cast<std::uint32_t>(current.front().identity.version) != *version)
        return remove_status::conflicting;
    static constexpr auto max = db_timestamp::max();
    // The row is named by its version as well as by its key, so the removal
    // cannot close a row that replaced the one the caller read between the
    // read above and this statement.
    const auto expected = version ? static_cast<int>(*version) : current.front().identity.version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::delete_from<swap_leg_entity> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "leg_number"_c == leg_number && "valid_to"_c == max &&
                                            "version"_c == expected);

    query_builder::execute_delete_query(ctx, query, lg(), "Removing swap leg from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, trade_id, leg_number).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void swap_leg_repository::remove(const context& ctx,
                                 const std::string& trade_id,
                                 const std::string& leg_number) {
    static_cast<void>(remove(ctx, trade_id, leg_number, std::nullopt));
}

std::vector<domain::swap_leg>
swap_leg_repository::read_latest(const context& ctx,
                                 std::uint32_t offset,
                                 std::uint32_t limit,
                                 const ores::utility::domain::order& order,
                                 const std::optional<messaging::swap_legs_filter>& filter,
                                 const std::optional<std::string>& as_of) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest swap legs with offset: " << offset
                              << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<swap_leg_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<swap_leg_entity, domain::swap_leg>(
        ctx,
        query,
        swap_leg_list_order(order, {"trade_id", "leg_number"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of), swap_leg_filter_condition(filter)),
        [](const auto& entities) { return swap_leg_mapper::map(entities); },
        lg(),
        "Reading latest swap legs with pagination.");
}

std::uint32_t swap_leg_repository::get_total_swap_leg_count(
    const context& ctx,
    const std::optional<messaging::swap_legs_filter>& filter,
    const std::optional<std::string>& as_of) {
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active swap leg count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<swap_leg_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<swap_leg_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of), swap_leg_filter_condition(filter)),
        lg(),
        "Counting swap legs");
}

std::vector<domain::swap_leg>
swap_leg_repository::read_latest(const context& ctx,
                                 const std::vector<std::string>& trade_ids,
                                 const std::vector<std::string>& leg_numbers) {
    if (trade_ids.empty() || leg_numbers.empty())
        return {};
    if (leg_numbers.size() != trade_ids.size())
        throw std::invalid_argument(
            "swap_leg_repository::read_latest: key column vectors must be the same length");
    // Every key is a bind and the store's bind count is finite, so a larger
    // key set is read one chunk at a time.
    constexpr std::size_t chunk_size = 1000;
    if (trade_ids.size() > chunk_size) {
        std::vector<domain::swap_leg> all;
        for (std::size_t begin = 0; begin < trade_ids.size(); begin += chunk_size) {
            const auto end = std::min(begin + chunk_size, trade_ids.size());
            auto part = read_latest(
                ctx,
                std::vector<std::string>(trade_ids.begin() + begin, trade_ids.begin() + end),
                std::vector<std::string>(leg_numbers.begin() + begin, leg_numbers.begin() + end));
            all.insert(all.end(),
                       std::make_move_iterator(part.begin()),
                       std::make_move_iterator(part.end()));
        }
        return all;
    }
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<swap_leg_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) &&
                                            "leg_number"_c.in(leg_numbers) && "valid_to"_c == max);
    auto result = query_builder::execute_read_query<swap_leg_entity, domain::swap_leg>(
        ctx,
        query,
        [](const auto& entities) { return swap_leg_mapper::map(entities); },
        lg(),
        "Reading latest swap legs by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (the query builder has no tuple IN), so filter down to the
    // exact requested key-tuples here.
    std::set<std::tuple<std::string, std::string>> requested;
    for (std::size_t i = 0; i < trade_ids.size(); ++i)
        requested.emplace(trade_ids[i], leg_numbers[i]);
    std::vector<domain::swap_leg> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(std::make_tuple(boost::uuids::to_string(item.identity.trade_id),
                                               std::to_string(item.identity.leg_number))))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void swap_leg_repository::remove(const context& ctx,
                                 const std::vector<std::string>& trade_ids,
                                 const std::vector<std::string>& leg_numbers) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (leg_numbers.size() != trade_ids.size())
        throw std::invalid_argument(
            "swap_leg_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < trade_ids.size(); ++i)
        remove(ctx, trade_ids[i], leg_numbers[i]);
}


std::vector<domain::swap_leg>
swap_leg_repository::read_by_instruments_batch(const context& ctx,
                                               const std::vector<std::string>& trade_ids) {
    if (trade_ids.empty())
        return {};
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<swap_leg_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) &&
                                            "valid_to"_c == max) |
                       query_builder::order_by("trade_id"_c, "leg_number"_c);
    return query_builder::execute_read_query<swap_leg_entity, domain::swap_leg>(
        ctx,
        query,
        [](const auto& entities) { return swap_leg_mapper::map(entities); },
        lg(),
        "Reading swap legs for multiple instruments.");
}

}
