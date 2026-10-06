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
#include "ores.trading.core/repository/equity_position_option_underlying_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/equity_position_option_underlying.hpp"
#include "ores.trading.api/domain/equity_position_option_underlying_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/equity_position_option_underlying_entity.hpp"
#include "ores.trading.core/repository/equity_position_option_underlying_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <set>
#include <sqlgen/delete_from.hpp>
#include <sqlgen/dynamic/OrderBy.hpp>
#include <sqlgen/limit.hpp>
#include <sqlgen/literals.hpp>
#include <sqlgen/offset.hpp>
#include <sqlgen/order_by.hpp>
#include <sqlgen/read.hpp>
#include <sqlgen/where.hpp>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string equity_position_option_underlying_repository::sql() {
    return generate_create_table_sql<equity_position_option_underlying_entity>(lg());
}

bool equity_position_option_underlying_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
sqlgen::dynamic::OrderBy list_order(const ores::utility::domain::order& order,
                                    std::initializer_list<std::string> default_columns,
                                    bool default_descending) {
    if (order.field.empty())
        return make_order(default_columns,
                          default_descending != order.descending,
                          {"trade_id", "sequence_number"});
    if (!equity_position_option_underlying_repository::is_sortable(order.field))
        throw std::invalid_argument(
            "A list of equity position option underlyings cannot be ordered by " + order.field +
            ".");
    return make_order({order.field}, order.descending, {"trade_id", "sequence_number"});
}

}

ores::utility::domain::precondition equity_position_option_underlying_repository::replace_claim(
    context ctx, const domain::equity_position_option_underlying& v) {
    const auto current =
        read_latest(ctx, boost::uuids::to_string(v.trade_id), std::to_string(v.sequence_number));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::equity_position_option_underlying equity_position_option_underlying_repository::apply_claim(
    context ctx,
    const domain::equity_position_option_underlying& v,
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

void equity_position_option_underlying_repository::write(
    context ctx, const domain::equity_position_option_underlying& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void equity_position_option_underlying_repository::write(
    context ctx, const std::vector<domain::equity_position_option_underlying>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void equity_position_option_underlying_repository::write(
    context ctx,
    const domain::equity_position_option_underlying& v,
    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing equity position option underlying. "
                               << "trade_id: " << v.trade_id
                               << " sequence_number: " << v.sequence_number;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(ctx,
                        equity_position_option_underlying_mapper::map(t),
                        lg(),
                        "Writing equity position option underlying to database.");
}

void equity_position_option_underlying_repository::write(
    context ctx,
    const std::vector<domain::equity_position_option_underlying>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing equity position option underlyings. Count: " << v.size();
    std::vector<domain::equity_position_option_underlying> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(ctx,
                        equity_position_option_underlying_mapper::map(batch),
                        lg(),
                        "Writing equity position option underlyings to database.");
}

std::vector<domain::equity_position_option_underlying>
equity_position_option_underlying_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<equity_position_option_underlying_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("trade_id"_c, "sequence_number"_c);

    return execute_read_query<equity_position_option_underlying_entity,
                              domain::equity_position_option_underlying>(
        ctx,
        query,
        [](const auto& entities) {
            return equity_position_option_underlying_mapper::map(entities);
        },
        lg(),
        "Reading latest equity position option underlyings");
}

std::vector<domain::equity_position_option_underlying>
equity_position_option_underlying_repository::read_latest(context ctx,
                                                          const std::string& trade_id,
                                                          const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest equity position option underlying. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<equity_position_option_underlying_entity>> |
                       where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                             "sequence_number"_c == sequence_number && "valid_to"_c == max.value());

    return execute_read_query<equity_position_option_underlying_entity,
                              domain::equity_position_option_underlying>(
        ctx,
        query,
        [](const auto& entities) {
            return equity_position_option_underlying_mapper::map(entities);
        },
        lg(),
        "Reading latest equity position option underlying by trade_id.");
}


std::vector<domain::equity_position_option_underlying>
equity_position_option_underlying_repository::read_all(context ctx,
                                                       const std::string& trade_id,
                                                       const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all equity position option underlying versions. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<equity_position_option_underlying_entity>> |
                       where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                             "sequence_number"_c == sequence_number) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<equity_position_option_underlying_entity,
                              domain::equity_position_option_underlying>(
        ctx,
        query,
        [](const auto& entities) {
            return equity_position_option_underlying_mapper::map(entities);
        },
        lg(),
        "Reading all equity position option underlying versions by trade_id.");
}

std::optional<domain::equity_position_option_underlying>
equity_position_option_underlying_repository::read_at_version(context ctx,
                                                              const std::string& trade_id,
                                                              const std::string& sequence_number,
                                                              std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading equity position option underlying at version. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<equity_position_option_underlying_entity>> |
                       where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                             "sequence_number"_c == sequence_number && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<equity_position_option_underlying_entity,
                                             domain::equity_position_option_underlying>(
        ctx,
        query,
        [](const auto& entities) {
            return equity_position_option_underlying_mapper::map(entities);
        },
        lg(),
        "Reading equity position option underlying at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}


equity_position_option_underlying_repository::remove_status
equity_position_option_underlying_repository::remove(context ctx,
                                                     const std::string& trade_id,
                                                     const std::string& sequence_number,
                                                     std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing equity position option underlying. "
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
    const auto query = sqlgen::delete_from<equity_position_option_underlying_entity> |
                       where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                             "sequence_number"_c == sequence_number &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(
        ctx, query, lg(), "Removing equity position option underlying from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, trade_id, sequence_number).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void equity_position_option_underlying_repository::remove(context ctx,
                                                          const std::string& trade_id,
                                                          const std::string& sequence_number) {
    static_cast<void>(remove(ctx, trade_id, sequence_number, std::nullopt));
}

std::vector<domain::equity_position_option_underlying>
equity_position_option_underlying_repository::read_latest(context ctx,
                                                          std::uint32_t offset,
                                                          std::uint32_t limit,
                                                          const ores::utility::domain::order& order,
                                                          const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest equity position option underlyings with offset: "
                               << offset << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<equity_position_option_underlying_entity>> |
                       where("tenant_id"_c == tid) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<equity_position_option_underlying_entity,
                                      domain::equity_position_option_underlying>(
        ctx,
        query,
        list_order(order, {"trade_id", "sequence_number"}, false),
        narrowed(valid_at(as_of), std::nullopt),
        [](const auto& entities) {
            return equity_position_option_underlying_mapper::map(entities);
        },
        lg(),
        "Reading latest equity position option underlyings with pagination.");
}

std::uint32_t
equity_position_option_underlying_repository::get_total_equity_position_option_underlying_count(
    context ctx, const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active equity position option underlying count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<equity_position_option_underlying_entity>> |
                       where("tenant_id"_c == tid);

    return execute_count_query<equity_position_option_underlying_entity>(
        ctx,
        query,
        narrowed(valid_at(as_of), std::nullopt),
        lg(),
        "Counting equity position option underlyings");
}

std::vector<domain::equity_position_option_underlying>
equity_position_option_underlying_repository::read_latest(
    context ctx,
    const std::vector<std::string>& trade_ids,
    const std::vector<std::string>& sequence_numbers) {
    if (trade_ids.empty() || sequence_numbers.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<equity_position_option_underlying_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) &&
              "sequence_number"_c.in(sequence_numbers) && "valid_to"_c == max.value());
    auto result = execute_read_query<equity_position_option_underlying_entity,
                                     domain::equity_position_option_underlying>(
        ctx,
        query,
        [](const auto& entities) {
            return equity_position_option_underlying_mapper::map(entities);
        },
        lg(),
        "Reading latest equity position option underlyings by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (sqlgen has no tuple/composite IN), so filter down to the
    // exact requested key-tuples here.
    if (sequence_numbers.size() != trade_ids.size())
        throw std::invalid_argument("equity_position_option_underlying_repository::read_latest: "
                                    "key column vectors must be the same length");
    std::set<std::tuple<std::string, std::string>> requested;
    for (std::size_t i = 0; i < trade_ids.size(); ++i)
        requested.emplace(trade_ids[i], sequence_numbers[i]);
    std::vector<domain::equity_position_option_underlying> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(std::make_tuple(boost::uuids::to_string(item.trade_id),
                                               std::to_string(item.sequence_number))))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void equity_position_option_underlying_repository::remove(
    context ctx,
    const std::vector<std::string>& trade_ids,
    const std::vector<std::string>& sequence_numbers) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (sequence_numbers.size() != trade_ids.size())
        throw std::invalid_argument("equity_position_option_underlying_repository::remove: key "
                                    "column vectors must be the same length");
    for (std::size_t i = 0; i < trade_ids.size(); ++i)
        remove(ctx, trade_ids[i], sequence_numbers[i]);
}


std::vector<domain::equity_position_option_underlying>
equity_position_option_underlying_repository::read_by_instruments_batch(
    context ctx, const std::vector<std::string>& trade_ids) {
    if (trade_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<equity_position_option_underlying_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) && "valid_to"_c == max.value()) |
        order_by("trade_id"_c, "sequence_number"_c);
    return execute_read_query<equity_position_option_underlying_entity,
                              domain::equity_position_option_underlying>(
        ctx,
        query,
        [](const auto& entities) {
            return equity_position_option_underlying_mapper::map(entities);
        },
        lg(),
        "Reading equity position option underlyings for multiple instruments.");
}

}
