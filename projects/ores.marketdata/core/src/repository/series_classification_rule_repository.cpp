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
#include "ores.marketdata.core/repository/series_classification_rule_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/series_classification_rule.hpp"
#include "ores.marketdata.api/domain/series_classification_rule_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/series_classification_rule_entity.hpp"
#include "ores.marketdata.core/repository/series_classification_rule_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
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

namespace ores::marketdata::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string series_classification_rule_repository::sql() {
    return query_builder::insert_statement_sql<series_classification_rule_entity>(lg());
}

bool series_classification_rule_repository::is_sortable(std::string_view field) {
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
series_classification_rule_list_order(const ores::utility::domain::order& order,
                                      std::initializer_list<std::string> default_columns,
                                      bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(default_columns,
                                         default_descending != order.descending,
                                         {"instrument", "quote", "key_kind"});
    if (!series_classification_rule_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of series classification rules cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order(
        {order.field}, order.descending, {"instrument", "quote", "key_kind"});
}

}

ores::utility::domain::precondition
series_classification_rule_repository::replace_claim(context ctx,
                                                     const domain::series_classification_rule& v) {
    const auto current = read_latest(ctx, v.instrument, v.quote, v.key_kind);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::series_classification_rule series_classification_rule_repository::apply_claim(
    context ctx,
    const domain::series_classification_rule& v,
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
            const auto current = read_latest(ctx, v.instrument, v.quote, v.key_kind);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void series_classification_rule_repository::write(context ctx,
                                                  const domain::series_classification_rule& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void series_classification_rule_repository::write(
    context ctx, const std::vector<domain::series_classification_rule>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void series_classification_rule_repository::write(
    context ctx,
    const domain::series_classification_rule& v,
    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing series classification rule. "
                               << "instrument: " << v.instrument << " quote: " << v.quote
                               << " key_kind: " << v.key_kind;
    const auto t = apply_claim(ctx, v, claim);
    query_builder::execute_write_query(ctx,
                                       series_classification_rule_mapper::map(t),
                                       lg(),
                                       "Writing series classification rule to database.");
}

void series_classification_rule_repository::write(
    context ctx,
    const std::vector<domain::series_classification_rule>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing series classification rules. Count: " << v.size();
    std::vector<domain::series_classification_rule> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    query_builder::execute_write_query(ctx,
                                       series_classification_rule_mapper::map(batch),
                                       lg(),
                                       "Writing series classification rules to database.");
}

std::vector<domain::series_classification_rule>
series_classification_rule_repository::read_latest(context ctx) {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<series_classification_rule_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("instrument"_c, "quote"_c, "key_kind"_c);

    return query_builder::execute_read_query<series_classification_rule_entity,
                                             domain::series_classification_rule>(
        ctx,
        query,
        [](const auto& entities) { return series_classification_rule_mapper::map(entities); },
        lg(),
        "Reading latest series classification rules");
}

std::vector<domain::series_classification_rule>
series_classification_rule_repository::read_latest(context ctx,
                                                   const std::string& instrument,
                                                   const std::string& quote,
                                                   const std::string& key_kind) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest series classification rule. "
                               << "instrument: " << instrument << " quote: " << quote
                               << " key_kind: " << key_kind;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<series_classification_rule_entity>> |
        query_builder::where("tenant_id"_c == tid && "instrument"_c == instrument &&
                             "quote"_c == quote && "key_kind"_c == key_kind && "valid_to"_c == max);

    return query_builder::execute_read_query<series_classification_rule_entity,
                                             domain::series_classification_rule>(
        ctx,
        query,
        [](const auto& entities) { return series_classification_rule_mapper::map(entities); },
        lg(),
        "Reading latest series classification rule by instrument.");
}


std::vector<domain::series_classification_rule>
series_classification_rule_repository::read_all(context ctx,
                                                const std::string& instrument,
                                                const std::string& quote,
                                                const std::string& key_kind) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all series classification rule versions. "
                               << "instrument: " << instrument << " quote: " << quote
                               << " key_kind: " << key_kind;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<series_classification_rule_entity>> |
                       query_builder::where("tenant_id"_c == tid && "instrument"_c == instrument &&
                                            "quote"_c == quote && "key_kind"_c == key_kind) |
                       query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<series_classification_rule_entity,
                                             domain::series_classification_rule>(
        ctx,
        query,
        [](const auto& entities) { return series_classification_rule_mapper::map(entities); },
        lg(),
        "Reading all series classification rule versions by instrument.");
}

std::optional<domain::series_classification_rule>
series_classification_rule_repository::read_at_version(context ctx,
                                                       const std::string& instrument,
                                                       const std::string& quote,
                                                       const std::string& key_kind,
                                                       std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading series classification rule at version. "
                               << "instrument: " << instrument << " quote: " << quote
                               << " key_kind: " << key_kind << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<series_classification_rule_entity>> |
                       query_builder::where("tenant_id"_c == tid && "instrument"_c == instrument &&
                                            "quote"_c == quote && "key_kind"_c == key_kind &&
                                            "version"_c == version) |
                       query_builder::limit(1);

    const auto entities = query_builder::execute_read_query<series_classification_rule_entity,
                                                            domain::series_classification_rule>(
        ctx,
        query,
        [](const auto& entities) { return series_classification_rule_mapper::map(entities); },
        lg(),
        "Reading series classification rule at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

series_classification_rule_repository::remove_status
series_classification_rule_repository::remove(context ctx,
                                              const std::string& instrument,
                                              const std::string& quote,
                                              const std::string& key_kind,
                                              std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing series classification rule. "
                               << "instrument: " << instrument << " quote: " << quote
                               << " key_kind: " << key_kind;
    const auto current = read_latest(ctx, instrument, quote, key_kind);
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
    const auto query = query_builder::delete_from<series_classification_rule_entity> |
                       query_builder::where("tenant_id"_c == tid && "instrument"_c == instrument &&
                                            "quote"_c == quote && "key_kind"_c == key_kind &&
                                            "valid_to"_c == max && "version"_c == expected);

    query_builder::execute_delete_query(
        ctx, query, lg(), "Removing series classification rule from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, instrument, quote, key_kind).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void series_classification_rule_repository::remove(context ctx,
                                                   const std::string& instrument,
                                                   const std::string& quote,
                                                   const std::string& key_kind) {
    static_cast<void>(remove(ctx, instrument, quote, key_kind, std::nullopt));
}

std::vector<domain::series_classification_rule>
series_classification_rule_repository::read_latest(context ctx,
                                                   std::uint32_t offset,
                                                   std::uint32_t limit,
                                                   const ores::utility::domain::order& order,
                                                   const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest series classification rules with offset: "
                               << offset << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<series_classification_rule_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<series_classification_rule_entity,
                                                     domain::series_classification_rule>(
        ctx,
        query,
        series_classification_rule_list_order(order, {"instrument", "quote", "key_kind"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of), std::nullopt),
        [](const auto& entities) { return series_classification_rule_mapper::map(entities); },
        lg(),
        "Reading latest series classification rules with pagination.");
}

std::uint32_t series_classification_rule_repository::get_total_rule_count(
    context ctx, const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active series classification rule count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<series_classification_rule_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<series_classification_rule_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of), std::nullopt),
        lg(),
        "Counting series classification rules");
}

std::vector<domain::series_classification_rule>
series_classification_rule_repository::read_latest(context ctx,
                                                   const std::vector<std::string>& instruments,
                                                   const std::vector<std::string>& quotes,
                                                   const std::vector<std::string>& key_kinds) {
    if (instruments.empty() || quotes.empty() || key_kinds.empty())
        return {};
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<series_classification_rule_entity>> |
        query_builder::where("tenant_id"_c == tid && "instrument"_c.in(instruments) &&
                             "quote"_c.in(quotes) && "key_kind"_c.in(key_kinds) &&
                             "valid_to"_c == max);
    auto result = query_builder::execute_read_query<series_classification_rule_entity,
                                                    domain::series_classification_rule>(
        ctx,
        query,
        [](const auto& entities) { return series_classification_rule_mapper::map(entities); },
        lg(),
        "Reading latest series classification rules by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (the query builder has no tuple IN), so filter down to the
    // exact requested key-tuples here.
    if (quotes.size() != instruments.size() || key_kinds.size() != instruments.size())
        throw std::invalid_argument("series_classification_rule_repository::read_latest: key "
                                    "column vectors must be the same length");
    std::set<std::tuple<std::string, std::string, std::string>> requested;
    for (std::size_t i = 0; i < instruments.size(); ++i)
        requested.emplace(instruments[i], quotes[i], key_kinds[i]);
    std::vector<domain::series_classification_rule> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(std::make_tuple(item.instrument, item.quote, item.key_kind)))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void series_classification_rule_repository::remove(context ctx,
                                                   const std::vector<std::string>& instruments,
                                                   const std::vector<std::string>& quotes,
                                                   const std::vector<std::string>& key_kinds) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (quotes.size() != instruments.size() || key_kinds.size() != instruments.size())
        throw std::invalid_argument("series_classification_rule_repository::remove: key column "
                                    "vectors must be the same length");
    for (std::size_t i = 0; i < instruments.size(); ++i)
        remove(ctx, instruments[i], quotes[i], key_kinds[i]);
}


}
