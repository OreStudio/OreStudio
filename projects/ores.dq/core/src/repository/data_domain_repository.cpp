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
#include "ores.dq.core/repository/data_domain_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.dq.api/domain/data_domain.hpp"
#include "ores.dq.api/domain/data_domain_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.api/messaging/data_domain_protocol.hpp"
#include "ores.dq.core/repository/data_domain_entity.hpp"
#include "ores.dq.core/repository/data_domain_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <iterator>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

namespace ores::dq::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string data_domain_repository::sql() {
    return query_builder::insert_statement_sql<data_domain_entity>(lg());
}

bool data_domain_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order data_domain_list_order(const ores::utility::domain::order& order,
                                            std::initializer_list<std::string> default_columns,
                                            bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"name"});
    if (!data_domain_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of data domains cannot be ordered by " + order.field +
                                    ".");
    return query_builder::make_order({order.field}, order.descending, {"name"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition>
data_domain_filter_condition(const std::optional<messaging::data_domains_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->name_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->name_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("name", std::move(values)));
    }
    return query_builder::all_of(std::move(r));
}

}

namespace {

/**
 * @brief Whether a claim is checked against the row its key names now.
 */
bool data_domain_claim_reads_current(const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    // The store checks a stated version itself. A claim of nothing still has
    // to state the version it replaces, so only that claim needs the row.
    return claim.kind == precondition_kind::any;
}

using data_domain_rows_by_key = std::map<std::tuple<std::string>, std::vector<domain::data_domain>>;

/**
 * @brief The current rows the objects name, by the text form of their key.
 *
 * One batch read for the set, so a batch checks its claims against one read
 * rather than one read per row.
 */
data_domain_rows_by_key data_domain_current_rows(data_domain_repository& repo,
                                                 const ores::database::context& ctx,
                                                 const std::vector<domain::data_domain>& of) {
    std::vector<std::string> names;
    names.reserve(of.size());
    for (const auto& row : of) {
        names.push_back(row.name);
    }
    data_domain_rows_by_key current;
    for (auto& item : repo.read_latest(ctx, names))
        current[std::make_tuple(item.name)].push_back(std::move(item));
    return current;
}

/**
 * @brief The current rows that the key of an object names, or none.
 */
const std::vector<domain::data_domain>& data_domain_rows_of(const data_domain_rows_by_key& current,
                                                            const domain::data_domain& item) {
    static const std::vector<domain::data_domain> none;
    const auto found = current.find(std::make_tuple(item.name));
    return found == current.end() ? none : found->second;
}

/**
 * @brief The claim a replace makes: the version the row carries now, or
 * that no row exists yet.
 */
ores::utility::domain::precondition
data_domain_replace_claim(const std::vector<domain::data_domain>& current) {
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

/**
 * @brief The object with the claim's version stamped onto it.
 *
 * The caller passes the row the key names now whenever the claim reads it. A
 * claim the store cannot check is refused here rather than ignored.
 */
domain::data_domain data_domain_apply_claim(const domain::data_domain& v,
                                            const ores::utility::domain::precondition& claim,
                                            const std::vector<domain::data_domain>& current) {
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
        case precondition_kind::any:
            // A caller that claims nothing still has to say what it replaces, so
            // the row's current version is stated. A row that moved on between
            // the read and the write is a conflict the trigger raises, never a
            // silent overwrite.
            t.version = current.empty() ? 0 : current.front().version;
            break;
    }
    return t;
}

}

void data_domain_repository::write(const context& ctx, const domain::data_domain& v) {
    write(ctx, v, data_domain_replace_claim(read_latest(ctx, v.name)));
}

void data_domain_repository::write(const context& ctx, const std::vector<domain::data_domain>& v) {
    const auto current = data_domain_current_rows(*this, ctx, v);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(data_domain_replace_claim(data_domain_rows_of(current, item)));
    write(ctx, v, claims);
}

void data_domain_repository::write(const context& ctx,
                                   const domain::data_domain& v,
                                   const ores::utility::domain::precondition& claim) {
    ORES_LOG_SEV(lg(), debug) << "Writing data domain. " << "name: " << v.name;
    const auto t = data_domain_apply_claim(v,
                                           claim,
                                           data_domain_claim_reads_current(claim) ?
                                               read_latest(ctx, v.name) :
                                               std::vector<domain::data_domain>{});
    query_builder::execute_write_query(
        ctx, data_domain_mapper::map(t), lg(), "Writing data domain to database.");
}

void data_domain_repository::write(const context& ctx,
                                   const std::vector<domain::data_domain>& v,
                                   const std::vector<ores::utility::domain::precondition>& claims) {
    ORES_LOG_SEV(lg(), debug) << "Writing data domains. Count: " << v.size();
    if (claims.size() != v.size())
        throw std::invalid_argument(
            "data_domain_repository::write: one claim is needed for each row");
    std::vector<domain::data_domain> to_read;
    for (std::size_t i = 0; i < v.size(); ++i) {
        if (data_domain_claim_reads_current(claims[i]))
            to_read.push_back(v[i]);
    }
    const auto current = data_domain_current_rows(*this, ctx, to_read);
    std::vector<domain::data_domain> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(
            data_domain_apply_claim(v[i], claims[i], data_domain_rows_of(current, v[i])));
    query_builder::execute_write_query(
        ctx, data_domain_mapper::map(batch), lg(), "Writing data domains to database.");
}

std::vector<domain::data_domain> data_domain_repository::read_latest(const context& ctx) {
    static constexpr auto max = db_timestamp::max();
    const auto query = query_builder::read<std::vector<data_domain_entity>> |
                       query_builder::where("valid_to"_c == max) |
                       query_builder::order_by("name"_c);

    return query_builder::execute_read_query<data_domain_entity, domain::data_domain>(
        ctx,
        query,
        [](const auto& entities) { return data_domain_mapper::map(entities); },
        lg(),
        "Reading latest data domains");
}

std::vector<domain::data_domain> data_domain_repository::read_latest(const context& ctx,
                                                                     const std::string& name) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest data domain. " << "name: " << name;
    static constexpr auto max = db_timestamp::max();
    const auto query = query_builder::read<std::vector<data_domain_entity>> |
                       query_builder::where("name"_c == name && "valid_to"_c == max);

    return query_builder::execute_read_query<data_domain_entity, domain::data_domain>(
        ctx,
        query,
        [](const auto& entities) { return data_domain_mapper::map(entities); },
        lg(),
        "Reading latest data domain by name.");
}


std::vector<domain::data_domain> data_domain_repository::read_all(const context& ctx,
                                                                  const std::string& name) {
    ORES_LOG_SEV(lg(), debug) << "Reading all data domain versions. " << "name: " << name;
    const auto query = query_builder::read<std::vector<data_domain_entity>> |
                       query_builder::where("name"_c == name) |
                       query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<data_domain_entity, domain::data_domain>(
        ctx,
        query,
        [](const auto& entities) { return data_domain_mapper::map(entities); },
        lg(),
        "Reading all data domain versions by name.");
}

std::optional<domain::data_domain> data_domain_repository::read_at_version(const context& ctx,
                                                                           const std::string& name,
                                                                           std::uint32_t version) {
    ORES_LOG_SEV(lg(), debug) << "Reading data domain at version. " << "name: " << name
                              << " version: " << version;
    const auto query = query_builder::read<std::vector<data_domain_entity>> |
                       query_builder::where("name"_c == name && "version"_c == version) |
                       query_builder::limit(1);

    const auto entities =
        query_builder::execute_read_query<data_domain_entity, domain::data_domain>(
            ctx,
            query,
            [](const auto& entities) { return data_domain_mapper::map(entities); },
            lg(),
            "Reading data domain at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

data_domain_repository::remove_status data_domain_repository::remove(
    const context& ctx, const std::string& name, std::optional<std::uint32_t> version) {
    ORES_LOG_SEV(lg(), debug) << "Removing data domain. " << "name: " << name;
    const auto current = read_latest(ctx, name);
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
    const auto query = query_builder::delete_from<data_domain_entity> |
                       query_builder::where("tenant_id"_c == tid && "name"_c == name &&
                                            "valid_to"_c == max && "version"_c == expected);

    query_builder::execute_delete_query(ctx, query, lg(), "Removing data domain from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, name).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void data_domain_repository::remove(const context& ctx, const std::string& name) {
    static_cast<void>(remove(ctx, name, std::nullopt));
}

std::vector<domain::data_domain>
data_domain_repository::read_latest(const context& ctx,
                                    std::uint32_t offset,
                                    std::uint32_t limit,
                                    const ores::utility::domain::order& order,
                                    const std::optional<messaging::data_domains_filter>& filter,
                                    const std::optional<std::string>& as_of) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest data domains with offset: " << offset
                              << " and limit: " << limit;
    const auto query = query_builder::read<std::vector<data_domain_entity>> |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<data_domain_entity, domain::data_domain>(
        ctx,
        query,
        data_domain_list_order(order, {"name"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of),
                                data_domain_filter_condition(filter)),
        [](const auto& entities) { return data_domain_mapper::map(entities); },
        lg(),
        "Reading latest data domains with pagination.");
}

std::uint32_t data_domain_repository::get_total_domain_count(
    const context& ctx,
    const std::optional<messaging::data_domains_filter>& filter,
    const std::optional<std::string>& as_of) {
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active data domain count";

    const auto query = query_builder::read<std::vector<data_domain_entity>>;

    return query_builder::execute_count_query<data_domain_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of),
                                data_domain_filter_condition(filter)),
        lg(),
        "Counting data domains");
}

std::vector<domain::data_domain>
data_domain_repository::read_latest(const context& ctx, const std::vector<std::string>& names) {
    if (names.empty())
        return {};
    // Every key is a bind and the store's bind count is finite, so a larger
    // key set is read one chunk at a time.
    constexpr std::size_t chunk_size = 1000;
    if (names.size() > chunk_size) {
        std::vector<domain::data_domain> all;
        for (std::size_t begin = 0; begin < names.size(); begin += chunk_size) {
            const auto end = std::min(begin + chunk_size, names.size());
            auto part = read_latest(
                ctx, std::vector<std::string>(names.begin() + begin, names.begin() + end));
            all.insert(all.end(),
                       std::make_move_iterator(part.begin()),
                       std::make_move_iterator(part.end()));
        }
        return all;
    }
    static constexpr auto max = db_timestamp::max();
    const auto query = query_builder::read<std::vector<data_domain_entity>> |
                       query_builder::where("name"_c.in(names) && "valid_to"_c == max);
    auto result = query_builder::execute_read_query<data_domain_entity, domain::data_domain>(
        ctx,
        query,
        [](const auto& entities) { return data_domain_mapper::map(entities); },
        lg(),
        "Reading latest data domains by ids.");
    return result;
}

void data_domain_repository::remove(const context& ctx, const std::vector<std::string>& names) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (names.empty())
        return;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<data_domain_entity> |
        query_builder::where("tenant_id"_c == tid && "name"_c.in(names) && "valid_to"_c == max);
    query_builder::execute_delete_query(ctx, query, lg(), "Batch removing data domains.");
}


}
