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
#include "ores.refdata.core/repository/legal_entity_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/legal_entity.hpp"
#include "ores.refdata.api/domain/legal_entity_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/messaging/legal_entity_protocol.hpp"
#include "ores.refdata.core/repository/legal_entity_entity.hpp"
#include "ores.refdata.core/repository/legal_entity_mapper.hpp"
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

namespace ores::refdata::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string legal_entity_repository::sql() {
    return query_builder::insert_statement_sql<legal_entity_entity>(lg());
}

bool legal_entity_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order legal_entity_list_order(const ores::utility::domain::order& order,
                                             std::initializer_list<std::string> default_columns,
                                             bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"lei"});
    if (!legal_entity_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of legal entities cannot be ordered by " + order.field +
                                    ".");
    return query_builder::make_order({order.field}, order.descending, {"lei"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition>
legal_entity_filter_condition(const std::optional<messaging::legal_entities_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->lei_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->lei_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("lei", std::move(values)));
    }
    return query_builder::all_of(std::move(r));
}

}

namespace {

/**
 * @brief Whether a claim is checked against the row its key names now.
 */
bool legal_entity_claim_reads_current(const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    // The store checks a stated version itself. A claim of nothing still has
    // to state the version it replaces, so only that claim needs the row.
    return claim.kind == precondition_kind::any;
}

using legal_entity_rows_by_key =
    std::map<std::tuple<std::string>, std::vector<domain::legal_entity>>;

/**
 * @brief The current rows the objects name, by the text form of their key.
 *
 * One batch read for the set, so a batch checks its claims against one read
 * rather than one read per row.
 */
legal_entity_rows_by_key legal_entity_current_rows(legal_entity_repository& repo,
                                                   const ores::database::context& ctx,
                                                   const std::vector<domain::legal_entity>& of) {
    std::vector<std::string> leis;
    leis.reserve(of.size());
    for (const auto& row : of) {
        leis.push_back(row.lei);
    }
    legal_entity_rows_by_key current;
    for (auto& item : repo.read_latest(ctx, leis))
        current[std::make_tuple(item.lei)].push_back(std::move(item));
    return current;
}

/**
 * @brief The current rows that the key of an object names, or none.
 */
const std::vector<domain::legal_entity>&
legal_entity_rows_of(const legal_entity_rows_by_key& current, const domain::legal_entity& item) {
    static const std::vector<domain::legal_entity> none;
    const auto found = current.find(std::make_tuple(item.lei));
    return found == current.end() ? none : found->second;
}

/**
 * @brief The claim a replace makes: the version the row carries now, or
 * that no row exists yet.
 */
ores::utility::domain::precondition
legal_entity_replace_claim(const std::vector<domain::legal_entity>& current) {
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
domain::legal_entity legal_entity_apply_claim(const domain::legal_entity& v,
                                              const ores::utility::domain::precondition& claim,
                                              const std::vector<domain::legal_entity>& current) {
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

void legal_entity_repository::write(const context& ctx, const domain::legal_entity& v) {
    write(ctx, v, legal_entity_replace_claim(read_latest(ctx, v.lei)));
}

void legal_entity_repository::write(const context& ctx,
                                    const std::vector<domain::legal_entity>& v) {
    const auto current = legal_entity_current_rows(*this, ctx, v);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(legal_entity_replace_claim(legal_entity_rows_of(current, item)));
    write(ctx, v, claims);
}

void legal_entity_repository::write(const context& ctx,
                                    const domain::legal_entity& v,
                                    const ores::utility::domain::precondition& claim) {
    ORES_LOG_SEV(lg(), debug) << "Writing legal entity. " << "lei: " << v.lei;
    const auto t = legal_entity_apply_claim(v,
                                            claim,
                                            legal_entity_claim_reads_current(claim) ?
                                                read_latest(ctx, v.lei) :
                                                std::vector<domain::legal_entity>{});
    query_builder::execute_write_query(
        ctx, legal_entity_mapper::map(t), lg(), "Writing legal entity to database.");
}

void legal_entity_repository::write(
    const context& ctx,
    const std::vector<domain::legal_entity>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    ORES_LOG_SEV(lg(), debug) << "Writing legal entities. Count: " << v.size();
    if (claims.size() != v.size())
        throw std::invalid_argument(
            "legal_entity_repository::write: one claim is needed for each row");
    std::vector<domain::legal_entity> to_read;
    for (std::size_t i = 0; i < v.size(); ++i) {
        if (legal_entity_claim_reads_current(claims[i]))
            to_read.push_back(v[i]);
    }
    const auto current = legal_entity_current_rows(*this, ctx, to_read);
    std::vector<domain::legal_entity> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(
            legal_entity_apply_claim(v[i], claims[i], legal_entity_rows_of(current, v[i])));
    query_builder::execute_write_query(
        ctx, legal_entity_mapper::map(batch), lg(), "Writing legal entities to database.");
}

std::vector<domain::legal_entity> legal_entity_repository::read_latest(const context& ctx) {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<legal_entity_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("lei"_c);

    return query_builder::execute_read_query<legal_entity_entity, domain::legal_entity>(
        ctx,
        query,
        [](const auto& entities) { return legal_entity_mapper::map(entities); },
        lg(),
        "Reading latest legal entities");
}

std::vector<domain::legal_entity> legal_entity_repository::read_latest(const context& ctx,
                                                                       const std::string& lei) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest legal entity. " << "lei: " << lei;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<legal_entity_entity>> |
        query_builder::where("tenant_id"_c == tid && "lei"_c == lei && "valid_to"_c == max);

    return query_builder::execute_read_query<legal_entity_entity, domain::legal_entity>(
        ctx,
        query,
        [](const auto& entities) { return legal_entity_mapper::map(entities); },
        lg(),
        "Reading latest legal entity by lei.");
}


std::vector<domain::legal_entity> legal_entity_repository::read_all(const context& ctx,
                                                                    const std::string& lei) {
    ORES_LOG_SEV(lg(), debug) << "Reading all legal entity versions. " << "lei: " << lei;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<legal_entity_entity>> |
                       query_builder::where("tenant_id"_c == tid && "lei"_c == lei) |
                       query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<legal_entity_entity, domain::legal_entity>(
        ctx,
        query,
        [](const auto& entities) { return legal_entity_mapper::map(entities); },
        lg(),
        "Reading all legal entity versions by lei.");
}

std::optional<domain::legal_entity> legal_entity_repository::read_at_version(
    const context& ctx, const std::string& lei, std::uint32_t version) {
    ORES_LOG_SEV(lg(), debug) << "Reading legal entity at version. " << "lei: " << lei
                              << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<legal_entity_entity>> |
        query_builder::where("tenant_id"_c == tid && "lei"_c == lei && "version"_c == version) |
        query_builder::limit(1);

    const auto entities =
        query_builder::execute_read_query<legal_entity_entity, domain::legal_entity>(
            ctx,
            query,
            [](const auto& entities) { return legal_entity_mapper::map(entities); },
            lg(),
            "Reading legal entity at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

legal_entity_repository::remove_status legal_entity_repository::remove(
    const context& ctx, const std::string& lei, std::optional<std::uint32_t> version) {
    ORES_LOG_SEV(lg(), debug) << "Removing legal entity. " << "lei: " << lei;
    const auto current = read_latest(ctx, lei);
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
    const auto query = query_builder::delete_from<legal_entity_entity> |
                       query_builder::where("tenant_id"_c == tid && "lei"_c == lei &&
                                            "valid_to"_c == max && "version"_c == expected);

    query_builder::execute_delete_query(ctx, query, lg(), "Removing legal entity from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, lei).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void legal_entity_repository::remove(const context& ctx, const std::string& lei) {
    static_cast<void>(remove(ctx, lei, std::nullopt));
}

std::vector<domain::legal_entity>
legal_entity_repository::read_latest(const context& ctx,
                                     std::uint32_t offset,
                                     std::uint32_t limit,
                                     const ores::utility::domain::order& order,
                                     const std::optional<messaging::legal_entities_filter>& filter,
                                     const std::optional<std::string>& as_of) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest legal entities with offset: " << offset
                              << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<legal_entity_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<legal_entity_entity, domain::legal_entity>(
        ctx,
        query,
        legal_entity_list_order(order, {"lei"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of),
                                legal_entity_filter_condition(filter)),
        [](const auto& entities) { return legal_entity_mapper::map(entities); },
        lg(),
        "Reading latest legal entities with pagination.");
}

std::uint32_t legal_entity_repository::get_total_legal_entity_count(
    const context& ctx,
    const std::optional<messaging::legal_entities_filter>& filter,
    const std::optional<std::string>& as_of) {
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active legal entity count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<legal_entity_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<legal_entity_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of),
                                legal_entity_filter_condition(filter)),
        lg(),
        "Counting legal entities");
}

std::vector<domain::legal_entity>
legal_entity_repository::read_latest(const context& ctx, const std::vector<std::string>& leis) {
    if (leis.empty())
        return {};
    // Every key is a bind and the store's bind count is finite, so a larger
    // key set is read one chunk at a time.
    constexpr std::size_t chunk_size = 1000;
    if (leis.size() > chunk_size) {
        std::vector<domain::legal_entity> all;
        for (std::size_t begin = 0; begin < leis.size(); begin += chunk_size) {
            const auto end = std::min(begin + chunk_size, leis.size());
            auto part = read_latest(
                ctx, std::vector<std::string>(leis.begin() + begin, leis.begin() + end));
            all.insert(all.end(),
                       std::make_move_iterator(part.begin()),
                       std::make_move_iterator(part.end()));
        }
        return all;
    }
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<legal_entity_entity>> |
        query_builder::where("tenant_id"_c == tid && "lei"_c.in(leis) && "valid_to"_c == max);
    auto result = query_builder::execute_read_query<legal_entity_entity, domain::legal_entity>(
        ctx,
        query,
        [](const auto& entities) { return legal_entity_mapper::map(entities); },
        lg(),
        "Reading latest legal entities by ids.");
    return result;
}

void legal_entity_repository::remove(const context& ctx, const std::vector<std::string>& leis) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (leis.empty())
        return;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<legal_entity_entity> |
        query_builder::where("tenant_id"_c == tid && "lei"_c.in(leis) && "valid_to"_c == max);
    query_builder::execute_delete_query(ctx, query, lg(), "Batch removing legal entities.");
}


}
