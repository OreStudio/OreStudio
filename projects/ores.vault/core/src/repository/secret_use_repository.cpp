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
#include "ores.vault.core/repository/secret_use_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.vault.api/domain/secret_use.hpp"
#include "ores.vault.api/domain/secret_use_json_io.hpp" // IWYU pragma: keep.
#include "ores.vault.api/messaging/secret_use_protocol.hpp"
#include "ores.vault.core/repository/secret_use_entity.hpp"
#include "ores.vault.core/repository/secret_use_mapper.hpp"
#include <boost/uuid/uuid_io.hpp>
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

namespace ores::vault::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string secret_use_repository::sql() {
    return query_builder::insert_statement_sql<secret_use_entity>(lg());
}

bool secret_use_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order secret_use_list_order(const ores::utility::domain::order& order,
                                           std::initializer_list<std::string> default_columns,
                                           bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"id"});
    if (!secret_use_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of uses cannot be ordered by " + order.field + ".");
    return query_builder::make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition>
secret_use_filter_condition(const std::optional<messaging::secret_uses_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("id", std::move(values)));
    }
    return query_builder::all_of(std::move(r));
}

}

namespace {

/**
 * @brief Whether a claim is checked against the row its key names now.
 */
bool secret_use_claim_reads_current(const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    // No version to state, so only a create needs to know whether a row exists.
    return claim.kind == precondition_kind::must_not_exist;
}

using secret_use_rows_by_key = std::map<std::tuple<std::string>, std::vector<domain::secret_use>>;

/**
 * @brief The current rows the objects name, by the text form of their key.
 *
 * One batch read for the set, so a batch checks its claims against one read
 * rather than one read per row.
 */
secret_use_rows_by_key secret_use_current_rows(secret_use_repository& repo,
                                               const ores::database::context& ctx,
                                               const std::vector<domain::secret_use>& of) {
    std::vector<std::string> ids;
    ids.reserve(of.size());
    for (const auto& row : of) {
        ids.push_back(boost::uuids::to_string(row.id));
    }
    secret_use_rows_by_key current;
    for (auto& item : repo.read_latest(ctx, ids))
        current[std::make_tuple(boost::uuids::to_string(item.id))].push_back(std::move(item));
    return current;
}

/**
 * @brief The current rows that the key of an object names, or none.
 */
const std::vector<domain::secret_use>& secret_use_rows_of(const secret_use_rows_by_key& current,
                                                          const domain::secret_use& item) {
    static const std::vector<domain::secret_use> none;
    const auto found = current.find(std::make_tuple(boost::uuids::to_string(item.id)));
    return found == current.end() ? none : found->second;
}

/**
 * @brief The claim a replace makes: the version the row carries now, or
 * that no row exists yet.
 */
ores::utility::domain::precondition
secret_use_replace_claim(const std::vector<domain::secret_use>& current) {
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // when the claim is applied.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

/**
 * @brief The object with the claim's version stamped onto it.
 *
 * The caller passes the row the key names now whenever the claim reads it. A
 * claim the store cannot check is refused here rather than ignored.
 */
domain::secret_use secret_use_apply_claim(const domain::secret_use& v,
                                          const ores::utility::domain::precondition& claim,
                                          const std::vector<domain::secret_use>& current) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "secret_use_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist && !current.empty())
        throw std::invalid_argument("secret_use_repository::write: a current row already exists");
    return t;
}

}

void secret_use_repository::write(const context& ctx, const domain::secret_use& v) {
    write(ctx, v, secret_use_replace_claim(read_latest(ctx, boost::uuids::to_string(v.id))));
}

void secret_use_repository::write(const context& ctx, const std::vector<domain::secret_use>& v) {
    const auto current = secret_use_current_rows(*this, ctx, v);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(secret_use_replace_claim(secret_use_rows_of(current, item)));
    write(ctx, v, claims);
}

void secret_use_repository::write(const context& ctx,
                                  const domain::secret_use& v,
                                  const ores::utility::domain::precondition& claim) {
    ORES_LOG_SEV(lg(), debug) << "Writing use. " << "id: " << v.id;
    const auto t = secret_use_apply_claim(v,
                                          claim,
                                          secret_use_claim_reads_current(claim) ?
                                              read_latest(ctx, boost::uuids::to_string(v.id)) :
                                              std::vector<domain::secret_use>{});
    query_builder::execute_write_op(ctx,
                                    query_builder::insert_or_replace(secret_use_mapper::map(t)),
                                    lg(),
                                    "Writing use to database.");
}

void secret_use_repository::write(const context& ctx,
                                  const std::vector<domain::secret_use>& v,
                                  const std::vector<ores::utility::domain::precondition>& claims) {
    ORES_LOG_SEV(lg(), debug) << "Writing uses. Count: " << v.size();
    if (claims.size() != v.size())
        throw std::invalid_argument(
            "secret_use_repository::write: one claim is needed for each row");
    std::vector<domain::secret_use> to_read;
    for (std::size_t i = 0; i < v.size(); ++i) {
        if (secret_use_claim_reads_current(claims[i]))
            to_read.push_back(v[i]);
    }
    const auto current = secret_use_current_rows(*this, ctx, to_read);
    std::vector<domain::secret_use> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(secret_use_apply_claim(v[i], claims[i], secret_use_rows_of(current, v[i])));
    query_builder::execute_write_op(ctx,
                                    query_builder::insert_or_replace(secret_use_mapper::map(batch)),
                                    lg(),
                                    "Writing uses to database.");
}

std::vector<domain::secret_use> secret_use_repository::read_latest(const context& ctx) {
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<secret_use_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::order_by("id"_c);

    return query_builder::execute_read_query<secret_use_entity, domain::secret_use>(
        ctx,
        query,
        [](const auto& entities) { return secret_use_mapper::map(entities); },
        lg(),
        "Reading latest uses");
}

std::vector<domain::secret_use> secret_use_repository::read_latest(const context& ctx,
                                                                   const std::string& id) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest use. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<secret_use_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id);

    return query_builder::execute_read_query<secret_use_entity, domain::secret_use>(
        ctx,
        query,
        [](const auto& entities) { return secret_use_mapper::map(entities); },
        lg(),
        "Reading latest use by id.");
}


std::vector<domain::secret_use> secret_use_repository::read_all(const context& ctx,
                                                                const std::string& id) {
    ORES_LOG_SEV(lg(), debug) << "Reading all use versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<secret_use_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id) |
                       query_builder::order_by("id"_c);

    return query_builder::execute_read_query<secret_use_entity, domain::secret_use>(
        ctx,
        query,
        [](const auto& entities) { return secret_use_mapper::map(entities); },
        lg(),
        "Reading all use versions by id.");
}


secret_use_repository::remove_status secret_use_repository::remove(
    const context& ctx, const std::string& id, std::optional<std::uint32_t> version) {
    ORES_LOG_SEV(lg(), debug) << "Removing use. " << "id: " << id;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, id);
    if (current.empty())
        return remove_status::missing;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::delete_from<secret_use_entity> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id);

    query_builder::execute_delete_query(ctx, query, lg(), "Removing use from database.");
    return remove_status::removed;
}

void secret_use_repository::remove(const context& ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::secret_use>
secret_use_repository::read_latest(const context& ctx,
                                   std::uint32_t offset,
                                   std::uint32_t limit,
                                   const ores::utility::domain::order& order,
                                   const std::optional<messaging::secret_uses_filter>& filter) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest uses with offset: " << offset
                              << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<secret_use_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<secret_use_entity, domain::secret_use>(
        ctx,
        query,
        secret_use_list_order(order, {"id"}, false),
        secret_use_filter_condition(filter),
        [](const auto& entities) { return secret_use_mapper::map(entities); },
        lg(),
        "Reading latest uses with pagination.");
}

std::uint32_t secret_use_repository::get_total_secret_use_count(
    const context& ctx, const std::optional<messaging::secret_uses_filter>& filter) {
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active use count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<secret_use_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<secret_use_entity>(
        ctx, query, secret_use_filter_condition(filter), lg(), "Counting uses");
}

std::vector<domain::secret_use>
secret_use_repository::read_latest(const context& ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    // Every key is a bind and the store's bind count is finite, so a larger
    // key set is read one chunk at a time.
    constexpr std::size_t chunk_size = 1000;
    if (ids.size() > chunk_size) {
        std::vector<domain::secret_use> all;
        for (std::size_t begin = 0; begin < ids.size(); begin += chunk_size) {
            const auto end = std::min(begin + chunk_size, ids.size());
            auto part =
                read_latest(ctx, std::vector<std::string>(ids.begin() + begin, ids.begin() + end));
            all.insert(all.end(),
                       std::make_move_iterator(part.begin()),
                       std::make_move_iterator(part.end()));
        }
        return all;
    }
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<secret_use_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c.in(ids));
    auto result = query_builder::execute_read_query<secret_use_entity, domain::secret_use>(
        ctx,
        query,
        [](const auto& entities) { return secret_use_mapper::map(entities); },
        lg(),
        "Reading latest uses by ids.");
    return result;
}

void secret_use_repository::remove(const context& ctx, const std::vector<std::string>& ids) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (ids.empty())
        return;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::delete_from<secret_use_entity> |
                       query_builder::where("tenant_id"_c == tid && "id"_c.in(ids));
    query_builder::execute_delete_query(ctx, query, lg(), "Batch removing uses.");
}


}
