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
#include "ores.iam.core/repository/external_identity_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.iam.api/domain/external_identity.hpp"
#include "ores.iam.api/domain/external_identity_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.api/messaging/external_identity_protocol.hpp"
#include "ores.iam.core/repository/external_identity_entity.hpp"
#include "ores.iam.core/repository/external_identity_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
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

namespace ores::iam::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string external_identity_repository::sql() {
    return query_builder::insert_statement_sql<external_identity_entity>(lg());
}

bool external_identity_repository::is_sortable(std::string_view field) {
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
external_identity_list_order(const ores::utility::domain::order& order,
                             std::initializer_list<std::string> default_columns,
                             bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"id"});
    if (!external_identity_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of external identities cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition> external_identity_filter_condition(
    const std::optional<messaging::external_identities_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->account_id)
        r.push_back(
            query_builder::equals("account_id", query_builder::filter_value(*filter->account_id)));
    if (filter->identity_provider_id)
        r.push_back(query_builder::equals(
            "identity_provider_id", query_builder::filter_value(*filter->identity_provider_id)));
    if (filter->id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("id", std::move(values)));
    }
    if (filter->account_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->account_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("account_id", std::move(values)));
    }
    if (filter->identity_provider_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->identity_provider_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("identity_provider_id", std::move(values)));
    }
    return query_builder::all_of(std::move(r));
}

}

namespace {

/**
 * @brief Whether a claim is checked against the row its key names now.
 */
bool external_identity_claim_reads_current(const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    // The store checks a stated version itself. A claim of nothing still has
    // to state the version it replaces, so only that claim needs the row.
    return claim.kind == precondition_kind::any;
}

using external_identity_rows_by_key =
    std::map<std::tuple<std::string>, std::vector<domain::external_identity>>;

/**
 * @brief The current rows the objects name, by the text form of their key.
 *
 * One batch read for the set, so a batch checks its claims against one read
 * rather than one read per row.
 */
external_identity_rows_by_key
external_identity_current_rows(external_identity_repository& repo,
                               const ores::database::context& ctx,
                               const std::vector<domain::external_identity>& of) {
    std::vector<std::string> ids;
    ids.reserve(of.size());
    for (const auto& row : of) {
        ids.push_back(boost::uuids::to_string(row.id));
    }
    external_identity_rows_by_key current;
    for (auto& item : repo.read_latest(ctx, ids))
        current[std::make_tuple(boost::uuids::to_string(item.id))].push_back(std::move(item));
    return current;
}

/**
 * @brief The current rows that the key of an object names, or none.
 */
const std::vector<domain::external_identity>&
external_identity_rows_of(const external_identity_rows_by_key& current,
                          const domain::external_identity& item) {
    static const std::vector<domain::external_identity> none;
    const auto found = current.find(std::make_tuple(boost::uuids::to_string(item.id)));
    return found == current.end() ? none : found->second;
}

/**
 * @brief The claim a replace makes: the version the row carries now, or
 * that no row exists yet.
 */
ores::utility::domain::precondition
external_identity_replace_claim(const std::vector<domain::external_identity>& current) {
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
domain::external_identity
external_identity_apply_claim(const domain::external_identity& v,
                              const ores::utility::domain::precondition& claim,
                              const std::vector<domain::external_identity>& current) {
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

void external_identity_repository::write(const context& ctx, const domain::external_identity& v) {
    write(ctx, v, external_identity_replace_claim(read_latest(ctx, boost::uuids::to_string(v.id))));
}

void external_identity_repository::write(const context& ctx,
                                         const std::vector<domain::external_identity>& v) {
    const auto current = external_identity_current_rows(*this, ctx, v);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(external_identity_replace_claim(external_identity_rows_of(current, item)));
    write(ctx, v, claims);
}

void external_identity_repository::write(const context& ctx,
                                         const domain::external_identity& v,
                                         const ores::utility::domain::precondition& claim) {
    ORES_LOG_SEV(lg(), debug) << "Writing external identity. " << "id: " << v.id;
    const auto t =
        external_identity_apply_claim(v,
                                      claim,
                                      external_identity_claim_reads_current(claim) ?
                                          read_latest(ctx, boost::uuids::to_string(v.id)) :
                                          std::vector<domain::external_identity>{});
    query_builder::execute_write_query(
        ctx, external_identity_mapper::map(t), lg(), "Writing external identity to database.");
}

void external_identity_repository::write(
    const context& ctx,
    const std::vector<domain::external_identity>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    ORES_LOG_SEV(lg(), debug) << "Writing external identities. Count: " << v.size();
    if (claims.size() != v.size())
        throw std::invalid_argument(
            "external_identity_repository::write: one claim is needed for each row");
    std::vector<domain::external_identity> to_read;
    for (std::size_t i = 0; i < v.size(); ++i) {
        if (external_identity_claim_reads_current(claims[i]))
            to_read.push_back(v[i]);
    }
    const auto current = external_identity_current_rows(*this, ctx, to_read);
    std::vector<domain::external_identity> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(external_identity_apply_claim(
            v[i], claims[i], external_identity_rows_of(current, v[i])));
    query_builder::execute_write_query(ctx,
                                       external_identity_mapper::map(batch),
                                       lg(),
                                       "Writing external identities to database.");
}

std::vector<domain::external_identity>
external_identity_repository::read_latest(const context& ctx) {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<external_identity_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("id"_c);

    return query_builder::execute_read_query<external_identity_entity, domain::external_identity>(
        ctx,
        query,
        [](const auto& entities) { return external_identity_mapper::map(entities); },
        lg(),
        "Reading latest external identities");
}

std::vector<domain::external_identity>
external_identity_repository::read_latest(const context& ctx, const std::string& id) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest external identity. " << "id: " << id;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<external_identity_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max);

    return query_builder::execute_read_query<external_identity_entity, domain::external_identity>(
        ctx,
        query,
        [](const auto& entities) { return external_identity_mapper::map(entities); },
        lg(),
        "Reading latest external identity by id.");
}

std::vector<domain::external_identity>
external_identity_repository::read_latest_by_email(const context& ctx, const std::string& email) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest external identity by email: " << email;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<external_identity_entity>> |
        query_builder::where("tenant_id"_c == tid && "email"_c == email && "valid_to"_c == max);

    return query_builder::execute_read_query<external_identity_entity, domain::external_identity>(
        ctx,
        query,
        [](const auto& entities) { return external_identity_mapper::map(entities); },
        lg(),
        "Reading latest external identity by email.");
}

std::vector<domain::external_identity>
external_identity_repository::read_latest_by_email(const context& ctx,
                                                   const std::vector<std::string>& emails) {
    if (emails.empty())
        return {};
    // Every key is a bind and the store's bind count is finite, so a larger
    // key set is read one chunk at a time.
    constexpr std::size_t chunk_size = 1000;
    if (emails.size() > chunk_size) {
        std::vector<domain::external_identity> all;
        for (std::size_t begin = 0; begin < emails.size(); begin += chunk_size) {
            const auto end = std::min(begin + chunk_size, emails.size());
            auto part = read_latest_by_email(
                ctx, std::vector<std::string>(emails.begin() + begin, emails.begin() + end));
            all.insert(all.end(),
                       std::make_move_iterator(part.begin()),
                       std::make_move_iterator(part.end()));
        }
        return all;
    }
    ORES_LOG_SEV(lg(), debug) << "Reading latest external identities by email. Count: "
                              << emails.size();
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<external_identity_entity>> |
        query_builder::where("tenant_id"_c == tid && "email"_c.in(emails) && "valid_to"_c == max);
    auto found =
        query_builder::execute_read_query<external_identity_entity, domain::external_identity>(
            ctx,
            query,
            [](const auto& entities) { return external_identity_mapper::map(entities); },
            lg(),
            "Reading latest external identities by email.");
    return found;
}

std::vector<domain::external_identity>
external_identity_repository::read_any_by_email(const context& ctx, const std::string& email) {
    ORES_LOG_SEV(lg(), debug) << "Reading any external identity by email: " << email;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<external_identity_entity>> |
                       query_builder::where("tenant_id"_c == tid && "email"_c == email) |
                       query_builder::order_by("valid_from"_c.desc()) | query_builder::limit(1);

    return query_builder::execute_read_query<external_identity_entity, domain::external_identity>(
        ctx,
        query,
        [](const auto& entities) { return external_identity_mapper::map(entities); },
        lg(),
        "Reading any external identity by email.");
}


std::vector<domain::external_identity>
external_identity_repository::read_all(const context& ctx, const std::string& id) {
    ORES_LOG_SEV(lg(), debug) << "Reading all external identity versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<external_identity_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id) |
                       query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<external_identity_entity, domain::external_identity>(
        ctx,
        query,
        [](const auto& entities) { return external_identity_mapper::map(entities); },
        lg(),
        "Reading all external identity versions by id.");
}

std::optional<domain::external_identity> external_identity_repository::read_at_version(
    const context& ctx, const std::string& id, std::uint32_t version) {
    ORES_LOG_SEV(lg(), debug) << "Reading external identity at version. " << "id: " << id
                              << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<external_identity_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
        query_builder::limit(1);

    const auto entities =
        query_builder::execute_read_query<external_identity_entity, domain::external_identity>(
            ctx,
            query,
            [](const auto& entities) { return external_identity_mapper::map(entities); },
            lg(),
            "Reading external identity at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::external_identity> external_identity_repository::read_latest_by_account_id(
    const context& ctx,
    const std::string& account_id,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::external_identities_filter>& filter) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest external identities. account_id: " << account_id
                              << " offset: " << offset << " limit: " << limit;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<external_identity_entity>> |
                       query_builder::where("tenant_id"_c == tid && "account_id"_c == account_id &&
                                            "valid_to"_c == max) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<external_identity_entity,
                                                     domain::external_identity>(
        ctx,
        query,
        external_identity_list_order(order, {"id"}, false),
        external_identity_filter_condition(filter),
        [](const auto& entities) { return external_identity_mapper::map(entities); },
        lg(),
        "Reading latest external identities by account_id.");
}

std::uint32_t external_identity_repository::get_total_link_count_by_account_id(
    const context& ctx,
    const std::string& account_id,
    const std::optional<messaging::external_identities_filter>& filter) {
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active external identities count. account_id: "
                              << account_id;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<external_identity_entity>> |
                       query_builder::where("tenant_id"_c == tid && "account_id"_c == account_id &&
                                            "valid_to"_c == max);

    return query_builder::execute_count_query<external_identity_entity>(
        ctx,
        query,
        external_identity_filter_condition(filter),
        lg(),
        "Counting external identities by account_id");
}


std::vector<domain::external_identity>
external_identity_repository::read_latest_by_identity_provider_id(
    const context& ctx,
    const std::string& identity_provider_id,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::external_identities_filter>& filter) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest external identities. identity_provider_id: "
                              << identity_provider_id << " offset: " << offset
                              << " limit: " << limit;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<external_identity_entity>> |
                       query_builder::where("tenant_id"_c == tid &&
                                            "identity_provider_id"_c == identity_provider_id &&
                                            "valid_to"_c == max) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<external_identity_entity,
                                                     domain::external_identity>(
        ctx,
        query,
        external_identity_list_order(order, {"id"}, false),
        external_identity_filter_condition(filter),
        [](const auto& entities) { return external_identity_mapper::map(entities); },
        lg(),
        "Reading latest external identities by identity_provider_id.");
}

std::uint32_t external_identity_repository::get_total_link_count_by_identity_provider_id(
    const context& ctx,
    const std::string& identity_provider_id,
    const std::optional<messaging::external_identities_filter>& filter) {
    ORES_LOG_SEV(lg(), debug)
        << "Retrieving total active external identities count. identity_provider_id: "
        << identity_provider_id;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<external_identity_entity>> |
                       query_builder::where("tenant_id"_c == tid &&
                                            "identity_provider_id"_c == identity_provider_id &&
                                            "valid_to"_c == max);

    return query_builder::execute_count_query<external_identity_entity>(
        ctx,
        query,
        external_identity_filter_condition(filter),
        lg(),
        "Counting external identities by identity_provider_id");
}


external_identity_repository::remove_status external_identity_repository::remove(
    const context& ctx, const std::string& id, std::optional<std::uint32_t> version) {
    ORES_LOG_SEV(lg(), debug) << "Removing external identity. " << "id: " << id;
    const auto current = read_latest(ctx, id);
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
    const auto query = query_builder::delete_from<external_identity_entity> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id &&
                                            "valid_to"_c == max && "version"_c == expected);

    query_builder::execute_delete_query(
        ctx, query, lg(), "Removing external identity from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void external_identity_repository::remove(const context& ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::external_identity> external_identity_repository::read_latest(
    const context& ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::external_identities_filter>& filter,
    const std::optional<std::string>& as_of) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest external identities with offset: " << offset
                              << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<external_identity_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<external_identity_entity,
                                                     domain::external_identity>(
        ctx,
        query,
        external_identity_list_order(order, {"id"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of),
                                external_identity_filter_condition(filter)),
        [](const auto& entities) { return external_identity_mapper::map(entities); },
        lg(),
        "Reading latest external identities with pagination.");
}

std::uint32_t external_identity_repository::get_total_link_count(
    const context& ctx,
    const std::optional<messaging::external_identities_filter>& filter,
    const std::optional<std::string>& as_of) {
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active external identity count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<external_identity_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<external_identity_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of),
                                external_identity_filter_condition(filter)),
        lg(),
        "Counting external identities");
}

std::vector<domain::external_identity>
external_identity_repository::read_latest(const context& ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    // Every key is a bind and the store's bind count is finite, so a larger
    // key set is read one chunk at a time.
    constexpr std::size_t chunk_size = 1000;
    if (ids.size() > chunk_size) {
        std::vector<domain::external_identity> all;
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
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<external_identity_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max);
    auto result =
        query_builder::execute_read_query<external_identity_entity, domain::external_identity>(
            ctx,
            query,
            [](const auto& entities) { return external_identity_mapper::map(entities); },
            lg(),
            "Reading latest external identities by ids.");
    return result;
}

void external_identity_repository::remove(const context& ctx, const std::vector<std::string>& ids) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (ids.empty())
        return;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<external_identity_entity> |
        query_builder::where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max);
    query_builder::execute_delete_query(ctx, query, lg(), "Batch removing external identities.");
}


}
