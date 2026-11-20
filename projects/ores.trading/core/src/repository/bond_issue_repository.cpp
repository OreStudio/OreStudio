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
#include "ores.trading.core/repository/bond_issue_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.trading.api/domain/bond_issue.hpp"
#include "ores.trading.api/domain/bond_issue_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/bond_issue_protocol.hpp"
#include "ores.trading.core/repository/bond_issue_entity.hpp"
#include "ores.trading.core/repository/bond_issue_mapper.hpp"
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

namespace ores::trading::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string bond_issue_repository::sql() {
    return query_builder::insert_statement_sql<bond_issue_entity>(lg());
}

bool bond_issue_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order bond_issue_list_order(const ores::utility::domain::order& order,
                                           std::initializer_list<std::string> default_columns,
                                           bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"issue_id"});
    if (!bond_issue_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of bond issues cannot be ordered by " + order.field +
                                    ".");
    return query_builder::make_order({order.field}, order.descending, {"issue_id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition>
bond_issue_filter_condition(const std::optional<messaging::bond_issues_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->issue_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->issue_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("issue_id", std::move(values)));
    }
    return query_builder::all_of(std::move(r));
}

}

namespace {

/**
 * @brief Whether a claim is checked against the row its key names now.
 */
bool bond_issue_claim_reads_current(const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    // The store checks a stated version itself. A claim of nothing still has
    // to state the version it replaces, so only that claim needs the row.
    return claim.kind == precondition_kind::any;
}

using bond_issue_rows_by_key = std::map<std::tuple<std::string>, std::vector<domain::bond_issue>>;

/**
 * @brief The current rows the objects name, by the text form of their key.
 *
 * One batch read for the set, so a batch checks its claims against one read
 * rather than one read per row.
 */
bond_issue_rows_by_key bond_issue_current_rows(bond_issue_repository& repo,
                                               const ores::database::context& ctx,
                                               const std::vector<domain::bond_issue>& of) {
    std::vector<std::string> issue_ids;
    issue_ids.reserve(of.size());
    for (const auto& row : of) {
        issue_ids.push_back(boost::uuids::to_string(row.issue_id));
    }
    bond_issue_rows_by_key current;
    for (auto& item : repo.read_latest(ctx, issue_ids))
        current[std::make_tuple(boost::uuids::to_string(item.issue_id))].push_back(std::move(item));
    return current;
}

/**
 * @brief The current rows that the key of an object names, or none.
 */
const std::vector<domain::bond_issue>& bond_issue_rows_of(const bond_issue_rows_by_key& current,
                                                          const domain::bond_issue& item) {
    static const std::vector<domain::bond_issue> none;
    const auto found = current.find(std::make_tuple(boost::uuids::to_string(item.issue_id)));
    return found == current.end() ? none : found->second;
}

/**
 * @brief The claim a replace makes: the version the row carries now, or
 * that no row exists yet.
 */
ores::utility::domain::precondition
bond_issue_replace_claim(const std::vector<domain::bond_issue>& current) {
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
domain::bond_issue bond_issue_apply_claim(const domain::bond_issue& v,
                                          const ores::utility::domain::precondition& claim,
                                          const std::vector<domain::bond_issue>& current) {
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

void bond_issue_repository::write(const context& ctx, const domain::bond_issue& v) {
    write(ctx, v, bond_issue_replace_claim(read_latest(ctx, boost::uuids::to_string(v.issue_id))));
}

void bond_issue_repository::write(const context& ctx, const std::vector<domain::bond_issue>& v) {
    const auto current = bond_issue_current_rows(*this, ctx, v);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(bond_issue_replace_claim(bond_issue_rows_of(current, item)));
    write(ctx, v, claims);
}

void bond_issue_repository::write(const context& ctx,
                                  const domain::bond_issue& v,
                                  const ores::utility::domain::precondition& claim) {
    ORES_LOG_SEV(lg(), debug) << "Writing bond issue. " << "issue_id: " << v.issue_id;
    const auto t =
        bond_issue_apply_claim(v,
                               claim,
                               bond_issue_claim_reads_current(claim) ?
                                   read_latest(ctx, boost::uuids::to_string(v.issue_id)) :
                                   std::vector<domain::bond_issue>{});
    query_builder::execute_write_query(
        ctx, bond_issue_mapper::map(t), lg(), "Writing bond issue to database.");
}

void bond_issue_repository::write(const context& ctx,
                                  const std::vector<domain::bond_issue>& v,
                                  const std::vector<ores::utility::domain::precondition>& claims) {
    ORES_LOG_SEV(lg(), debug) << "Writing bond issues. Count: " << v.size();
    if (claims.size() != v.size())
        throw std::invalid_argument(
            "bond_issue_repository::write: one claim is needed for each row");
    std::vector<domain::bond_issue> to_read;
    for (std::size_t i = 0; i < v.size(); ++i) {
        if (bond_issue_claim_reads_current(claims[i]))
            to_read.push_back(v[i]);
    }
    const auto current = bond_issue_current_rows(*this, ctx, to_read);
    std::vector<domain::bond_issue> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(bond_issue_apply_claim(v[i], claims[i], bond_issue_rows_of(current, v[i])));
    query_builder::execute_write_query(
        ctx, bond_issue_mapper::map(batch), lg(), "Writing bond issues to database.");
}

std::vector<domain::bond_issue> bond_issue_repository::read_latest(const context& ctx) {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<bond_issue_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("issue_id"_c);

    return query_builder::execute_read_query<bond_issue_entity, domain::bond_issue>(
        ctx,
        query,
        [](const auto& entities) { return bond_issue_mapper::map(entities); },
        lg(),
        "Reading latest bond issues");
}

std::vector<domain::bond_issue> bond_issue_repository::read_latest(const context& ctx,
                                                                   const std::string& issue_id) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest bond issue. " << "issue_id: " << issue_id;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<bond_issue_entity>> |
                       query_builder::where("tenant_id"_c == tid && "issue_id"_c == issue_id &&
                                            "valid_to"_c == max);

    return query_builder::execute_read_query<bond_issue_entity, domain::bond_issue>(
        ctx,
        query,
        [](const auto& entities) { return bond_issue_mapper::map(entities); },
        lg(),
        "Reading latest bond issue by issue_id.");
}


std::vector<domain::bond_issue> bond_issue_repository::read_all(const context& ctx,
                                                                const std::string& issue_id) {
    ORES_LOG_SEV(lg(), debug) << "Reading all bond issue versions. " << "issue_id: " << issue_id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<bond_issue_entity>> |
                       query_builder::where("tenant_id"_c == tid && "issue_id"_c == issue_id) |
                       query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<bond_issue_entity, domain::bond_issue>(
        ctx,
        query,
        [](const auto& entities) { return bond_issue_mapper::map(entities); },
        lg(),
        "Reading all bond issue versions by issue_id.");
}

std::optional<domain::bond_issue> bond_issue_repository::read_at_version(
    const context& ctx, const std::string& issue_id, std::uint32_t version) {
    ORES_LOG_SEV(lg(), debug) << "Reading bond issue at version. " << "issue_id: " << issue_id
                              << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<bond_issue_entity>> |
                       query_builder::where("tenant_id"_c == tid && "issue_id"_c == issue_id &&
                                            "version"_c == version) |
                       query_builder::limit(1);

    const auto entities = query_builder::execute_read_query<bond_issue_entity, domain::bond_issue>(
        ctx,
        query,
        [](const auto& entities) { return bond_issue_mapper::map(entities); },
        lg(),
        "Reading bond issue at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}


bond_issue_repository::remove_status bond_issue_repository::remove(
    const context& ctx, const std::string& issue_id, std::optional<std::uint32_t> version) {
    ORES_LOG_SEV(lg(), debug) << "Removing bond issue. " << "issue_id: " << issue_id;
    const auto current = read_latest(ctx, issue_id);
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
    const auto query = query_builder::delete_from<bond_issue_entity> |
                       query_builder::where("tenant_id"_c == tid && "issue_id"_c == issue_id &&
                                            "valid_to"_c == max && "version"_c == expected);

    query_builder::execute_delete_query(ctx, query, lg(), "Removing bond issue from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, issue_id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void bond_issue_repository::remove(const context& ctx, const std::string& issue_id) {
    static_cast<void>(remove(ctx, issue_id, std::nullopt));
}

std::vector<domain::bond_issue>
bond_issue_repository::read_latest(const context& ctx,
                                   std::uint32_t offset,
                                   std::uint32_t limit,
                                   const ores::utility::domain::order& order,
                                   const std::optional<messaging::bond_issues_filter>& filter,
                                   const std::optional<std::string>& as_of) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest bond issues with offset: " << offset
                              << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<bond_issue_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<bond_issue_entity, domain::bond_issue>(
        ctx,
        query,
        bond_issue_list_order(order, {"issue_id"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of),
                                bond_issue_filter_condition(filter)),
        [](const auto& entities) { return bond_issue_mapper::map(entities); },
        lg(),
        "Reading latest bond issues with pagination.");
}

std::uint32_t bond_issue_repository::get_total_issue_count(
    const context& ctx,
    const std::optional<messaging::bond_issues_filter>& filter,
    const std::optional<std::string>& as_of) {
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active bond issue count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<bond_issue_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<bond_issue_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of),
                                bond_issue_filter_condition(filter)),
        lg(),
        "Counting bond issues");
}

std::vector<domain::bond_issue>
bond_issue_repository::read_latest(const context& ctx, const std::vector<std::string>& issue_ids) {
    if (issue_ids.empty())
        return {};
    // Every key is a bind and the store's bind count is finite, so a larger
    // key set is read one chunk at a time.
    constexpr std::size_t chunk_size = 1000;
    if (issue_ids.size() > chunk_size) {
        std::vector<domain::bond_issue> all;
        for (std::size_t begin = 0; begin < issue_ids.size(); begin += chunk_size) {
            const auto end = std::min(begin + chunk_size, issue_ids.size());
            auto part = read_latest(
                ctx, std::vector<std::string>(issue_ids.begin() + begin, issue_ids.begin() + end));
            all.insert(all.end(),
                       std::make_move_iterator(part.begin()),
                       std::make_move_iterator(part.end()));
        }
        return all;
    }
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<bond_issue_entity>> |
                       query_builder::where("tenant_id"_c == tid && "issue_id"_c.in(issue_ids) &&
                                            "valid_to"_c == max);
    auto result = query_builder::execute_read_query<bond_issue_entity, domain::bond_issue>(
        ctx,
        query,
        [](const auto& entities) { return bond_issue_mapper::map(entities); },
        lg(),
        "Reading latest bond issues by ids.");
    return result;
}

void bond_issue_repository::remove(const context& ctx, const std::vector<std::string>& issue_ids) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (issue_ids.empty())
        return;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::delete_from<bond_issue_entity> |
                       query_builder::where("tenant_id"_c == tid && "issue_id"_c.in(issue_ids) &&
                                            "valid_to"_c == max);
    query_builder::execute_delete_query(ctx, query, lg(), "Batch removing bond issues.");
}


}
