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
 * Template: cpp_domain_type_repository_pq.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.iam.core/repository/run_token_issue_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.iam.api/domain/run_token_issue.hpp"
#include "ores.iam.api/domain/run_token_issue_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/run_token_issue_entity.hpp"
#include "ores.iam.core/repository/run_token_issue_mapper.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
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

namespace ores::iam::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string run_token_issue_repository::sql() {
    return query_builder::insert_statement_sql<run_token_issue_entity>(lg());
}

bool run_token_issue_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order run_token_issue_list_order(const ores::utility::domain::order& order,
                                                std::initializer_list<std::string> default_columns,
                                                bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"id", "issued_at"});
    if (!run_token_issue_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of run token issues cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order({order.field}, order.descending, {"id", "issued_at"});
}

}

ores::utility::domain::precondition
run_token_issue_repository::replace_claim(context ctx, const domain::run_token_issue& v) {
    const auto current =
        read_latest(ctx, v.id, ores::platform::time::datetime::to_db_string(v.issued_at));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // by the read in apply_claim.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

domain::run_token_issue
run_token_issue_repository::apply_claim(context ctx,
                                        const domain::run_token_issue& v,
                                        const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "run_token_issue_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist &&
        !read_latest(ctx, v.id, ores::platform::time::datetime::to_db_string(v.issued_at)).empty())
        throw std::invalid_argument(
            "run_token_issue_repository::write: a current row already exists");
    return t;
}

void run_token_issue_repository::write(context ctx, const domain::run_token_issue& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void run_token_issue_repository::write(context ctx, const std::vector<domain::run_token_issue>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void run_token_issue_repository::write(context ctx,
                                       const domain::run_token_issue& v,
                                       const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing issue. " << "id: " << v.id
                               << " issued_at: " << v.issued_at;
    const auto t = apply_claim(ctx, v, claim);
    query_builder::execute_write_op(
        ctx,
        query_builder::insert_or_replace(run_token_issue_mapper::map(t)),
        lg(),
        "Writing issue to database.");
}

void run_token_issue_repository::write(
    context ctx,
    const std::vector<domain::run_token_issue>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing run token issues. Count: " << v.size();
    std::vector<domain::run_token_issue> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    query_builder::execute_write_op(
        ctx,
        query_builder::insert_or_replace(run_token_issue_mapper::map(batch)),
        lg(),
        "Writing run token issues to database.");
}

std::vector<domain::run_token_issue> run_token_issue_repository::read_latest(context ctx) {
    const auto query = query_builder::read<std::vector<run_token_issue_entity>> |
                       query_builder::order_by("id"_c, "issued_at"_c);

    return query_builder::execute_read_query<run_token_issue_entity, domain::run_token_issue>(
        ctx,
        query,
        [](const auto& entities) { return run_token_issue_mapper::map(entities); },
        lg(),
        "Reading latest run token issues");
}

std::vector<domain::run_token_issue> run_token_issue_repository::read_latest(
    context ctx, const std::string& id, const std::string& issued_at) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest issue. " << "id: " << id
                               << " issued_at: " << issued_at;
    const auto query = query_builder::read<std::vector<run_token_issue_entity>> |
                       query_builder::where("id"_c == id && "issued_at"_c == issued_at);

    return query_builder::execute_read_query<run_token_issue_entity, domain::run_token_issue>(
        ctx,
        query,
        [](const auto& entities) { return run_token_issue_mapper::map(entities); },
        lg(),
        "Reading latest issue by id.");
}


std::vector<domain::run_token_issue> run_token_issue_repository::read_all(
    context ctx, const std::string& id, const std::string& issued_at) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all issue versions. " << "id: " << id
                               << " issued_at: " << issued_at;
    const auto query = query_builder::read<std::vector<run_token_issue_entity>> |
                       query_builder::where("id"_c == id && "issued_at"_c == issued_at) |
                       query_builder::order_by("id"_c, "issued_at"_c);

    return query_builder::execute_read_query<run_token_issue_entity, domain::run_token_issue>(
        ctx,
        query,
        [](const auto& entities) { return run_token_issue_mapper::map(entities); },
        lg(),
        "Reading all issue versions by id.");
}


run_token_issue_repository::remove_status
run_token_issue_repository::remove(context ctx,
                                   const std::string& id,
                                   const std::string& issued_at,
                                   std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing issue. " << "id: " << id << " issued_at: " << issued_at;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, id, issued_at);
    if (current.empty())
        return remove_status::missing;
    const auto query = query_builder::delete_from<run_token_issue_entity> |
                       query_builder::where("id"_c == id && "issued_at"_c == issued_at);

    query_builder::execute_delete_query(ctx, query, lg(), "Removing issue from database.");
    return remove_status::removed;
}

void run_token_issue_repository::remove(context ctx,
                                        const std::string& id,
                                        const std::string& issued_at) {
    static_cast<void>(remove(ctx, id, issued_at, std::nullopt));
}

std::vector<domain::run_token_issue>
run_token_issue_repository::read_latest(context ctx,
                                        std::uint32_t offset,
                                        std::uint32_t limit,
                                        const ores::utility::domain::order& order) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest run token issues with offset: " << offset
                               << " and limit: " << limit;
    const auto query = query_builder::read<std::vector<run_token_issue_entity>> |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<run_token_issue_entity,
                                                     domain::run_token_issue>(
        ctx,
        query,
        run_token_issue_list_order(order, {"id", "issued_at"}, false),
        std::nullopt,
        [](const auto& entities) { return run_token_issue_mapper::map(entities); },
        lg(),
        "Reading latest run token issues with pagination.");
}

std::uint32_t run_token_issue_repository::get_total__count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active issue count";

    const auto query = query_builder::read<std::vector<run_token_issue_entity>>;

    return query_builder::execute_count_query<run_token_issue_entity>(
        ctx, query, std::nullopt, lg(), "Counting run token issues");
}

std::vector<domain::run_token_issue> run_token_issue_repository::read_latest(
    context ctx, const std::vector<std::string>& ids, const std::vector<std::string>& issued_ats) {
    if (ids.empty() || issued_ats.empty())
        return {};
    const auto query = query_builder::read<std::vector<run_token_issue_entity>> |
                       query_builder::where("id"_c.in(ids) && "issued_at"_c.in(issued_ats));
    auto result =
        query_builder::execute_read_query<run_token_issue_entity, domain::run_token_issue>(
            ctx,
            query,
            [](const auto& entities) { return run_token_issue_mapper::map(entities); },
            lg(),
            "Reading latest run token issues by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (the query builder has no tuple IN), so filter down to the
    // exact requested key-tuples here.
    if (issued_ats.size() != ids.size())
        throw std::invalid_argument(
            "run_token_issue_repository::read_latest: key column vectors must be the same length");
    std::set<std::tuple<std::string, std::string>> requested;
    for (std::size_t i = 0; i < ids.size(); ++i)
        requested.emplace(ids[i], issued_ats[i]);
    std::vector<domain::run_token_issue> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(std::make_tuple(
                item.id, ores::platform::time::datetime::to_db_string(item.issued_at))))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void run_token_issue_repository::remove(context ctx,
                                        const std::vector<std::string>& ids,
                                        const std::vector<std::string>& issued_ats) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (issued_ats.size() != ids.size())
        throw std::invalid_argument(
            "run_token_issue_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < ids.size(); ++i)
        remove(ctx, ids[i], issued_ats[i]);
}


}
