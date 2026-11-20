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
#include "ores.refdata.core/repository/local_jurisdiction_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.refdata.api/domain/local_jurisdiction.hpp"
#include "ores.refdata.api/domain/local_jurisdiction_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/local_jurisdiction_entity.hpp"
#include "ores.refdata.core/repository/local_jurisdiction_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
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

namespace ores::refdata::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string local_jurisdiction_repository::sql() {
    return query_builder::insert_statement_sql<local_jurisdiction_entity>(lg());
}

bool local_jurisdiction_repository::is_sortable(std::string_view field) {
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
local_jurisdiction_list_order(const ores::utility::domain::order& order,
                              std::initializer_list<std::string> default_columns,
                              bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(default_columns,
                                         default_descending != order.descending,
                                         {"code", "coding_scheme_code"});
    if (!local_jurisdiction_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of local jurisdictions cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order(
        {order.field}, order.descending, {"code", "coding_scheme_code"});
}

}

namespace {

/**
 * @brief Whether a claim is checked against the row its key names now.
 */
bool local_jurisdiction_claim_reads_current(const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    // The store checks a stated version itself. A claim of nothing still has
    // to state the version it replaces, so only that claim needs the row.
    return claim.kind == precondition_kind::any;
}

using local_jurisdiction_rows_by_key =
    std::map<std::tuple<std::string, std::string>, std::vector<domain::local_jurisdiction>>;

/**
 * @brief The rows a claim states: the caller's own, and none of the shared ones.
 *
 * The read admits the system tenant's rows beside the caller's. A claim is about
 * the row this write replaces, and that row is one the caller owns; taking the
 * system tenant's version would make the store refuse a write the caller is
 * entitled to make.
 */
std::vector<domain::local_jurisdiction>
local_jurisdiction_claim_rows(const ores::database::context& ctx,
                              std::vector<domain::local_jurisdiction> rows) {
    const auto tid = ctx.tenant_id();
    std::erase_if(rows, [&](const auto& row) { return row.tenant_id != tid; });
    return rows;
}

/**
 * @brief The current rows the objects name, by the text form of their key.
 *
 * One batch read for the set, so a batch checks its claims against one read
 * rather than one read per row.
 */
local_jurisdiction_rows_by_key
local_jurisdiction_current_rows(local_jurisdiction_repository& repo,
                                const ores::database::context& ctx,
                                const std::vector<domain::local_jurisdiction>& of) {
    std::vector<std::string> codes;
    codes.reserve(of.size());
    std::vector<std::string> coding_scheme_codes;
    coding_scheme_codes.reserve(of.size());
    for (const auto& row : of) {
        codes.push_back(row.code);
        coding_scheme_codes.push_back(row.coding_scheme_code);
    }
    local_jurisdiction_rows_by_key current;
    for (auto& item :
         local_jurisdiction_claim_rows(ctx, repo.read_latest(ctx, codes, coding_scheme_codes)))
        current[std::make_tuple(item.code, item.coding_scheme_code)].push_back(std::move(item));
    return current;
}

/**
 * @brief The current rows that the key of an object names, or none.
 */
const std::vector<domain::local_jurisdiction>&
local_jurisdiction_rows_of(const local_jurisdiction_rows_by_key& current,
                           const domain::local_jurisdiction& item) {
    static const std::vector<domain::local_jurisdiction> none;
    const auto found = current.find(std::make_tuple(item.code, item.coding_scheme_code));
    return found == current.end() ? none : found->second;
}

/**
 * @brief The claim a replace makes: the version the row carries now, or
 * that no row exists yet.
 */
ores::utility::domain::precondition
local_jurisdiction_replace_claim(const std::vector<domain::local_jurisdiction>& current) {
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
domain::local_jurisdiction
local_jurisdiction_apply_claim(const domain::local_jurisdiction& v,
                               const ores::utility::domain::precondition& claim,
                               const std::vector<domain::local_jurisdiction>& current) {
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

void local_jurisdiction_repository::write(const context& ctx, const domain::local_jurisdiction& v) {
    write(ctx,
          v,
          local_jurisdiction_replace_claim(
              local_jurisdiction_claim_rows(ctx, read_latest(ctx, v.code, v.coding_scheme_code))));
}

void local_jurisdiction_repository::write(const context& ctx,
                                          const std::vector<domain::local_jurisdiction>& v) {
    const auto current = local_jurisdiction_current_rows(*this, ctx, v);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(
            local_jurisdiction_replace_claim(local_jurisdiction_rows_of(current, item)));
    write(ctx, v, claims);
}

void local_jurisdiction_repository::write(const context& ctx,
                                          const domain::local_jurisdiction& v,
                                          const ores::utility::domain::precondition& claim) {
    ORES_LOG_SEV(lg(), debug) << "Writing local jurisdiction. " << "code: " << v.code
                              << " coding_scheme_code: " << v.coding_scheme_code;
    const auto t = local_jurisdiction_apply_claim(
        v,
        claim,
        local_jurisdiction_claim_reads_current(claim) ?
            local_jurisdiction_claim_rows(ctx, read_latest(ctx, v.code, v.coding_scheme_code)) :
            std::vector<domain::local_jurisdiction>{});
    query_builder::execute_write_query(
        ctx, local_jurisdiction_mapper::map(t), lg(), "Writing local jurisdiction to database.");
}

void local_jurisdiction_repository::write(
    const context& ctx,
    const std::vector<domain::local_jurisdiction>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    ORES_LOG_SEV(lg(), debug) << "Writing local jurisdictions. Count: " << v.size();
    if (claims.size() != v.size())
        throw std::invalid_argument(
            "local_jurisdiction_repository::write: one claim is needed for each row");
    std::vector<domain::local_jurisdiction> to_read;
    for (std::size_t i = 0; i < v.size(); ++i) {
        if (local_jurisdiction_claim_reads_current(claims[i]))
            to_read.push_back(v[i]);
    }
    const auto current = local_jurisdiction_current_rows(*this, ctx, to_read);
    std::vector<domain::local_jurisdiction> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(local_jurisdiction_apply_claim(
            v[i], claims[i], local_jurisdiction_rows_of(current, v[i])));
    query_builder::execute_write_query(ctx,
                                       local_jurisdiction_mapper::map(batch),
                                       lg(),
                                       "Writing local jurisdictions to database.");
}

std::vector<domain::local_jurisdiction>
local_jurisdiction_repository::read_latest(const context& ctx) {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query = query_builder::read<std::vector<local_jurisdiction_entity>> |
                       query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys) &&
                                            "valid_to"_c == max) |
                       query_builder::order_by("code"_c, "coding_scheme_code"_c);

    return query_builder::execute_read_query<local_jurisdiction_entity, domain::local_jurisdiction>(
        ctx,
        query,
        [](const auto& entities) { return local_jurisdiction_mapper::map(entities); },
        lg(),
        "Reading latest local jurisdictions");
}

std::vector<domain::local_jurisdiction> local_jurisdiction_repository::read_latest(
    const context& ctx, const std::string& code, const std::string& coding_scheme_code) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest local jurisdiction. " << "code: " << code
                              << " coding_scheme_code: " << coding_scheme_code;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query =
        query_builder::read<std::vector<local_jurisdiction_entity>> |
        query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys) && "code"_c == code &&
                             "coding_scheme_code"_c == coding_scheme_code && "valid_to"_c == max);

    return query_builder::execute_read_query<local_jurisdiction_entity, domain::local_jurisdiction>(
        ctx,
        query,
        [](const auto& entities) { return local_jurisdiction_mapper::map(entities); },
        lg(),
        "Reading latest local jurisdiction by code.");
}


std::vector<domain::local_jurisdiction> local_jurisdiction_repository::read_all(
    const context& ctx, const std::string& code, const std::string& coding_scheme_code) {
    ORES_LOG_SEV(lg(), debug) << "Reading all local jurisdiction versions. " << "code: " << code
                              << " coding_scheme_code: " << coding_scheme_code;
    const auto tid = ctx.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query =
        query_builder::read<std::vector<local_jurisdiction_entity>> |
        query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys) && "code"_c == code &&
                             "coding_scheme_code"_c == coding_scheme_code) |
        query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<local_jurisdiction_entity, domain::local_jurisdiction>(
        ctx,
        query,
        [](const auto& entities) { return local_jurisdiction_mapper::map(entities); },
        lg(),
        "Reading all local jurisdiction versions by code.");
}

std::optional<domain::local_jurisdiction>
local_jurisdiction_repository::read_at_version(const context& ctx,
                                               const std::string& code,
                                               const std::string& coding_scheme_code,
                                               std::uint32_t version) {
    ORES_LOG_SEV(lg(), debug) << "Reading local jurisdiction at version. " << "code: " << code
                              << " coding_scheme_code: " << coding_scheme_code
                              << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query = query_builder::read<std::vector<local_jurisdiction_entity>> |
                       query_builder::where(
                           ("tenant_id"_c == tid || "tenant_id"_c == sys) && "code"_c == code &&
                           "coding_scheme_code"_c == coding_scheme_code && "version"_c == version) |
                       query_builder::limit(1);

    const auto entities =
        query_builder::execute_read_query<local_jurisdiction_entity, domain::local_jurisdiction>(
            ctx,
            query,
            [](const auto& entities) { return local_jurisdiction_mapper::map(entities); },
            lg(),
            "Reading local jurisdiction at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

local_jurisdiction_repository::remove_status
local_jurisdiction_repository::remove(const context& ctx,
                                      const std::string& code,
                                      const std::string& coding_scheme_code,
                                      std::optional<std::uint32_t> version) {
    ORES_LOG_SEV(lg(), debug) << "Removing local jurisdiction. " << "code: " << code
                              << " coding_scheme_code: " << coding_scheme_code;
    const auto current = read_latest(ctx, code, coding_scheme_code);
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
    const auto query = query_builder::delete_from<local_jurisdiction_entity> |
                       query_builder::where("tenant_id"_c == tid && "code"_c == code &&
                                            "coding_scheme_code"_c == coding_scheme_code &&
                                            "valid_to"_c == max && "version"_c == expected);

    query_builder::execute_delete_query(
        ctx, query, lg(), "Removing local jurisdiction from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, code, coding_scheme_code).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void local_jurisdiction_repository::remove(const context& ctx,
                                           const std::string& code,
                                           const std::string& coding_scheme_code) {
    static_cast<void>(remove(ctx, code, coding_scheme_code, std::nullopt));
}

std::vector<domain::local_jurisdiction>
local_jurisdiction_repository::read_latest(const context& ctx,
                                           std::uint32_t offset,
                                           std::uint32_t limit,
                                           const ores::utility::domain::order& order,
                                           const std::optional<std::string>& as_of) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest local jurisdictions with offset: " << offset
                              << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query = query_builder::read<std::vector<local_jurisdiction_entity>> |
                       query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys)) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<local_jurisdiction_entity,
                                                     domain::local_jurisdiction>(
        ctx,
        query,
        local_jurisdiction_list_order(order, {"code", "coding_scheme_code"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of), std::nullopt),
        [](const auto& entities) { return local_jurisdiction_mapper::map(entities); },
        lg(),
        "Reading latest local jurisdictions with pagination.");
}

std::uint32_t local_jurisdiction_repository::get_total_jurisdiction_count(
    const context& ctx, const std::optional<std::string>& as_of) {
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active local jurisdiction count";

    const auto tid = ctx.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query = query_builder::read<std::vector<local_jurisdiction_entity>> |
                       query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys));

    return query_builder::execute_count_query<local_jurisdiction_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of), std::nullopt),
        lg(),
        "Counting local jurisdictions");
}

std::vector<domain::local_jurisdiction>
local_jurisdiction_repository::read_latest(const context& ctx,
                                           const std::vector<std::string>& codes,
                                           const std::vector<std::string>& coding_scheme_codes) {
    if (codes.empty() || coding_scheme_codes.empty())
        return {};
    if (coding_scheme_codes.size() != codes.size())
        throw std::invalid_argument("local_jurisdiction_repository::read_latest: key column "
                                    "vectors must be the same length");
    // Every key is a bind and the store's bind count is finite, so a larger
    // key set is read one chunk at a time.
    constexpr std::size_t chunk_size = 1000;
    if (codes.size() > chunk_size) {
        std::vector<domain::local_jurisdiction> all;
        for (std::size_t begin = 0; begin < codes.size(); begin += chunk_size) {
            const auto end = std::min(begin + chunk_size, codes.size());
            auto part =
                read_latest(ctx,
                            std::vector<std::string>(codes.begin() + begin, codes.begin() + end),
                            std::vector<std::string>(coding_scheme_codes.begin() + begin,
                                                     coding_scheme_codes.begin() + end));
            all.insert(all.end(),
                       std::make_move_iterator(part.begin()),
                       std::make_move_iterator(part.end()));
        }
        return all;
    }
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query =
        query_builder::read<std::vector<local_jurisdiction_entity>> |
        query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys) && "code"_c.in(codes) &&
                             "coding_scheme_code"_c.in(coding_scheme_codes) && "valid_to"_c == max);
    auto result =
        query_builder::execute_read_query<local_jurisdiction_entity, domain::local_jurisdiction>(
            ctx,
            query,
            [](const auto& entities) { return local_jurisdiction_mapper::map(entities); },
            lg(),
            "Reading latest local jurisdictions by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (the query builder has no tuple IN), so filter down to the
    // exact requested key-tuples here.
    std::set<std::tuple<std::string, std::string>> requested;
    for (std::size_t i = 0; i < codes.size(); ++i)
        requested.emplace(codes[i], coding_scheme_codes[i]);
    std::vector<domain::local_jurisdiction> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(std::make_tuple(item.code, item.coding_scheme_code)))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void local_jurisdiction_repository::remove(const context& ctx,
                                           const std::vector<std::string>& codes,
                                           const std::vector<std::string>& coding_scheme_codes) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (coding_scheme_codes.size() != codes.size())
        throw std::invalid_argument(
            "local_jurisdiction_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < codes.size(); ++i)
        remove(ctx, codes[i], coding_scheme_codes[i]);
}


}
