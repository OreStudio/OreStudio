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
#include "ores.trading.core/repository/script_result_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/script_result.hpp"
#include "ores.trading.api/domain/script_result_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/script_result_entity.hpp"
#include "ores.trading.core/repository/script_result_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
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

std::string script_result_repository::sql() {
    return query_builder::insert_statement_sql<script_result_entity>(lg());
}

bool script_result_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order script_result_list_order(const ores::utility::domain::order& order,
                                              std::initializer_list<std::string> default_columns,
                                              bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(default_columns,
                                         default_descending != order.descending,
                                         {"script_name", "purpose", "sequence_number"});
    if (!script_result_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of script results cannot be ordered by " + order.field +
                                    ".");
    return query_builder::make_order(
        {order.field}, order.descending, {"script_name", "purpose", "sequence_number"});
}

}

namespace {

/**
 * @brief Whether a claim is checked against the row its key names now.
 */
bool script_result_claim_reads_current(const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    // The store checks a stated version itself. A claim of nothing still has
    // to state the version it replaces, so only that claim needs the row.
    return claim.kind == precondition_kind::any;
}

using script_result_rows_by_key =
    std::map<std::tuple<std::string, std::string, std::string>, std::vector<domain::script_result>>;

/**
 * @brief The current rows the objects name, by the text form of their key.
 *
 * One batch read for the set, so a batch checks its claims against one read
 * rather than one read per row.
 */
script_result_rows_by_key script_result_current_rows(script_result_repository& repo,
                                                     const ores::database::context& ctx,
                                                     const std::vector<domain::script_result>& of) {
    std::vector<std::string> script_names;
    script_names.reserve(of.size());
    std::vector<std::string> purposes;
    purposes.reserve(of.size());
    std::vector<std::string> sequence_numbers;
    sequence_numbers.reserve(of.size());
    for (const auto& row : of) {
        script_names.push_back(row.script_name);
        purposes.push_back(row.purpose);
        sequence_numbers.push_back(std::to_string(row.sequence_number));
    }
    script_result_rows_by_key current;
    for (auto& item : repo.read_latest(ctx, script_names, purposes, sequence_numbers))
        current[std::make_tuple(
                    item.script_name, item.purpose, std::to_string(item.sequence_number))]
            .push_back(std::move(item));
    return current;
}

/**
 * @brief The current rows that the key of an object names, or none.
 */
const std::vector<domain::script_result>&
script_result_rows_of(const script_result_rows_by_key& current, const domain::script_result& item) {
    static const std::vector<domain::script_result> none;
    const auto found = current.find(
        std::make_tuple(item.script_name, item.purpose, std::to_string(item.sequence_number)));
    return found == current.end() ? none : found->second;
}

/**
 * @brief The claim a replace makes: the version the row carries now, or
 * that no row exists yet.
 */
ores::utility::domain::precondition
script_result_replace_claim(const std::vector<domain::script_result>& current) {
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
domain::script_result script_result_apply_claim(const domain::script_result& v,
                                                const ores::utility::domain::precondition& claim,
                                                const std::vector<domain::script_result>& current) {
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

void script_result_repository::write(const context& ctx, const domain::script_result& v) {
    write(ctx,
          v,
          script_result_replace_claim(
              read_latest(ctx, v.script_name, v.purpose, std::to_string(v.sequence_number))));
}

void script_result_repository::write(const context& ctx,
                                     const std::vector<domain::script_result>& v) {
    const auto current = script_result_current_rows(*this, ctx, v);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(script_result_replace_claim(script_result_rows_of(current, item)));
    write(ctx, v, claims);
}

void script_result_repository::write(const context& ctx,
                                     const domain::script_result& v,
                                     const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing script result. " << "script_name: " << v.script_name
                               << " purpose: " << v.purpose
                               << " sequence_number: " << v.sequence_number;
    const auto t = script_result_apply_claim(
        v,
        claim,
        script_result_claim_reads_current(claim) ?
            read_latest(ctx, v.script_name, v.purpose, std::to_string(v.sequence_number)) :
            std::vector<domain::script_result>{});
    query_builder::execute_write_query(
        ctx, script_result_mapper::map(t), lg(), "Writing script result to database.");
}

void script_result_repository::write(
    const context& ctx,
    const std::vector<domain::script_result>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing script results. Count: " << v.size();
    if (claims.size() != v.size())
        throw std::invalid_argument(
            "script_result_repository::write: one claim is needed for each row");
    std::vector<domain::script_result> to_read;
    for (std::size_t i = 0; i < v.size(); ++i) {
        if (script_result_claim_reads_current(claims[i]))
            to_read.push_back(v[i]);
    }
    const auto current = script_result_current_rows(*this, ctx, to_read);
    std::vector<domain::script_result> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(
            script_result_apply_claim(v[i], claims[i], script_result_rows_of(current, v[i])));
    query_builder::execute_write_query(
        ctx, script_result_mapper::map(batch), lg(), "Writing script results to database.");
}

std::vector<domain::script_result> script_result_repository::read_latest(const context& ctx) {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<script_result_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("script_name"_c, "purpose"_c, "sequence_number"_c);

    return query_builder::execute_read_query<script_result_entity, domain::script_result>(
        ctx,
        query,
        [](const auto& entities) { return script_result_mapper::map(entities); },
        lg(),
        "Reading latest script results");
}

std::vector<domain::script_result>
script_result_repository::read_latest(const context& ctx,
                                      const std::string& script_name,
                                      const std::string& purpose,
                                      const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest script result. " << "script_name: " << script_name
                               << " purpose: " << purpose
                               << " sequence_number: " << sequence_number;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<script_result_entity>> |
        query_builder::where("tenant_id"_c == tid && "script_name"_c == script_name &&
                             "purpose"_c == purpose && "sequence_number"_c == sequence_number &&
                             "valid_to"_c == max);

    return query_builder::execute_read_query<script_result_entity, domain::script_result>(
        ctx,
        query,
        [](const auto& entities) { return script_result_mapper::map(entities); },
        lg(),
        "Reading latest script result by script_name.");
}


std::vector<domain::script_result>
script_result_repository::read_all(const context& ctx,
                                   const std::string& script_name,
                                   const std::string& purpose,
                                   const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all script result versions. "
                               << "script_name: " << script_name << " purpose: " << purpose
                               << " sequence_number: " << sequence_number;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<script_result_entity>> |
        query_builder::where("tenant_id"_c == tid && "script_name"_c == script_name &&
                             "purpose"_c == purpose && "sequence_number"_c == sequence_number) |
        query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<script_result_entity, domain::script_result>(
        ctx,
        query,
        [](const auto& entities) { return script_result_mapper::map(entities); },
        lg(),
        "Reading all script result versions by script_name.");
}

std::optional<domain::script_result>
script_result_repository::read_at_version(const context& ctx,
                                          const std::string& script_name,
                                          const std::string& purpose,
                                          const std::string& sequence_number,
                                          std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading script result at version. "
                               << "script_name: " << script_name << " purpose: " << purpose
                               << " sequence_number: " << sequence_number
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<script_result_entity>> |
        query_builder::where("tenant_id"_c == tid && "script_name"_c == script_name &&
                             "purpose"_c == purpose && "sequence_number"_c == sequence_number &&
                             "version"_c == version) |
        query_builder::limit(1);

    const auto entities =
        query_builder::execute_read_query<script_result_entity, domain::script_result>(
            ctx,
            query,
            [](const auto& entities) { return script_result_mapper::map(entities); },
            lg(),
            "Reading script result at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}


script_result_repository::remove_status
script_result_repository::remove(const context& ctx,
                                 const std::string& script_name,
                                 const std::string& purpose,
                                 const std::string& sequence_number,
                                 std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing script result. " << "script_name: " << script_name
                               << " purpose: " << purpose
                               << " sequence_number: " << sequence_number;
    const auto current = read_latest(ctx, script_name, purpose, sequence_number);
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
    const auto query =
        query_builder::delete_from<script_result_entity> |
        query_builder::where("tenant_id"_c == tid && "script_name"_c == script_name &&
                             "purpose"_c == purpose && "sequence_number"_c == sequence_number &&
                             "valid_to"_c == max && "version"_c == expected);

    query_builder::execute_delete_query(ctx, query, lg(), "Removing script result from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, script_name, purpose, sequence_number).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void script_result_repository::remove(const context& ctx,
                                      const std::string& script_name,
                                      const std::string& purpose,
                                      const std::string& sequence_number) {
    static_cast<void>(remove(ctx, script_name, purpose, sequence_number, std::nullopt));
}

std::vector<domain::script_result>
script_result_repository::read_latest(const context& ctx,
                                      std::uint32_t offset,
                                      std::uint32_t limit,
                                      const ores::utility::domain::order& order,
                                      const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest script results with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<script_result_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<script_result_entity, domain::script_result>(
        ctx,
        query,
        script_result_list_order(order, {"script_name", "purpose", "sequence_number"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of), std::nullopt),
        [](const auto& entities) { return script_result_mapper::map(entities); },
        lg(),
        "Reading latest script results with pagination.");
}

std::uint32_t
script_result_repository::get_total_result_count(const context& ctx,
                                                 const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active script result count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<script_result_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<script_result_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of), std::nullopt),
        lg(),
        "Counting script results");
}

std::vector<domain::script_result>
script_result_repository::read_latest(const context& ctx,
                                      const std::vector<std::string>& script_names,
                                      const std::vector<std::string>& purposes,
                                      const std::vector<std::string>& sequence_numbers) {
    if (script_names.empty() || purposes.empty() || sequence_numbers.empty())
        return {};
    if (purposes.size() != script_names.size() || sequence_numbers.size() != script_names.size())
        throw std::invalid_argument(
            "script_result_repository::read_latest: key column vectors must be the same length");
    // Every key is a bind and the store's bind count is finite, so a larger
    // key set is read one chunk at a time.
    constexpr std::size_t chunk_size = 1000;
    if (script_names.size() > chunk_size) {
        std::vector<domain::script_result> all;
        for (std::size_t begin = 0; begin < script_names.size(); begin += chunk_size) {
            const auto end = std::min(begin + chunk_size, script_names.size());
            auto part = read_latest(
                ctx,
                std::vector<std::string>(script_names.begin() + begin, script_names.begin() + end),
                std::vector<std::string>(purposes.begin() + begin, purposes.begin() + end),
                std::vector<std::string>(sequence_numbers.begin() + begin,
                                         sequence_numbers.begin() + end));
            all.insert(all.end(),
                       std::make_move_iterator(part.begin()),
                       std::make_move_iterator(part.end()));
        }
        return all;
    }
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<script_result_entity>> |
        query_builder::where("tenant_id"_c == tid && "script_name"_c.in(script_names) &&
                             "purpose"_c.in(purposes) && "sequence_number"_c.in(sequence_numbers) &&
                             "valid_to"_c == max);
    auto result = query_builder::execute_read_query<script_result_entity, domain::script_result>(
        ctx,
        query,
        [](const auto& entities) { return script_result_mapper::map(entities); },
        lg(),
        "Reading latest script results by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (the query builder has no tuple IN), so filter down to the
    // exact requested key-tuples here.
    std::set<std::tuple<std::string, std::string, std::string>> requested;
    for (std::size_t i = 0; i < script_names.size(); ++i)
        requested.emplace(script_names[i], purposes[i], sequence_numbers[i]);
    std::vector<domain::script_result> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(std::make_tuple(
                item.script_name, item.purpose, std::to_string(item.sequence_number))))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void script_result_repository::remove(const context& ctx,
                                      const std::vector<std::string>& script_names,
                                      const std::vector<std::string>& purposes,
                                      const std::vector<std::string>& sequence_numbers) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (purposes.size() != script_names.size() || sequence_numbers.size() != script_names.size())
        throw std::invalid_argument(
            "script_result_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < script_names.size(); ++i)
        remove(ctx, script_names[i], purposes[i], sequence_numbers[i]);
}


}
