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
#include "ores.refdata.core/repository/country_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/country.hpp"
#include "ores.refdata.api/domain/country_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/messaging/country_protocol.hpp"
#include "ores.refdata.core/repository/country_entity.hpp"
#include "ores.refdata.core/repository/country_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
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

std::string country_repository::sql() {
    return query_builder::insert_statement_sql<country_entity>(lg());
}

bool country_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order country_list_order(const ores::utility::domain::order& order,
                                        std::initializer_list<std::string> default_columns,
                                        bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"alpha2_code"});
    if (!country_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of countries cannot be ordered by " + order.field +
                                    ".");
    return query_builder::make_order({order.field}, order.descending, {"alpha2_code"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition>
country_filter_condition(const std::optional<messaging::countries_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->alpha2_code_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->alpha2_code_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("alpha2_code", std::move(values)));
    }
    return query_builder::all_of(std::move(r));
}

}

namespace {

/**
 * @brief Whether a claim is checked against the row its key names now.
 */
bool country_claim_reads_current(const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    // The store checks a stated version itself. A claim of nothing still has
    // to state the version it replaces, so only that claim needs the row.
    return claim.kind == precondition_kind::any;
}

using country_rows_by_key = std::map<std::tuple<std::string>, std::vector<domain::country>>;

/**
 * @brief The rows a claim states: the caller's own, and none of the shared ones.
 *
 * The read admits the system tenant's rows beside the caller's. A claim is about
 * the row this write replaces, and that row is one the caller owns; taking the
 * system tenant's version would make the store refuse a write the caller is
 * entitled to make.
 */
std::vector<domain::country> country_claim_rows(const ores::database::context& ctx,
                                                std::vector<domain::country> rows) {
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
country_rows_by_key country_current_rows(country_repository& repo,
                                         const ores::database::context& ctx,
                                         const std::vector<domain::country>& of) {
    std::vector<std::string> alpha2_codes;
    alpha2_codes.reserve(of.size());
    for (const auto& row : of) {
        alpha2_codes.push_back(row.alpha2_code);
    }
    country_rows_by_key current;
    for (auto& item : country_claim_rows(ctx, repo.read_latest(ctx, alpha2_codes)))
        current[std::make_tuple(item.alpha2_code)].push_back(std::move(item));
    return current;
}

/**
 * @brief The current rows that the key of an object names, or none.
 */
const std::vector<domain::country>& country_rows_of(const country_rows_by_key& current,
                                                    const domain::country& item) {
    static const std::vector<domain::country> none;
    const auto found = current.find(std::make_tuple(item.alpha2_code));
    return found == current.end() ? none : found->second;
}

/**
 * @brief The claim a replace makes: the version the row carries now, or
 * that no row exists yet.
 */
ores::utility::domain::precondition
country_replace_claim(const std::vector<domain::country>& current) {
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
domain::country country_apply_claim(const domain::country& v,
                                    const ores::utility::domain::precondition& claim,
                                    const std::vector<domain::country>& current) {
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

void country_repository::write(const context& ctx, const domain::country& v) {
    write(ctx, v, country_replace_claim(country_claim_rows(ctx, read_latest(ctx, v.alpha2_code))));
}

void country_repository::write(const context& ctx, const std::vector<domain::country>& v) {
    const auto current = country_current_rows(*this, ctx, v);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(country_replace_claim(country_rows_of(current, item)));
    write(ctx, v, claims);
}

void country_repository::write(const context& ctx,
                               const domain::country& v,
                               const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing country. " << "alpha2_code: " << v.alpha2_code;
    const auto t =
        country_apply_claim(v,
                            claim,
                            country_claim_reads_current(claim) ?
                                country_claim_rows(ctx, read_latest(ctx, v.alpha2_code)) :
                                std::vector<domain::country>{});
    query_builder::execute_write_query(
        ctx, country_mapper::map(t), lg(), "Writing country to database.");
}

void country_repository::write(const context& ctx,
                               const std::vector<domain::country>& v,
                               const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing countries. Count: " << v.size();
    if (claims.size() != v.size())
        throw std::invalid_argument("country_repository::write: one claim is needed for each row");
    std::vector<domain::country> to_read;
    for (std::size_t i = 0; i < v.size(); ++i) {
        if (country_claim_reads_current(claims[i]))
            to_read.push_back(v[i]);
    }
    const auto current = country_current_rows(*this, ctx, to_read);
    std::vector<domain::country> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(country_apply_claim(v[i], claims[i], country_rows_of(current, v[i])));
    query_builder::execute_write_query(
        ctx, country_mapper::map(batch), lg(), "Writing countries to database.");
}

std::vector<domain::country> country_repository::read_latest(const context& ctx) {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query = query_builder::read<std::vector<country_entity>> |
                       query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys) &&
                                            "valid_to"_c == max) |
                       query_builder::order_by("alpha2_code"_c);

    return query_builder::execute_read_query<country_entity, domain::country>(
        ctx,
        query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(),
        "Reading latest countries");
}

std::vector<domain::country> country_repository::read_latest(const context& ctx,
                                                             const std::string& alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest country. " << "alpha2_code: " << alpha2_code;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query = query_builder::read<std::vector<country_entity>> |
                       query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys) &&
                                            "alpha2_code"_c == alpha2_code && "valid_to"_c == max);

    return query_builder::execute_read_query<country_entity, domain::country>(
        ctx,
        query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(),
        "Reading latest country by alpha2_code.");
}


std::vector<domain::country> country_repository::read_all(const context& ctx,
                                                          const std::string& alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all country versions. "
                               << "alpha2_code: " << alpha2_code;
    const auto tid = ctx.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query = query_builder::read<std::vector<country_entity>> |
                       query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys) &&
                                            "alpha2_code"_c == alpha2_code) |
                       query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<country_entity, domain::country>(
        ctx,
        query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(),
        "Reading all country versions by alpha2_code.");
}

std::optional<domain::country> country_repository::read_at_version(const context& ctx,
                                                                   const std::string& alpha2_code,
                                                                   std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading country at version. " << "alpha2_code: " << alpha2_code
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query =
        query_builder::read<std::vector<country_entity>> |
        query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys) &&
                             "alpha2_code"_c == alpha2_code && "version"_c == version) |
        query_builder::limit(1);

    const auto entities = query_builder::execute_read_query<country_entity, domain::country>(
        ctx,
        query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(),
        "Reading country at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

country_repository::remove_status country_repository::remove(const context& ctx,
                                                             const std::string& alpha2_code,
                                                             std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing country. " << "alpha2_code: " << alpha2_code;
    const auto current = read_latest(ctx, alpha2_code);
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
        query_builder::delete_from<country_entity> |
        query_builder::where("tenant_id"_c == tid && "alpha2_code"_c == alpha2_code &&
                             "valid_to"_c == max && "version"_c == expected);

    query_builder::execute_delete_query(ctx, query, lg(), "Removing country from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, alpha2_code).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void country_repository::remove(const context& ctx, const std::string& alpha2_code) {
    static_cast<void>(remove(ctx, alpha2_code, std::nullopt));
}

std::vector<domain::country>
country_repository::read_latest(const context& ctx,
                                std::uint32_t offset,
                                std::uint32_t limit,
                                const ores::utility::domain::order& order,
                                const std::optional<messaging::countries_filter>& filter,
                                const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest countries with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query = query_builder::read<std::vector<country_entity>> |
                       query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys)) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<country_entity, domain::country>(
        ctx,
        query,
        country_list_order(order, {"alpha2_code"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of), country_filter_condition(filter)),
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(),
        "Reading latest countries with pagination.");
}

std::uint32_t country_repository::get_total_country_count(
    const context& ctx,
    const std::optional<messaging::countries_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active country count";

    const auto tid = ctx.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query = query_builder::read<std::vector<country_entity>> |
                       query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys));

    return query_builder::execute_count_query<country_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of), country_filter_condition(filter)),
        lg(),
        "Counting countries");
}

std::vector<domain::country>
country_repository::read_latest(const context& ctx, const std::vector<std::string>& alpha2_codes) {
    if (alpha2_codes.empty())
        return {};
    // Every key is a bind and the store's bind count is finite, so a larger
    // key set is read one chunk at a time.
    constexpr std::size_t chunk_size = 1000;
    if (alpha2_codes.size() > chunk_size) {
        std::vector<domain::country> all;
        for (std::size_t begin = 0; begin < alpha2_codes.size(); begin += chunk_size) {
            const auto end = std::min(begin + chunk_size, alpha2_codes.size());
            auto part = read_latest(
                ctx,
                std::vector<std::string>(alpha2_codes.begin() + begin, alpha2_codes.begin() + end));
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
        query_builder::read<std::vector<country_entity>> |
        query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys) &&
                             "alpha2_code"_c.in(alpha2_codes) && "valid_to"_c == max);
    auto result = query_builder::execute_read_query<country_entity, domain::country>(
        ctx,
        query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(),
        "Reading latest countries by ids.");
    return result;
}

void country_repository::remove(const context& ctx, const std::vector<std::string>& alpha2_codes) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (alpha2_codes.empty())
        return;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<country_entity> |
        query_builder::where("tenant_id"_c == tid && "alpha2_code"_c.in(alpha2_codes) &&
                             "valid_to"_c == max);
    query_builder::execute_delete_query(ctx, query, lg(), "Batch removing countries.");
}


std::vector<domain::country> country_repository::read_all(const context& ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all country versions.";
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<country_entity>> |
                       query_builder::where("tenant_id"_c == tid) |
                       query_builder::order_by("valid_from"_c.desc());

    return query_builder::execute_read_query<country_entity, domain::country>(
        ctx,
        query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(),
        "Reading all countries.");
}

}
