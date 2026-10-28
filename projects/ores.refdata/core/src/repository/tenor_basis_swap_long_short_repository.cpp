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
#include "ores.refdata.core/repository/tenor_basis_swap_long_short_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_long_short.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_long_short_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/messaging/tenor_basis_swap_long_short_protocol.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_long_short_entity.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_long_short_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::refdata::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string tenor_basis_swap_long_short_repository::sql() {
    return query_builder::insert_statement_sql<tenor_basis_swap_long_short_entity>(lg());
}

bool tenor_basis_swap_long_short_repository::is_sortable(std::string_view field) {
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
tenor_basis_swap_long_short_list_order(const ores::utility::domain::order& order,
                                       std::initializer_list<std::string> default_columns,
                                       bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"id"});
    if (!tenor_basis_swap_long_short_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of tenor basis swap long shorts cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition> tenor_basis_swap_long_short_filter_condition(
    const std::optional<messaging::tenor_basis_swap_long_shorts_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->convention_id)
        r.push_back(query_builder::equals("convention_id",
                                          query_builder::filter_value(*filter->convention_id)));
    if (filter->id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("id", std::move(values)));
    }
    if (filter->convention_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->convention_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("convention_id", std::move(values)));
    }
    return query_builder::all_of(std::move(r));
}

}

ores::utility::domain::precondition tenor_basis_swap_long_short_repository::replace_claim(
    context ctx, const domain::tenor_basis_swap_long_short& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.identity.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().identity.version)};
}

domain::tenor_basis_swap_long_short tenor_basis_swap_long_short_repository::apply_claim(
    context ctx,
    const domain::tenor_basis_swap_long_short& v,
    const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    switch (claim.kind) {
        case precondition_kind::must_not_exist:
            // Zero states that no current row exists, which is the one meaning the
            // store gives a zero version.
            t.identity.version = 0;
            break;
        case precondition_kind::must_match_version:
            t.identity.version = claim.version ? static_cast<int>(*claim.version) : 0;
            break;
        case precondition_kind::any: {
            // A caller that claims nothing still has to say what it replaces, so
            // the row is read and its version stated. A row that moved on between
            // this read and the write is a conflict the trigger raises, never a
            // silent overwrite.
            const auto current = read_latest(ctx, boost::uuids::to_string(v.identity.id));
            t.identity.version = current.empty() ? 0 : current.front().identity.version;
            break;
        }
    }
    return t;
}

void tenor_basis_swap_long_short_repository::write(context ctx,
                                                   const domain::tenor_basis_swap_long_short& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void tenor_basis_swap_long_short_repository::write(
    context ctx, const std::vector<domain::tenor_basis_swap_long_short>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void tenor_basis_swap_long_short_repository::write(
    context ctx,
    const domain::tenor_basis_swap_long_short& v,
    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenor basis swap long short. "
                               << "id: " << v.identity.id;
    const auto t = apply_claim(ctx, v, claim);
    query_builder::execute_write_query(ctx,
                                       tenor_basis_swap_long_short_mapper::map(t),
                                       lg(),
                                       "Writing tenor basis swap long short to database.");
}

void tenor_basis_swap_long_short_repository::write(
    context ctx,
    const std::vector<domain::tenor_basis_swap_long_short>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenor basis swap long shorts. Count: " << v.size();
    std::vector<domain::tenor_basis_swap_long_short> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    query_builder::execute_write_query(ctx,
                                       tenor_basis_swap_long_short_mapper::map(batch),
                                       lg(),
                                       "Writing tenor basis swap long shorts to database.");
}

std::vector<domain::tenor_basis_swap_long_short>
tenor_basis_swap_long_short_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<tenor_basis_swap_long_short_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       query_builder::order_by("id"_c);

    return query_builder::execute_read_query<tenor_basis_swap_long_short_entity,
                                             domain::tenor_basis_swap_long_short>(
        ctx,
        query,
        [](const auto& entities) { return tenor_basis_swap_long_short_mapper::map(entities); },
        lg(),
        "Reading latest tenor basis swap long shorts");
}

std::vector<domain::tenor_basis_swap_long_short>
tenor_basis_swap_long_short_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor basis swap long short. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<tenor_basis_swap_long_short_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    auto found = query_builder::execute_read_query<tenor_basis_swap_long_short_entity,
                                                   domain::tenor_basis_swap_long_short>(
        ctx,
        query,
        [](const auto& entities) { return tenor_basis_swap_long_short_mapper::map(entities); },
        lg(),
        "Reading latest tenor basis swap long short by id.");
    // The store keys the row by its party too, and a session sees the rows of
    // every party below its own, so one key can name several rows. The acting
    // party's row is the one the caller means when it holds one; otherwise
    // every row stays, so another party's row can still be found by its key,
    // ordered by party so the first row is the same on every read.
    const auto party = ctx.party_id();
    const auto acting = [&](const auto& row) {
        return row.identity.party_id == *party;
    };
    if (party.has_value() && std::ranges::any_of(found, acting))
        std::erase_if(found, [&](const auto& row) { return !acting(row); });
    else
        std::sort(found.begin(), found.end(), [](const auto& a, const auto& b) {
            return a.identity.party_id < b.identity.party_id;
        });
    return found;
}


std::vector<domain::tenor_basis_swap_long_short>
tenor_basis_swap_long_short_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all tenor basis swap long short versions. "
                               << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<tenor_basis_swap_long_short_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id) |
                       query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<tenor_basis_swap_long_short_entity,
                                             domain::tenor_basis_swap_long_short>(
        ctx,
        query,
        [](const auto& entities) { return tenor_basis_swap_long_short_mapper::map(entities); },
        lg(),
        "Reading all tenor basis swap long short versions by id.");
}

std::optional<domain::tenor_basis_swap_long_short>
tenor_basis_swap_long_short_repository::read_at_version(context ctx,
                                                        const std::string& id,
                                                        std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading tenor basis swap long short at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<tenor_basis_swap_long_short_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
        query_builder::limit(1);

    const auto entities = query_builder::execute_read_query<tenor_basis_swap_long_short_entity,
                                                            domain::tenor_basis_swap_long_short>(
        ctx,
        query,
        [](const auto& entities) { return tenor_basis_swap_long_short_mapper::map(entities); },
        lg(),
        "Reading tenor basis swap long short at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::tenor_basis_swap_long_short>
tenor_basis_swap_long_short_repository::read_latest_by_convention_id(
    context ctx,
    const std::string& convention_id,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::tenor_basis_swap_long_shorts_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor basis swap long shorts. convention_id: "
                               << convention_id << " offset: " << offset << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<tenor_basis_swap_long_short_entity>> |
        query_builder::where("tenant_id"_c == tid && "convention_id"_c == convention_id &&
                             "valid_to"_c == max.value()) |
        query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<tenor_basis_swap_long_short_entity,
                                                     domain::tenor_basis_swap_long_short>(
        ctx,
        query,
        tenor_basis_swap_long_short_list_order(order, {"id"}, false),
        tenor_basis_swap_long_short_filter_condition(filter),
        [](const auto& entities) { return tenor_basis_swap_long_short_mapper::map(entities); },
        lg(),
        "Reading latest tenor basis swap long shorts by convention_id.");
}

std::uint32_t tenor_basis_swap_long_short_repository::
    get_total_tenor_basis_swap_long_short_count_by_convention_id(
        context ctx,
        const std::string& convention_id,
        const std::optional<messaging::tenor_basis_swap_long_shorts_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active tenor basis swap long shorts count. convention_id: "
        << convention_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<tenor_basis_swap_long_short_entity>> |
        query_builder::where("tenant_id"_c == tid && "convention_id"_c == convention_id &&
                             "valid_to"_c == max.value());

    return query_builder::execute_count_query<tenor_basis_swap_long_short_entity>(
        ctx,
        query,
        tenor_basis_swap_long_short_filter_condition(filter),
        lg(),
        "Counting tenor basis swap long shorts by convention_id");
}


std::vector<domain::tenor_basis_swap_long_short>
tenor_basis_swap_long_short_repository::read_by_convention_id_as_of(
    context ctx,
    const std::string& convention_id,
    std::chrono::system_clock::time_point valid_from_bound,
    std::chrono::system_clock::time_point valid_to_bound) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading tenor basis swap long shorts as of window. convention_id: " << convention_id;

    const auto vf(
        make_timestamp(ores::platform::time::datetime::to_db_string(valid_from_bound), lg()));
    const auto vt(
        make_timestamp(ores::platform::time::datetime::to_db_string(valid_to_bound), lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<tenor_basis_swap_long_short_entity>> |
        query_builder::where("tenant_id"_c == tid && "convention_id"_c == convention_id &&
                             "valid_from"_c < vt.value() && "valid_to"_c > vf.value()) |
        query_builder::order_by("id"_c);

    return query_builder::execute_read_query<tenor_basis_swap_long_short_entity,
                                             domain::tenor_basis_swap_long_short>(
        ctx,
        query,
        [](const auto& entities) { return tenor_basis_swap_long_short_mapper::map(entities); },
        lg(),
        "Reading tenor basis swap long shorts as of window by convention_id.");
}

tenor_basis_swap_long_short_repository::remove_status
tenor_basis_swap_long_short_repository::remove(context ctx,
                                               const std::string& id,
                                               std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing tenor basis swap long short. " << "id: " << id;
    const auto current = read_latest(ctx, id);
    if (current.empty())
        return remove_status::missing;
    // The protocol states the version as a uint32 and the row carries it as an
    // int, so the comparison states the conversion rather than relying on one.
    if (version && static_cast<std::uint32_t>(current.front().identity.version) != *version)
        return remove_status::conflicting;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    // The row is named by its version as well as by its key, so the removal
    // cannot close a row that replaced the one the caller read between the
    // read above and this statement.
    const auto expected = version ? static_cast<int>(*version) : current.front().identity.version;
    // The row checked above is one party's row of the key, and the removal
    // closes that row only, not every visible party's row of the same key.
    const auto party = boost::uuids::to_string(current.front().identity.party_id);
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<tenor_basis_swap_long_short_entity> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "party_id"_c == party &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    query_builder::execute_delete_query(
        ctx, query, lg(), "Removing tenor basis swap long short from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    // Another party's row of the key stays open by design, so only a row of
    // the party removed counts.
    const auto removed_party = current.front().identity.party_id;
    if (std::ranges::any_of(read_latest(ctx, id), [&](const auto& row) {
            return row.identity.party_id == removed_party;
        }))
        return remove_status::conflicting;
    return remove_status::removed;
}

void tenor_basis_swap_long_short_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::tenor_basis_swap_long_short>
tenor_basis_swap_long_short_repository::read_latest(
    context ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::tenor_basis_swap_long_shorts_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor basis swap long shorts with offset: "
                               << offset << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<tenor_basis_swap_long_short_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<tenor_basis_swap_long_short_entity,
                                                     domain::tenor_basis_swap_long_short>(
        ctx,
        query,
        tenor_basis_swap_long_short_list_order(order, {"id"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of),
                                tenor_basis_swap_long_short_filter_condition(filter)),
        [](const auto& entities) { return tenor_basis_swap_long_short_mapper::map(entities); },
        lg(),
        "Reading latest tenor basis swap long shorts with pagination.");
}

std::uint32_t tenor_basis_swap_long_short_repository::get_total_tenor_basis_swap_long_short_count(
    context ctx,
    const std::optional<messaging::tenor_basis_swap_long_shorts_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active tenor basis swap long short count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<tenor_basis_swap_long_short_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<tenor_basis_swap_long_short_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of),
                                tenor_basis_swap_long_short_filter_condition(filter)),
        lg(),
        "Counting tenor basis swap long shorts");
}

std::vector<domain::tenor_basis_swap_long_short>
tenor_basis_swap_long_short_repository::read_latest(context ctx,
                                                    const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<tenor_basis_swap_long_short_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    auto result = query_builder::execute_read_query<tenor_basis_swap_long_short_entity,
                                                    domain::tenor_basis_swap_long_short>(
        ctx,
        query,
        [](const auto& entities) { return tenor_basis_swap_long_short_mapper::map(entities); },
        lg(),
        "Reading latest tenor basis swap long shorts by ids.");
    return result;
}

void tenor_basis_swap_long_short_repository::remove(context ctx,
                                                    const std::vector<std::string>& ids) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (ids.empty())
        return;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<tenor_basis_swap_long_short_entity> |
        query_builder::where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    query_builder::execute_delete_query(
        ctx, query, lg(), "Batch removing tenor basis swap long shorts.");
}


}
