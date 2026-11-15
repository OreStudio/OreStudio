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
#include "ores.marketdata.core/repository/demand_interval_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/demand_interval.hpp"
#include "ores.marketdata.api/domain/demand_interval_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.api/messaging/demand_interval_protocol.hpp"
#include "ores.marketdata.core/repository/demand_interval_entity.hpp"
#include "ores.marketdata.core/repository/demand_interval_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
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

namespace ores::marketdata::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string demand_interval_repository::sql() {
    return query_builder::insert_statement_sql<demand_interval_entity>(lg());
}

bool demand_interval_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order demand_interval_list_order(const ores::utility::domain::order& order,
                                                std::initializer_list<std::string> default_columns,
                                                bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"id"});
    if (!demand_interval_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of demand intervals cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition>
demand_interval_filter_condition(const std::optional<messaging::demand_intervals_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->party_id)
        r.push_back(
            query_builder::equals("party_id", query_builder::filter_value(*filter->party_id)));
    if (filter->provider_code)
        r.push_back(query_builder::equals("provider_code",
                                          query_builder::filter_value(*filter->provider_code)));
    if (filter->feed_id)
        r.push_back(
            query_builder::equals("feed_id", query_builder::filter_value(*filter->feed_id)));
    if (filter->id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("id", std::move(values)));
    }
    if (filter->party_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->party_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("party_id", std::move(values)));
    }
    if (filter->provider_code_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->provider_code_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("provider_code", std::move(values)));
    }
    if (filter->feed_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->feed_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("feed_id", std::move(values)));
    }
    return query_builder::all_of(std::move(r));
}

}

namespace {

/**
 * @brief Whether a claim is checked against the row its key names now.
 */
bool demand_interval_claim_reads_current(const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    // The store checks a stated version itself. A claim of nothing still has
    // to state the version it replaces, so only that claim needs the row.
    return claim.kind == precondition_kind::any;
}

using demand_interval_rows_by_key =
    std::map<std::tuple<std::string>, std::vector<domain::demand_interval>>;

/**
 * @brief The current rows the objects name, by the text form of their key.
 *
 * One batch read for the set, so a batch checks its claims against one read
 * rather than one read per row.
 */
demand_interval_rows_by_key
demand_interval_current_rows(demand_interval_repository& repo,
                             const ores::database::context& ctx,
                             const std::vector<domain::demand_interval>& of) {
    std::vector<std::string> ids;
    ids.reserve(of.size());
    for (const auto& row : of) {
        ids.push_back(boost::uuids::to_string(row.id));
    }
    demand_interval_rows_by_key current;
    for (auto& item : repo.read_latest(ctx, ids))
        current[std::make_tuple(boost::uuids::to_string(item.id))].push_back(std::move(item));
    return current;
}

/**
 * @brief The current rows that the key of an object names, or none.
 */
const std::vector<domain::demand_interval>&
demand_interval_rows_of(const demand_interval_rows_by_key& current,
                        const domain::demand_interval& item) {
    static const std::vector<domain::demand_interval> none;
    const auto found = current.find(std::make_tuple(boost::uuids::to_string(item.id)));
    return found == current.end() ? none : found->second;
}

/**
 * @brief The claim a replace makes: the version the row carries now, or
 * that no row exists yet.
 */
ores::utility::domain::precondition
demand_interval_replace_claim(const std::vector<domain::demand_interval>& current) {
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
domain::demand_interval
demand_interval_apply_claim(const domain::demand_interval& v,
                            const ores::utility::domain::precondition& claim,
                            const std::vector<domain::demand_interval>& current) {
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

void demand_interval_repository::write(const context& ctx, const domain::demand_interval& v) {
    write(ctx, v, demand_interval_replace_claim(read_latest(ctx, boost::uuids::to_string(v.id))));
}

void demand_interval_repository::write(const context& ctx,
                                       const std::vector<domain::demand_interval>& v) {
    const auto current = demand_interval_current_rows(*this, ctx, v);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(demand_interval_replace_claim(demand_interval_rows_of(current, item)));
    write(ctx, v, claims);
}

void demand_interval_repository::write(const context& ctx,
                                       const domain::demand_interval& v,
                                       const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing demand interval. " << "id: " << v.id;
    const auto t = demand_interval_apply_claim(v,
                                               claim,
                                               demand_interval_claim_reads_current(claim) ?
                                                   read_latest(ctx, boost::uuids::to_string(v.id)) :
                                                   std::vector<domain::demand_interval>{});
    query_builder::execute_write_query(
        ctx, demand_interval_mapper::map(t), lg(), "Writing demand interval to database.");
}

void demand_interval_repository::write(
    const context& ctx,
    const std::vector<domain::demand_interval>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing demand intervals. Count: " << v.size();
    std::vector<domain::demand_interval> to_read;
    for (std::size_t i = 0; i < v.size(); ++i) {
        if (demand_interval_claim_reads_current(claims[i]))
            to_read.push_back(v[i]);
    }
    const auto current = demand_interval_current_rows(*this, ctx, to_read);
    std::vector<domain::demand_interval> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(
            demand_interval_apply_claim(v[i], claims[i], demand_interval_rows_of(current, v[i])));
    query_builder::execute_write_query(
        ctx, demand_interval_mapper::map(batch), lg(), "Writing demand intervals to database.");
}

std::vector<domain::demand_interval> demand_interval_repository::read_latest(const context& ctx) {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<demand_interval_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("id"_c);

    return query_builder::execute_read_query<demand_interval_entity, domain::demand_interval>(
        ctx,
        query,
        [](const auto& entities) { return demand_interval_mapper::map(entities); },
        lg(),
        "Reading latest demand intervals");
}

std::vector<domain::demand_interval>
demand_interval_repository::read_latest(const context& ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest demand interval. " << "id: " << id;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<demand_interval_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max);

    return query_builder::execute_read_query<demand_interval_entity, domain::demand_interval>(
        ctx,
        query,
        [](const auto& entities) { return demand_interval_mapper::map(entities); },
        lg(),
        "Reading latest demand interval by id.");
}

std::vector<domain::demand_interval>
demand_interval_repository::read_latest_by_oresmd_uri(const context& ctx,
                                                      const std::string& oresmd_uri) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest demand interval by oresmd_uri: " << oresmd_uri;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<demand_interval_entity>> |
                       query_builder::where("tenant_id"_c == tid && "oresmd_uri"_c == oresmd_uri &&
                                            "valid_to"_c == max);

    // A key names a row of one party: another party can hold the same key, so
    // the lookup keeps the rows of the session's party when it has one. A
    // finder with a parent column is pinned to its parent's party already.
    auto found = query_builder::execute_read_query<demand_interval_entity, domain::demand_interval>(
        ctx,
        query,
        [](const auto& entities) { return demand_interval_mapper::map(entities); },
        lg(),
        "Reading latest demand interval by oresmd_uri.");
    if (const auto party = ctx.party_id(); party.has_value())
        std::erase_if(found, [&](const auto& row) { return row.party_id != *party; });
    return found;
}

std::vector<domain::demand_interval>
demand_interval_repository::read_latest_by_oresmd_uri(const context& ctx,
                                                      const std::vector<std::string>& oresmd_uris) {
    if (oresmd_uris.empty())
        return {};
    // Every key is a bind and the store's bind count is finite, so a larger
    // key set is read one chunk at a time.
    constexpr std::size_t chunk_size = 1000;
    if (oresmd_uris.size() > chunk_size) {
        std::vector<domain::demand_interval> all;
        for (std::size_t begin = 0; begin < oresmd_uris.size(); begin += chunk_size) {
            const auto end = std::min(begin + chunk_size, oresmd_uris.size());
            auto part = read_latest_by_oresmd_uri(
                ctx,
                std::vector<std::string>(oresmd_uris.begin() + begin, oresmd_uris.begin() + end));
            all.insert(all.end(),
                       std::make_move_iterator(part.begin()),
                       std::make_move_iterator(part.end()));
        }
        return all;
    }
    BOOST_LOG_SEV(lg(), debug) << "Reading latest demand intervals by oresmd_uri. Count: "
                               << oresmd_uris.size();
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<demand_interval_entity>> |
                       query_builder::where("tenant_id"_c == tid &&
                                            "oresmd_uri"_c.in(oresmd_uris) && "valid_to"_c == max);
    auto found = query_builder::execute_read_query<demand_interval_entity, domain::demand_interval>(
        ctx,
        query,
        [](const auto& entities) { return demand_interval_mapper::map(entities); },
        lg(),
        "Reading latest demand intervals by oresmd_uri.");
    // The same party rule as the read of one key.
    if (const auto party = ctx.party_id(); party.has_value())
        std::erase_if(found, [&](const auto& row) { return row.party_id != *party; });
    return found;
}

std::vector<domain::demand_interval>
demand_interval_repository::read_any_by_oresmd_uri(const context& ctx,
                                                   const std::string& oresmd_uri) {
    BOOST_LOG_SEV(lg(), debug) << "Reading any demand interval by oresmd_uri: " << oresmd_uri;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<demand_interval_entity>> |
                       query_builder::where("tenant_id"_c == tid && "oresmd_uri"_c == oresmd_uri) |
                       query_builder::order_by("valid_from"_c.desc());

    auto found = query_builder::execute_read_query<demand_interval_entity, domain::demand_interval>(
        ctx,
        query,
        [](const auto& entities) { return demand_interval_mapper::map(entities); },
        lg(),
        "Reading any demand interval by oresmd_uri.");
    // A key names a row of one party, and another party of the tenant may hold
    // the same key, so the read keeps the session's party's rows. The filter
    // runs before the newest row is taken because the party column may be
    // null, and a null party means the row belongs to the whole tenant and
    // stays readable by every party, as the latest-by finder also has it.
    if (const auto party = ctx.party_id(); party.has_value())
        std::erase_if(found, [&](const auto& row) { return row.party_id != *party; });
    if (found.size() > 1)
        found.resize(1);
    return found;
}


std::vector<domain::demand_interval> demand_interval_repository::read_all(const context& ctx,
                                                                          const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all demand interval versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<demand_interval_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id) |
                       query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<demand_interval_entity, domain::demand_interval>(
        ctx,
        query,
        [](const auto& entities) { return demand_interval_mapper::map(entities); },
        lg(),
        "Reading all demand interval versions by id.");
}

std::optional<domain::demand_interval> demand_interval_repository::read_at_version(
    const context& ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading demand interval at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<demand_interval_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
        query_builder::limit(1);

    const auto entities =
        query_builder::execute_read_query<demand_interval_entity, domain::demand_interval>(
            ctx,
            query,
            [](const auto& entities) { return demand_interval_mapper::map(entities); },
            lg(),
            "Reading demand interval at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

demand_interval_repository::remove_status demand_interval_repository::remove(
    const context& ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing demand interval. " << "id: " << id;
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
    const auto query = query_builder::delete_from<demand_interval_entity> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id &&
                                            "valid_to"_c == max && "version"_c == expected);

    query_builder::execute_delete_query(
        ctx, query, lg(), "Removing demand interval from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void demand_interval_repository::remove(const context& ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::demand_interval> demand_interval_repository::read_latest(
    const context& ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::demand_intervals_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest demand intervals with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<demand_interval_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<demand_interval_entity,
                                                     domain::demand_interval>(
        ctx,
        query,
        demand_interval_list_order(order, {"id"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of),
                                demand_interval_filter_condition(filter)),
        [](const auto& entities) { return demand_interval_mapper::map(entities); },
        lg(),
        "Reading latest demand intervals with pagination.");
}

std::uint32_t demand_interval_repository::get_total_demand_interval_count(
    const context& ctx,
    const std::optional<messaging::demand_intervals_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active demand interval count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<demand_interval_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<demand_interval_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of),
                                demand_interval_filter_condition(filter)),
        lg(),
        "Counting demand intervals");
}

std::vector<domain::demand_interval>
demand_interval_repository::read_latest(const context& ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    // Every key is a bind and the store's bind count is finite, so a larger
    // key set is read one chunk at a time.
    constexpr std::size_t chunk_size = 1000;
    if (ids.size() > chunk_size) {
        std::vector<domain::demand_interval> all;
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
        query_builder::read<std::vector<demand_interval_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max);
    auto result =
        query_builder::execute_read_query<demand_interval_entity, domain::demand_interval>(
            ctx,
            query,
            [](const auto& entities) { return demand_interval_mapper::map(entities); },
            lg(),
            "Reading latest demand intervals by ids.");
    return result;
}

void demand_interval_repository::remove(const context& ctx, const std::vector<std::string>& ids) {
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
        query_builder::delete_from<demand_interval_entity> |
        query_builder::where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max);
    query_builder::execute_delete_query(ctx, query, lg(), "Batch removing demand intervals.");
}


std::vector<domain::demand_interval>
demand_interval_repository::read_latest_all_tenants(const context& ctx) {
    static constexpr auto max = db_timestamp::max();
    const auto query = query_builder::read<std::vector<demand_interval_entity>> |
                       query_builder::where("valid_to"_c == max) | query_builder::order_by("id"_c);

    return query_builder::execute_read_query<demand_interval_entity, domain::demand_interval>(
        ctx,
        query,
        [](const auto& entities) { return demand_interval_mapper::map(entities); },
        lg(),
        "Reading latest demand intervals across all tenants");
}

}
