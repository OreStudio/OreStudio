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
#include "ores.marketdata.core/repository/delivery_count_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.marketdata.api/domain/delivery_count.hpp"
#include "ores.marketdata.api/domain/delivery_count_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.api/messaging/delivery_count_protocol.hpp"
#include "ores.marketdata.core/repository/delivery_count_entity.hpp"
#include "ores.marketdata.core/repository/delivery_count_mapper.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_io.hpp>
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

namespace ores::marketdata::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string delivery_count_repository::sql() {
    return query_builder::insert_statement_sql<delivery_count_entity>(lg());
}

bool delivery_count_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order delivery_count_list_order(const ores::utility::domain::order& order,
                                               std::initializer_list<std::string> default_columns,
                                               bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns,
            default_descending != order.descending,
            {"party_id", "provider_code", "feed_id", "oresmd_uri", "period_start"});
    if (!delivery_count_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of delivery counts cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order(
        {order.field},
        order.descending,
        {"party_id", "provider_code", "feed_id", "oresmd_uri", "period_start"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition>
delivery_count_filter_condition(const std::optional<messaging::delivery_counts_filter>& filter) {
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
bool delivery_count_claim_reads_current(const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    // No version to state, so only a create needs to know whether a row exists.
    return claim.kind == precondition_kind::must_not_exist;
}

using delivery_count_rows_by_key =
    std::map<std::tuple<std::string, std::string, std::string, std::string, std::string>,
             std::vector<domain::delivery_count>>;

/**
 * @brief The current rows the objects name, by the text form of their key.
 *
 * One batch read for the set, so a batch checks its claims against one read
 * rather than one read per row.
 */
delivery_count_rows_by_key
delivery_count_current_rows(delivery_count_repository& repo,
                            const ores::database::context& ctx,
                            const std::vector<domain::delivery_count>& of) {
    std::vector<std::string> party_ids;
    party_ids.reserve(of.size());
    std::vector<std::string> provider_codes;
    provider_codes.reserve(of.size());
    std::vector<std::string> feed_ids;
    feed_ids.reserve(of.size());
    std::vector<std::string> oresmd_uris;
    oresmd_uris.reserve(of.size());
    std::vector<std::string> period_starts;
    period_starts.reserve(of.size());
    for (const auto& row : of) {
        party_ids.push_back(boost::uuids::to_string(row.party_id));
        provider_codes.push_back(row.provider_code);
        feed_ids.push_back(boost::uuids::to_string(row.feed_id));
        oresmd_uris.push_back(row.oresmd_uri);
        period_starts.push_back(ores::platform::time::datetime::to_db_string(row.period_start));
    }
    delivery_count_rows_by_key current;
    for (auto& item :
         repo.read_latest(ctx, party_ids, provider_codes, feed_ids, oresmd_uris, period_starts))
        current[std::make_tuple(boost::uuids::to_string(item.party_id),
                                item.provider_code,
                                boost::uuids::to_string(item.feed_id),
                                item.oresmd_uri,
                                ores::platform::time::datetime::to_db_string(item.period_start))]
            .push_back(std::move(item));
    return current;
}

/**
 * @brief The current rows that the key of an object names, or none.
 */
const std::vector<domain::delivery_count>&
delivery_count_rows_of(const delivery_count_rows_by_key& current,
                       const domain::delivery_count& item) {
    static const std::vector<domain::delivery_count> none;
    const auto found = current.find(
        std::make_tuple(boost::uuids::to_string(item.party_id),
                        item.provider_code,
                        boost::uuids::to_string(item.feed_id),
                        item.oresmd_uri,
                        ores::platform::time::datetime::to_db_string(item.period_start)));
    return found == current.end() ? none : found->second;
}

/**
 * @brief The claim a replace makes: the version the row carries now, or
 * that no row exists yet.
 */
ores::utility::domain::precondition
delivery_count_replace_claim(const std::vector<domain::delivery_count>& current) {
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
domain::delivery_count
delivery_count_apply_claim(const domain::delivery_count& v,
                           const ores::utility::domain::precondition& claim,
                           const std::vector<domain::delivery_count>& current) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "delivery_count_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist && !current.empty())
        throw std::invalid_argument(
            "delivery_count_repository::write: a current row already exists");
    return t;
}

}

void delivery_count_repository::write(const context& ctx, const domain::delivery_count& v) {
    write(ctx,
          v,
          delivery_count_replace_claim(
              read_latest(ctx,
                          boost::uuids::to_string(v.party_id),
                          v.provider_code,
                          boost::uuids::to_string(v.feed_id),
                          v.oresmd_uri,
                          ores::platform::time::datetime::to_db_string(v.period_start))));
}

void delivery_count_repository::write(const context& ctx,
                                      const std::vector<domain::delivery_count>& v) {
    const auto current = delivery_count_current_rows(*this, ctx, v);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(delivery_count_replace_claim(delivery_count_rows_of(current, item)));
    write(ctx, v, claims);
}

void delivery_count_repository::write(const context& ctx,
                                      const domain::delivery_count& v,
                                      const ores::utility::domain::precondition& claim) {
    ORES_LOG_SEV(lg(), debug) << "Writing delivery count. " << "party_id: " << v.party_id
                              << " provider_code: " << v.provider_code << " feed_id: " << v.feed_id
                              << " oresmd_uri: " << v.oresmd_uri
                              << " period_start: " << v.period_start;
    const auto t = delivery_count_apply_claim(
        v,
        claim,
        delivery_count_claim_reads_current(claim) ?
            read_latest(ctx,
                        boost::uuids::to_string(v.party_id),
                        v.provider_code,
                        boost::uuids::to_string(v.feed_id),
                        v.oresmd_uri,
                        ores::platform::time::datetime::to_db_string(v.period_start)) :
            std::vector<domain::delivery_count>{});
    query_builder::execute_write_op(ctx,
                                    query_builder::insert_or_replace(delivery_count_mapper::map(t)),
                                    lg(),
                                    "Writing delivery count to database.");
}

void delivery_count_repository::write(
    const context& ctx,
    const std::vector<domain::delivery_count>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    ORES_LOG_SEV(lg(), debug) << "Writing delivery counts. Count: " << v.size();
    if (claims.size() != v.size())
        throw std::invalid_argument(
            "delivery_count_repository::write: one claim is needed for each row");
    std::vector<domain::delivery_count> to_read;
    for (std::size_t i = 0; i < v.size(); ++i) {
        if (delivery_count_claim_reads_current(claims[i]))
            to_read.push_back(v[i]);
    }
    const auto current = delivery_count_current_rows(*this, ctx, to_read);
    std::vector<domain::delivery_count> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(
            delivery_count_apply_claim(v[i], claims[i], delivery_count_rows_of(current, v[i])));
    query_builder::execute_write_op(
        ctx,
        query_builder::insert_or_replace(delivery_count_mapper::map(batch)),
        lg(),
        "Writing delivery counts to database.");
}

std::vector<domain::delivery_count> delivery_count_repository::read_latest(const context& ctx) {
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<delivery_count_entity>> |
        query_builder::where("tenant_id"_c == tid) |
        query_builder::order_by(
            "party_id"_c, "provider_code"_c, "feed_id"_c, "oresmd_uri"_c, "period_start"_c);

    return query_builder::execute_read_query<delivery_count_entity, domain::delivery_count>(
        ctx,
        query,
        [](const auto& entities) { return delivery_count_mapper::map(entities); },
        lg(),
        "Reading latest delivery counts");
}

std::vector<domain::delivery_count>
delivery_count_repository::read_latest(const context& ctx,
                                       const std::string& party_id,
                                       const std::string& provider_code,
                                       const std::string& feed_id,
                                       const std::string& oresmd_uri,
                                       const std::string& period_start) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest delivery count. " << "party_id: " << party_id
                              << " provider_code: " << provider_code << " feed_id: " << feed_id
                              << " oresmd_uri: " << oresmd_uri << " period_start: " << period_start;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<delivery_count_entity>> |
        query_builder::where("tenant_id"_c == tid && "party_id"_c == party_id &&
                             "provider_code"_c == provider_code && "feed_id"_c == feed_id &&
                             "oresmd_uri"_c == oresmd_uri && "period_start"_c == period_start);

    return query_builder::execute_read_query<delivery_count_entity, domain::delivery_count>(
        ctx,
        query,
        [](const auto& entities) { return delivery_count_mapper::map(entities); },
        lg(),
        "Reading latest delivery count by party_id.");
}

std::vector<domain::delivery_count>
delivery_count_repository::read_latest_by_oresmd_uri(const context& ctx,
                                                     const std::string& oresmd_uri) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest delivery count by oresmd_uri: " << oresmd_uri;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<delivery_count_entity>> |
                       query_builder::where("tenant_id"_c == tid && "oresmd_uri"_c == oresmd_uri);

    return query_builder::execute_read_query<delivery_count_entity, domain::delivery_count>(
        ctx,
        query,
        [](const auto& entities) { return delivery_count_mapper::map(entities); },
        lg(),
        "Reading latest delivery count by oresmd_uri.");
}

std::vector<domain::delivery_count>
delivery_count_repository::read_latest_by_oresmd_uri(const context& ctx,
                                                     const std::vector<std::string>& oresmd_uris) {
    if (oresmd_uris.empty())
        return {};
    // Every key is a bind and the store's bind count is finite, so a larger
    // key set is read one chunk at a time.
    constexpr std::size_t chunk_size = 1000;
    if (oresmd_uris.size() > chunk_size) {
        std::vector<domain::delivery_count> all;
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
    ORES_LOG_SEV(lg(), debug) << "Reading latest delivery counts by oresmd_uri. Count: "
                              << oresmd_uris.size();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<delivery_count_entity>> |
                       query_builder::where("tenant_id"_c == tid && "oresmd_uri"_c.in(oresmd_uris));
    auto found = query_builder::execute_read_query<delivery_count_entity, domain::delivery_count>(
        ctx,
        query,
        [](const auto& entities) { return delivery_count_mapper::map(entities); },
        lg(),
        "Reading latest delivery counts by oresmd_uri.");
    return found;
}


std::vector<domain::delivery_count>
delivery_count_repository::read_all(const context& ctx,
                                    const std::string& party_id,
                                    const std::string& provider_code,
                                    const std::string& feed_id,
                                    const std::string& oresmd_uri,
                                    const std::string& period_start) {
    ORES_LOG_SEV(lg(), debug) << "Reading all delivery count versions. " << "party_id: " << party_id
                              << " provider_code: " << provider_code << " feed_id: " << feed_id
                              << " oresmd_uri: " << oresmd_uri << " period_start: " << period_start;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<delivery_count_entity>> |
        query_builder::where("tenant_id"_c == tid && "party_id"_c == party_id &&
                             "provider_code"_c == provider_code && "feed_id"_c == feed_id &&
                             "oresmd_uri"_c == oresmd_uri && "period_start"_c == period_start) |
        query_builder::order_by(
            "party_id"_c, "provider_code"_c, "feed_id"_c, "oresmd_uri"_c, "period_start"_c);

    return query_builder::execute_read_query<delivery_count_entity, domain::delivery_count>(
        ctx,
        query,
        [](const auto& entities) { return delivery_count_mapper::map(entities); },
        lg(),
        "Reading all delivery count versions by party_id.");
}


delivery_count_repository::remove_status
delivery_count_repository::remove(const context& ctx,
                                  const std::string& party_id,
                                  const std::string& provider_code,
                                  const std::string& feed_id,
                                  const std::string& oresmd_uri,
                                  const std::string& period_start,
                                  std::optional<std::uint32_t> version) {
    ORES_LOG_SEV(lg(), debug) << "Removing delivery count. " << "party_id: " << party_id
                              << " provider_code: " << provider_code << " feed_id: " << feed_id
                              << " oresmd_uri: " << oresmd_uri << " period_start: " << period_start;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current =
        read_latest(ctx, party_id, provider_code, feed_id, oresmd_uri, period_start);
    if (current.empty())
        return remove_status::missing;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<delivery_count_entity> |
        query_builder::where("tenant_id"_c == tid && "party_id"_c == party_id &&
                             "provider_code"_c == provider_code && "feed_id"_c == feed_id &&
                             "oresmd_uri"_c == oresmd_uri && "period_start"_c == period_start);

    query_builder::execute_delete_query(ctx, query, lg(), "Removing delivery count from database.");
    return remove_status::removed;
}

void delivery_count_repository::remove(const context& ctx,
                                       const std::string& party_id,
                                       const std::string& provider_code,
                                       const std::string& feed_id,
                                       const std::string& oresmd_uri,
                                       const std::string& period_start) {
    static_cast<void>(
        remove(ctx, party_id, provider_code, feed_id, oresmd_uri, period_start, std::nullopt));
}

std::vector<domain::delivery_count> delivery_count_repository::read_latest(
    const context& ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::delivery_counts_filter>& filter) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest delivery counts with offset: " << offset
                              << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<delivery_count_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<delivery_count_entity, domain::delivery_count>(
        ctx,
        query,
        delivery_count_list_order(
            order, {"party_id", "provider_code", "feed_id", "oresmd_uri", "period_start"}, false),
        delivery_count_filter_condition(filter),
        [](const auto& entities) { return delivery_count_mapper::map(entities); },
        lg(),
        "Reading latest delivery counts with pagination.");
}

std::uint32_t delivery_count_repository::get_total_delivery_count_count(
    const context& ctx, const std::optional<messaging::delivery_counts_filter>& filter) {
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active delivery count count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<delivery_count_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<delivery_count_entity>(
        ctx, query, delivery_count_filter_condition(filter), lg(), "Counting delivery counts");
}

std::vector<domain::delivery_count>
delivery_count_repository::read_latest(const context& ctx,
                                       const std::vector<std::string>& party_ids,
                                       const std::vector<std::string>& provider_codes,
                                       const std::vector<std::string>& feed_ids,
                                       const std::vector<std::string>& oresmd_uris,
                                       const std::vector<std::string>& period_starts) {
    if (party_ids.empty() || provider_codes.empty() || feed_ids.empty() || oresmd_uris.empty() ||
        period_starts.empty())
        return {};
    if (provider_codes.size() != party_ids.size() || feed_ids.size() != party_ids.size() ||
        oresmd_uris.size() != party_ids.size() || period_starts.size() != party_ids.size())
        throw std::invalid_argument(
            "delivery_count_repository::read_latest: key column vectors must be the same length");
    // Every key is a bind and the store's bind count is finite, so a larger
    // key set is read one chunk at a time.
    constexpr std::size_t chunk_size = 1000;
    if (party_ids.size() > chunk_size) {
        std::vector<domain::delivery_count> all;
        for (std::size_t begin = 0; begin < party_ids.size(); begin += chunk_size) {
            const auto end = std::min(begin + chunk_size, party_ids.size());
            auto part = read_latest(
                ctx,
                std::vector<std::string>(party_ids.begin() + begin, party_ids.begin() + end),
                std::vector<std::string>(provider_codes.begin() + begin,
                                         provider_codes.begin() + end),
                std::vector<std::string>(feed_ids.begin() + begin, feed_ids.begin() + end),
                std::vector<std::string>(oresmd_uris.begin() + begin, oresmd_uris.begin() + end),
                std::vector<std::string>(period_starts.begin() + begin,
                                         period_starts.begin() + end));
            all.insert(all.end(),
                       std::make_move_iterator(part.begin()),
                       std::make_move_iterator(part.end()));
        }
        return all;
    }
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<delivery_count_entity>> |
        query_builder::where("tenant_id"_c == tid && "party_id"_c.in(party_ids) &&
                             "provider_code"_c.in(provider_codes) && "feed_id"_c.in(feed_ids) &&
                             "oresmd_uri"_c.in(oresmd_uris) && "period_start"_c.in(period_starts));
    auto result = query_builder::execute_read_query<delivery_count_entity, domain::delivery_count>(
        ctx,
        query,
        [](const auto& entities) { return delivery_count_mapper::map(entities); },
        lg(),
        "Reading latest delivery counts by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (the query builder has no tuple IN), so filter down to the
    // exact requested key-tuples here.
    std::set<std::tuple<std::string, std::string, std::string, std::string, std::string>> requested;
    for (std::size_t i = 0; i < party_ids.size(); ++i)
        requested.emplace(
            party_ids[i], provider_codes[i], feed_ids[i], oresmd_uris[i], period_starts[i]);
    std::vector<domain::delivery_count> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(
                std::make_tuple(boost::uuids::to_string(item.party_id),
                                item.provider_code,
                                boost::uuids::to_string(item.feed_id),
                                item.oresmd_uri,
                                ores::platform::time::datetime::to_db_string(item.period_start))))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void delivery_count_repository::remove(const context& ctx,
                                       const std::vector<std::string>& party_ids,
                                       const std::vector<std::string>& provider_codes,
                                       const std::vector<std::string>& feed_ids,
                                       const std::vector<std::string>& oresmd_uris,
                                       const std::vector<std::string>& period_starts) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (provider_codes.size() != party_ids.size() || feed_ids.size() != party_ids.size() ||
        oresmd_uris.size() != party_ids.size() || period_starts.size() != party_ids.size())
        throw std::invalid_argument(
            "delivery_count_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < party_ids.size(); ++i)
        remove(ctx, party_ids[i], provider_codes[i], feed_ids[i], oresmd_uris[i], period_starts[i]);
}


std::vector<domain::delivery_count>
delivery_count_repository::read_latest_all_tenants(const context& ctx) {
    const auto query =
        query_builder::read<std::vector<delivery_count_entity>> | query_builder::order_by("id"_c);

    return query_builder::execute_read_query<delivery_count_entity, domain::delivery_count>(
        ctx,
        query,
        [](const auto& entities) { return delivery_count_mapper::map(entities); },
        lg(),
        "Reading latest delivery counts across all tenants");
}

}
