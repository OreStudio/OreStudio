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
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.marketdata.api/domain/market_observation_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/market_observation_entity.hpp"
#include "ores.marketdata.core/repository/market_observation_mapper.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <initializer_list>
#include <sqlgen/postgres.hpp>
#include <stdexcept>
#include <string>
#include <string_view>

namespace ores::marketdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string market_observation_repository::sql() {
    return generate_create_table_sql<market_observation_entity>(lg());
}

bool market_observation_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
sqlgen::dynamic::OrderBy list_order(const ores::utility::domain::order& order,
                                    std::initializer_list<std::string> default_columns,
                                    bool default_descending) {
    if (order.field.empty())
        return make_order(default_columns, default_descending != order.descending, {"id"});
    if (!market_observation_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of market observations cannot be ordered by " +
                                    order.field + ".");
    return make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<sqlgen::dynamic::Condition>
filter_condition(const std::optional<messaging::market_observations_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<sqlgen::dynamic::Condition> r;
    if (filter->series_id)
        r.push_back(equals("series_id", filter_value(*filter->series_id)));
    if (filter->id_one_of) {
        std::vector<sqlgen::dynamic::Value> values;
        for (const auto& v : *filter->id_one_of)
            values.push_back(filter_value(v));
        r.push_back(one_of("id", std::move(values)));
    }
    if (filter->series_id_one_of) {
        std::vector<sqlgen::dynamic::Value> values;
        for (const auto& v : *filter->series_id_one_of)
            values.push_back(filter_value(v));
        r.push_back(one_of("series_id", std::move(values)));
    }
    return all_of(std::move(r));
}

}

ores::utility::domain::precondition
market_observation_repository::replace_claim(context ctx, const domain::market_observation& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // by the read in apply_claim.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

domain::market_observation
market_observation_repository::apply_claim(context ctx,
                                           const domain::market_observation& v,
                                           const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "market_observation_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist &&
        !read_latest(ctx, boost::uuids::to_string(v.id)).empty())
        throw std::invalid_argument(
            "market_observation_repository::write: a current row already exists");
    return t;
}

void market_observation_repository::write(context ctx, const domain::market_observation& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void market_observation_repository::insert(context ctx, const domain::market_observation& v) {
    BOOST_LOG_SEV(lg(), debug) << "Inserting market observation. " << "id: " << v.id;
    execute_write_query(ctx,
                        market_observation_mapper::map(v),
                        lg(),
                        "Inserting market observation into database.");
}

void market_observation_repository::insert(context ctx,
                                           const std::vector<domain::market_observation>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Inserting market observations. Count: " << v.size();
    execute_write_query(ctx,
                        market_observation_mapper::map(v),
                        lg(),
                        "Inserting market observations into database.");
}

void market_observation_repository::write(context ctx,
                                          const std::vector<domain::market_observation>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void market_observation_repository::write(context ctx,
                                          const domain::market_observation& v,
                                          const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing market observation. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(
        ctx, market_observation_mapper::map(t), lg(), "Writing market observation to database.");
}

void market_observation_repository::write(
    context ctx,
    const std::vector<domain::market_observation>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing market observations. Count: " << v.size();
    std::vector<domain::market_observation> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(ctx,
                        market_observation_mapper::map(batch),
                        lg(),
                        "Writing market observations to database.");
}

std::vector<domain::market_observation> market_observation_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_observation_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<market_observation_entity, domain::market_observation>(
        ctx,
        query,
        [](const auto& entities) { return market_observation_mapper::map(entities); },
        lg(),
        "Reading latest market observations");
}

std::vector<domain::market_observation>
market_observation_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest market observation. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_observation_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<market_observation_entity, domain::market_observation>(
        ctx,
        query,
        [](const auto& entities) { return market_observation_mapper::map(entities); },
        lg(),
        "Reading latest market observation by id.");
}


std::vector<domain::market_observation>
market_observation_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all market observation versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_observation_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("valid_from"_c.desc());

    return execute_read_query<market_observation_entity, domain::market_observation>(
        ctx,
        query,
        [](const auto& entities) { return market_observation_mapper::map(entities); },
        lg(),
        "Reading all market observation versions by id.");
}


std::vector<domain::market_observation> market_observation_repository::read_latest_by_series_id(
    context ctx,
    const std::string& series_id,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::market_observations_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest market observations. series_id: " << series_id
                               << " offset: " << offset << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<market_observation_entity>> |
        where("tenant_id"_c == tid && "series_id"_c == series_id && "valid_to"_c == max.value()) |
        sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<market_observation_entity, domain::market_observation>(
        ctx,
        query,
        list_order(order, {"observation_datetime"}, true),
        filter_condition(filter),
        [](const auto& entities) { return market_observation_mapper::map(entities); },
        lg(),
        "Reading latest market observations by series_id.");
}

std::uint32_t market_observation_repository::get_total_market_observation_count_by_series_id(
    context ctx,
    const std::string& series_id,
    const std::optional<messaging::market_observations_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active market observations count. series_id: "
                               << series_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<market_observation_entity>> |
        where("tenant_id"_c == tid && "series_id"_c == series_id && "valid_to"_c == max.value());

    return execute_count_query<market_observation_entity>(
        ctx, query, filter_condition(filter), lg(), "Counting market observations by series_id");
}


market_observation_repository::remove_status market_observation_repository::remove(
    context ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing market observation. " << "id: " << id;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, id);
    if (current.empty())
        return remove_status::missing;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<market_observation_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing market observation from database.");
    return remove_status::removed;
}

void market_observation_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::market_observation> market_observation_repository::read_latest(
    context ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::market_observations_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest market observations with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_observation_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<market_observation_entity, domain::market_observation>(
        ctx,
        query,
        list_order(order, {"id"}, false),
        filter_condition(filter),
        [](const auto& entities) { return market_observation_mapper::map(entities); },
        lg(),
        "Reading latest market observations with pagination.");
}

std::uint32_t market_observation_repository::get_total_market_observation_count(
    context ctx, const std::optional<messaging::market_observations_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active market observation count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_observation_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value());

    return execute_count_query<market_observation_entity>(
        ctx, query, filter_condition(filter), lg(), "Counting market observations");
}

std::vector<domain::market_observation>
market_observation_repository::read_latest(context ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_observation_entity>> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    auto result = execute_read_query<market_observation_entity, domain::market_observation>(
        ctx,
        query,
        [](const auto& entities) { return market_observation_mapper::map(entities); },
        lg(),
        "Reading latest market observations by ids.");
    return result;
}

void market_observation_repository::remove(context ctx, const std::vector<std::string>& ids) {
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
    const auto query = sqlgen::delete_from<market_observation_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing market observations.");
}


std::vector<domain::market_observation>
market_observation_repository::read_latest_for_series(context ctx,
                                                      const boost::uuids::uuid& series_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest observations for series: " << series_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto sid = boost::uuids::to_string(series_id);
    const auto query =
        sqlgen::read<std::vector<market_observation_entity>> |
        where("tenant_id"_c == tid && "series_id"_c == sid && "valid_to"_c == max.value()) |
        order_by("observation_datetime"_c);

    return execute_read_query<market_observation_entity, domain::market_observation>(
        ctx,
        query,
        [](const auto& entities) { return market_observation_mapper::map(entities); },
        lg(),
        "Reading latest market observations.");
}

std::vector<domain::market_observation> market_observation_repository::read_as_of(
    context ctx,
    const boost::uuids::uuid& series_id,
    const std::chrono::system_clock::time_point& as_of_datetime) {
    using ores::platform::time::datetime;
    BOOST_LOG_SEV(lg(), debug) << "Reading as-of snapshot for series: " << series_id
                               << " as-of: " << datetime::to_iso8601_utc(as_of_datetime);
    const auto tid = ctx.tenant_id().to_string();
    const auto sid = boost::uuids::to_string(series_id);
    const auto as_of_str = datetime::to_iso8601_utc(as_of_datetime);

    // DISTINCT ON (oresmd_uri) returns exactly one row per point -- the latest at or before
    // as_of_datetime -- reconstructing a curve/grid snapshot from independently-ticking rows.
    // Correct whether every point shares one observation_datetime or the points have
    // staggered timestamps.
    static const std::string sql = R"(
        SELECT DISTINCT ON (oresmd_uri)
            id, tenant_id, party_id, series_id, observation_datetime, oresmd_uri, value,
            source, valid_from, valid_to
        FROM ores_marketdata_market_observations_tbl
        WHERE tenant_id = $1 AND series_id = $2 AND observation_datetime <= $3
            AND valid_to = $4
        ORDER BY oresmd_uri, observation_datetime DESC
    )";

    const auto rows = execute_parameterized_multi_column_query(
        ctx, sql, {tid, sid, as_of_str, MAX_TIMESTAMP}, lg(), "reading as-of curve snapshot");

    std::vector<domain::market_observation> result;
    result.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() < 10)
            throw std::runtime_error("read_as_of: a row has " + std::to_string(row.size()) +
                                     " columns, expected 10");

        market_observation_entity e;
        e.id = row[0].value_or("");
        e.tenant_id = row[1].value_or("");
        e.party_id = row[2].value_or("");
        e.series_id = row[3].value_or("");
        e.observation_datetime = row[4].value_or("");
        e.oresmd_uri = row[5].value_or("");
        e.value = row[6].value_or("");
        e.source = row[7];
        e.valid_from = row[8].value_or("");
        e.valid_to = row[9].value_or("");
        result.push_back(market_observation_mapper::map(e));
    }
    return result;
}

std::vector<std::vector<domain::market_observation>>
market_observation_repository::read_as_of_buckets(
    context ctx,
    const boost::uuids::uuid& series_id,
    const std::chrono::system_clock::time_point& latest_boundary,
    const std::chrono::seconds& bucket_size,
    unsigned int bucket_count) {
    using ores::platform::time::datetime;
    BOOST_LOG_SEV(lg(), debug) << "Reading " << bucket_count
                               << " as-of bucket snapshots for series: " << series_id << " every "
                               << bucket_size.count()
                               << "s ending at: " << datetime::to_iso8601_utc(latest_boundary);
    const auto tid = ctx.tenant_id().to_string();
    const auto sid = boost::uuids::to_string(series_id);
    const auto latest_str = datetime::to_iso8601_utc(latest_boundary);
    const auto bucket_seconds = std::to_string(bucket_size.count());
    const auto count_str = std::to_string(bucket_count);

    // Bucket boundaries and the per-bucket as-of reduction both happen in the database, in
    // one statement: generate_series() produces the (up to bucket_count) boundaries ending
    // at latest_boundary, and the LATERAL subquery resolves each oresmd_uri to its own latest
    // observation at or before that boundary -- a per-bucket DISTINCT ON (oresmd_uri), driven
    // by observations_series_coordinate_datetime_idx (tenant_id, series_id, oresmd_uri,
    // observation_datetime desc), so each point is a skip-scan straight to its latest row
    // per bucket rather than a sort over the whole series. bucket_ordinal lets the caller
    // regroup rows by bucket without relying on floating-point/timestamp equality.
    static const std::string sql = R"(
        WITH boundaries AS (
            SELECT ordinality - 1 AS bucket_ordinal, boundary
            FROM generate_series(
                $3::timestamptz - ($5::int - 1) * ($4::int * interval '1 second'),
                $3::timestamptz,
                $4::int * interval '1 second'
            ) WITH ORDINALITY AS t(boundary, ordinality)
        )
        SELECT b.bucket_ordinal, o.id, o.tenant_id, o.party_id, o.series_id,
               o.observation_datetime, o.oresmd_uri, o.value, o.source, o.valid_from, o.valid_to
        FROM boundaries b
        CROSS JOIN LATERAL (
            SELECT DISTINCT ON (oresmd_uri) *
            FROM ores_marketdata_market_observations_tbl m
            WHERE m.tenant_id = $1 AND m.series_id = $2
                AND m.observation_datetime <= b.boundary
                AND m.valid_to = $6
            ORDER BY oresmd_uri, observation_datetime DESC
        ) o
        ORDER BY b.bucket_ordinal, o.oresmd_uri
    )";

    const auto rows = execute_parameterized_multi_column_query(
        ctx,
        sql,
        {tid, sid, latest_str, bucket_seconds, count_str, MAX_TIMESTAMP},
        lg(),
        "reading as-of bucketed curve evolution");

    std::vector<std::vector<domain::market_observation>> result(bucket_count);
    for (const auto& row : rows) {
        if (row.size() < 11)
            throw std::runtime_error("read_as_of_buckets: a row has " + std::to_string(row.size()) +
                                     " columns, expected 11");
        if (!row[0])
            throw std::runtime_error("read_as_of_buckets: a row has no bucket ordinal");

        const auto ordinal = std::stoul(*row[0]);
        if (ordinal >= result.size())
            throw std::runtime_error("read_as_of_buckets: bucket ordinal " + *row[0] +
                                     " is outside the " + std::to_string(result.size()) +
                                     " buckets requested");

        market_observation_entity e;
        e.id = row[1].value_or("");
        e.tenant_id = row[2].value_or("");
        e.party_id = row[3].value_or("");
        e.series_id = row[4].value_or("");
        e.observation_datetime = row[5].value_or("");
        e.oresmd_uri = row[6].value_or("");
        e.value = row[7].value_or("");
        e.source = row[8];
        e.valid_from = row[9].value_or("");
        e.valid_to = row[10].value_or("");
        result[ordinal].push_back(market_observation_mapper::map(e));
    }
    return result;
}

}
