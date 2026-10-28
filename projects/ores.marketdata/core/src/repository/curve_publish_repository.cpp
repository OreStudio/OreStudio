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
#include "ores.marketdata.core/repository/curve_publish_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/curve_publish.hpp"
#include "ores.marketdata.api/domain/curve_publish_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.api/messaging/curve_publish_protocol.hpp"
#include "ores.marketdata.core/repository/curve_publish_entity.hpp"
#include "ores.marketdata.core/repository/curve_publish_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::marketdata::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string curve_publish_repository::sql() {
    return query_builder::insert_statement_sql<curve_publish_entity>(lg());
}

bool curve_publish_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {
        "as_of",
    };
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order curve_publish_list_order(const ores::utility::domain::order& order,
                                              std::initializer_list<std::string> default_columns,
                                              bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"id"});
    if (!curve_publish_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of curve publishes cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition>
curve_publish_filter_condition(const std::optional<messaging::curve_publishes_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->request_id)
        r.push_back(
            query_builder::equals("request_id", query_builder::filter_value(*filter->request_id)));
    if (filter->party_id)
        r.push_back(
            query_builder::equals("party_id", query_builder::filter_value(*filter->party_id)));
    if (filter->bootstrap_config_id)
        r.push_back(query_builder::equals(
            "bootstrap_config_id", query_builder::filter_value(*filter->bootstrap_config_id)));
    if (filter->id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("id", std::move(values)));
    }
    if (filter->request_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->request_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("request_id", std::move(values)));
    }
    if (filter->party_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->party_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("party_id", std::move(values)));
    }
    if (filter->bootstrap_config_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->bootstrap_config_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("bootstrap_config_id", std::move(values)));
    }
    return query_builder::all_of(std::move(r));
}

}

ores::utility::domain::precondition
curve_publish_repository::replace_claim(context ctx, const domain::curve_publish& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::curve_publish curve_publish_repository::apply_claim(
    context ctx, const domain::curve_publish& v, const ores::utility::domain::precondition& claim) {
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
        case precondition_kind::any: {
            // A caller that claims nothing still has to say what it replaces, so
            // the row is read and its version stated. A row that moved on between
            // this read and the write is a conflict the trigger raises, never a
            // silent overwrite.
            const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void curve_publish_repository::write(context ctx, const domain::curve_publish& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void curve_publish_repository::write(context ctx, const std::vector<domain::curve_publish>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void curve_publish_repository::write(context ctx,
                                     const domain::curve_publish& v,
                                     const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing curve publish. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    query_builder::execute_write_query(
        ctx, curve_publish_mapper::map(t), lg(), "Writing curve publish to database.");
}

void curve_publish_repository::write(
    context ctx,
    const std::vector<domain::curve_publish>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing curve publishes. Count: " << v.size();
    std::vector<domain::curve_publish> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    query_builder::execute_write_query(
        ctx, curve_publish_mapper::map(batch), lg(), "Writing curve publishes to database.");
}

std::vector<domain::curve_publish> curve_publish_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<curve_publish_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       query_builder::order_by("id"_c);

    return query_builder::execute_read_query<curve_publish_entity, domain::curve_publish>(
        ctx,
        query,
        [](const auto& entities) { return curve_publish_mapper::map(entities); },
        lg(),
        "Reading latest curve publishes");
}

std::vector<domain::curve_publish> curve_publish_repository::read_latest(context ctx,
                                                                         const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest curve publish. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<curve_publish_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return query_builder::execute_read_query<curve_publish_entity, domain::curve_publish>(
        ctx,
        query,
        [](const auto& entities) { return curve_publish_mapper::map(entities); },
        lg(),
        "Reading latest curve publish by id.");
}


std::vector<domain::curve_publish> curve_publish_repository::read_all(context ctx,
                                                                      const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all curve publish versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<curve_publish_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id) |
                       query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<curve_publish_entity, domain::curve_publish>(
        ctx,
        query,
        [](const auto& entities) { return curve_publish_mapper::map(entities); },
        lg(),
        "Reading all curve publish versions by id.");
}

std::optional<domain::curve_publish> curve_publish_repository::read_at_version(
    context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading curve publish at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<curve_publish_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
        query_builder::limit(1);

    const auto entities =
        query_builder::execute_read_query<curve_publish_entity, domain::curve_publish>(
            ctx,
            query,
            [](const auto& entities) { return curve_publish_mapper::map(entities); },
            lg(),
            "Reading curve publish at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::curve_publish> curve_publish_repository::read_latest_by_request_id(
    context ctx,
    const std::string& request_id,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::curve_publishes_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest curve publishes. request_id: " << request_id
                               << " offset: " << offset << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<curve_publish_entity>> |
                       query_builder::where("tenant_id"_c == tid && "request_id"_c == request_id &&
                                            "valid_to"_c == max.value()) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<curve_publish_entity, domain::curve_publish>(
        ctx,
        query,
        curve_publish_list_order(order, {"id"}, false),
        curve_publish_filter_condition(filter),
        [](const auto& entities) { return curve_publish_mapper::map(entities); },
        lg(),
        "Reading latest curve publishes by request_id.");
}

std::uint32_t curve_publish_repository::get_total_curve_publish_count_by_request_id(
    context ctx,
    const std::string& request_id,
    const std::optional<messaging::curve_publishes_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active curve publishes count. request_id: "
                               << request_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<curve_publish_entity>> |
                       query_builder::where("tenant_id"_c == tid && "request_id"_c == request_id &&
                                            "valid_to"_c == max.value());

    return query_builder::execute_count_query<curve_publish_entity>(
        ctx,
        query,
        curve_publish_filter_condition(filter),
        lg(),
        "Counting curve publishes by request_id");
}


curve_publish_repository::remove_status curve_publish_repository::remove(
    context ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing curve publish. " << "id: " << id;
    const auto current = read_latest(ctx, id);
    if (current.empty())
        return remove_status::missing;
    // The protocol states the version as a uint32 and the row carries it as an
    // int, so the comparison states the conversion rather than relying on one.
    if (version && static_cast<std::uint32_t>(current.front().version) != *version)
        return remove_status::conflicting;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    // The row is named by its version as well as by its key, so the removal
    // cannot close a row that replaced the one the caller read between the
    // read above and this statement.
    const auto expected = version ? static_cast<int>(*version) : current.front().version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::delete_from<curve_publish_entity> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id &&
                                            "valid_to"_c == max.value() && "version"_c == expected);

    query_builder::execute_delete_query(ctx, query, lg(), "Removing curve publish from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void curve_publish_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::curve_publish> curve_publish_repository::read_latest(
    context ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::curve_publishes_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest curve publishes with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<curve_publish_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<curve_publish_entity, domain::curve_publish>(
        ctx,
        query,
        curve_publish_list_order(order, {"id"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of),
                                curve_publish_filter_condition(filter)),
        [](const auto& entities) { return curve_publish_mapper::map(entities); },
        lg(),
        "Reading latest curve publishes with pagination.");
}

std::uint32_t curve_publish_repository::get_total_curve_publish_count(
    context ctx,
    const std::optional<messaging::curve_publishes_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active curve publish count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<curve_publish_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<curve_publish_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of),
                                curve_publish_filter_condition(filter)),
        lg(),
        "Counting curve publishes");
}

std::vector<domain::curve_publish>
curve_publish_repository::read_latest(context ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<curve_publish_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    auto result = query_builder::execute_read_query<curve_publish_entity, domain::curve_publish>(
        ctx,
        query,
        [](const auto& entities) { return curve_publish_mapper::map(entities); },
        lg(),
        "Reading latest curve publishes by ids.");
    return result;
}

void curve_publish_repository::remove(context ctx, const std::vector<std::string>& ids) {
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
        query_builder::delete_from<curve_publish_entity> |
        query_builder::where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    query_builder::execute_delete_query(ctx, query, lg(), "Batch removing curve publishes.");
}


std::optional<domain::curve_publish>
curve_publish_repository::read_latest_by_request(context ctx, const std::string& request_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading the publish record of request: " << request_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<curve_publish_entity>> |
                       query_builder::where("tenant_id"_c == tid && "request_id"_c == request_id &&
                                            "valid_to"_c == max.value());

    auto rows = query_builder::execute_read_query<curve_publish_entity, domain::curve_publish>(
        ctx,
        query,
        [](const auto& entities) { return curve_publish_mapper::map(entities); },
        lg(),
        "Reading the publish record of a request.");
    if (rows.empty())
        return std::nullopt;
    return rows.front();
}

std::optional<domain::curve_publish>
curve_publish_repository::read_latest_published_for_series(context ctx,
                                                           const boost::uuids::uuid& series_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading the published set of series: " << series_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto sid = boost::uuids::to_string(series_id);
    const auto query = query_builder::read<std::vector<curve_publish_entity>> |
                       query_builder::where("tenant_id"_c == tid && "output_series_id"_c == sid &&
                                            "valid_to"_c == max.value());

    auto rows = query_builder::execute_read_query<curve_publish_entity, domain::curve_publish>(
        ctx,
        query,
        [](const auto& entities) { return curve_publish_mapper::map(entities); },
        lg(),
        "Reading the published set of a series.");

    // The newest applied version wins. A record that holds no instant is a
    // request that still waits, and a waiting request publishes nothing.
    rows.erase(std::remove_if(rows.begin(),
                              rows.end(),
                              [](const auto& r) { return !r.published_at.has_value(); }),
               rows.end());
    if (rows.empty())
        return std::nullopt;
    const auto newest =
        std::max_element(rows.begin(), rows.end(), [](const auto& a, const auto& b) {
            return *a.published_at < *b.published_at;
        });
    return *newest;
}

std::vector<domain::curve_publish>
curve_publish_repository::read_live_for_series(context ctx, const boost::uuids::uuid& series_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading the live sets of series: " << series_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto sid = boost::uuids::to_string(series_id);
    const auto query = query_builder::read<std::vector<curve_publish_entity>> |
                       query_builder::where("tenant_id"_c == tid && "output_series_id"_c == sid &&
                                            "valid_to"_c == max.value());

    auto rows = query_builder::execute_read_query<curve_publish_entity, domain::curve_publish>(
        ctx,
        query,
        [](const auto& entities) { return curve_publish_mapper::map(entities); },
        lg(),
        "Reading the live sets of a series.");

    // A set is live when it was published and nothing ended it: a record with no
    // instant is a request that still waits, and one that carries an end is a set
    // a reader no longer prices from.
    rows.erase(std::remove_if(rows.begin(),
                              rows.end(),
                              [](const auto& r) {
                                  return !r.published_at.has_value() ||
                                         r.withdrawn_at.has_value() || r.superseded_at.has_value();
                              }),
               rows.end());
    std::sort(rows.begin(), rows.end(), [](const auto& a, const auto& b) {
        return *a.published_at > *b.published_at;
    });
    return rows;
}

}
