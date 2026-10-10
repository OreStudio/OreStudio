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
#include "ores.marketdata.core/repository/observation_lineage_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/list_filter.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/observation_lineage.hpp"
#include "ores.marketdata.api/domain/observation_lineage_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.api/messaging/observation_lineage_protocol.hpp"
#include "ores.marketdata.core/repository/observation_lineage_entity.hpp"
#include "ores.marketdata.core/repository/observation_lineage_mapper.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <sqlgen/delete_from.hpp>
#include <sqlgen/dynamic/Condition.hpp>
#include <sqlgen/dynamic/OrderBy.hpp>
#include <sqlgen/dynamic/Value.hpp>
#include <sqlgen/limit.hpp>
#include <sqlgen/literals.hpp>
#include <sqlgen/offset.hpp>
#include <sqlgen/order_by.hpp>
#include <sqlgen/read.hpp>
#include <sqlgen/where.hpp>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::marketdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string observation_lineage_repository::sql() {
    return generate_create_table_sql<observation_lineage_entity>(lg());
}

bool observation_lineage_repository::is_sortable(std::string_view field) {
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
    if (!observation_lineage_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of observation lineages cannot be ordered by " +
                                    order.field + ".");
    return make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<sqlgen::dynamic::Condition>
filter_condition(const std::optional<messaging::observation_lineages_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<sqlgen::dynamic::Condition> r;
    if (filter->id_one_of) {
        std::vector<sqlgen::dynamic::Value> values;
        for (const auto& v : *filter->id_one_of)
            values.push_back(filter_value(v));
        r.push_back(one_of("id", std::move(values)));
    }
    return all_of(std::move(r));
}

}

ores::utility::domain::precondition
observation_lineage_repository::replace_claim(context ctx, const domain::observation_lineage& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::observation_lineage
observation_lineage_repository::apply_claim(context ctx,
                                            const domain::observation_lineage& v,
                                            const ores::utility::domain::precondition& claim) {
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

void observation_lineage_repository::write(context ctx, const domain::observation_lineage& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void observation_lineage_repository::write(context ctx,
                                           const std::vector<domain::observation_lineage>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void observation_lineage_repository::write(context ctx,
                                           const domain::observation_lineage& v,
                                           const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing observation lineage. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(
        ctx, observation_lineage_mapper::map(t), lg(), "Writing observation lineage to database.");
}

void observation_lineage_repository::write(
    context ctx,
    const std::vector<domain::observation_lineage>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing observation lineages. Count: " << v.size();
    std::vector<domain::observation_lineage> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(ctx,
                        observation_lineage_mapper::map(batch),
                        lg(),
                        "Writing observation lineages to database.");
}

std::vector<domain::observation_lineage> observation_lineage_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<observation_lineage_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<observation_lineage_entity, domain::observation_lineage>(
        ctx,
        query,
        [](const auto& entities) { return observation_lineage_mapper::map(entities); },
        lg(),
        "Reading latest observation lineages");
}

std::vector<domain::observation_lineage>
observation_lineage_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest observation lineage. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<observation_lineage_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<observation_lineage_entity, domain::observation_lineage>(
        ctx,
        query,
        [](const auto& entities) { return observation_lineage_mapper::map(entities); },
        lg(),
        "Reading latest observation lineage by id.");
}


std::vector<domain::observation_lineage>
observation_lineage_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all observation lineage versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<observation_lineage_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<observation_lineage_entity, domain::observation_lineage>(
        ctx,
        query,
        [](const auto& entities) { return observation_lineage_mapper::map(entities); },
        lg(),
        "Reading all observation lineage versions by id.");
}

std::optional<domain::observation_lineage> observation_lineage_repository::read_at_version(
    context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading observation lineage at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<observation_lineage_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities =
        execute_read_query<observation_lineage_entity, domain::observation_lineage>(
            ctx,
            query,
            [](const auto& entities) { return observation_lineage_mapper::map(entities); },
            lg(),
            "Reading observation lineage at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

observation_lineage_repository::remove_status observation_lineage_repository::remove(
    context ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing observation lineage. " << "id: " << id;
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
    const auto query = sqlgen::delete_from<observation_lineage_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value() &&
                             "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing observation lineage from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void observation_lineage_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::observation_lineage> observation_lineage_repository::read_latest(
    context ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::observation_lineages_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest observation lineages with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<observation_lineage_entity>> |
                       where("tenant_id"_c == tid) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<observation_lineage_entity, domain::observation_lineage>(
        ctx,
        query,
        list_order(order, {"id"}, false),
        narrowed(valid_at(as_of), filter_condition(filter)),
        [](const auto& entities) { return observation_lineage_mapper::map(entities); },
        lg(),
        "Reading latest observation lineages with pagination.");
}

std::uint32_t observation_lineage_repository::get_total_observation_lineage_count(
    context ctx,
    const std::optional<messaging::observation_lineages_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active observation lineage count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<observation_lineage_entity>> | where("tenant_id"_c == tid);

    return execute_count_query<observation_lineage_entity>(
        ctx,
        query,
        narrowed(valid_at(as_of), filter_condition(filter)),
        lg(),
        "Counting observation lineages");
}

std::vector<domain::observation_lineage>
observation_lineage_repository::read_latest(context ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<observation_lineage_entity>> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    auto result = execute_read_query<observation_lineage_entity, domain::observation_lineage>(
        ctx,
        query,
        [](const auto& entities) { return observation_lineage_mapper::map(entities); },
        lg(),
        "Reading latest observation lineages by ids.");
    return result;
}

void observation_lineage_repository::remove(context ctx, const std::vector<std::string>& ids) {
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
    const auto query = sqlgen::delete_from<observation_lineage_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing observation lineages.");
}


std::optional<domain::observation_lineage>
observation_lineage_repository::read_latest_by_observation(
    context ctx,
    const boost::uuids::uuid& series_id,
    std::chrono::system_clock::time_point observation_datetime,
    const std::string& oresmd_uri) {
    using ores::platform::time::datetime;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto sid = boost::uuids::to_string(series_id);
    const auto odt_str = datetime::to_iso8601_utc(observation_datetime);

    const auto query =
        sqlgen::read<std::vector<observation_lineage_entity>> |
        where("tenant_id"_c == tid && "series_id"_c == sid && "observation_datetime"_c == odt_str &&
              "oresmd_uri"_c == oresmd_uri && "valid_to"_c == max.value()) |
        order_by("id"_c);
    const auto results =
        execute_read_query<observation_lineage_entity, domain::observation_lineage>(
            ctx,
            query,
            [](const auto& entities) { return observation_lineage_mapper::map(entities); },
            lg(),
            "Reading latest observation lineage by observation");
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::observation_lineage> observation_lineage_repository::read_latest_for_points(
    context ctx, const boost::uuids::uuid& series_id, const std::vector<point_key>& points) {
    using ores::platform::time::datetime;
    if (points.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto sid = boost::uuids::to_string(series_id);

    // The query is a per-column cross-product of the stated URIs and the stated
    // time span, so the rows it returns are narrowed to the exact points below.
    std::vector<std::string> uris;
    uris.reserve(points.size());
    auto first = points.front().observation_datetime;
    auto last = first;
    for (const auto& point : points) {
        uris.push_back(point.oresmd_uri);
        first = std::min(first, point.observation_datetime);
        last = std::max(last, point.observation_datetime);
    }
    std::ranges::sort(uris);
    uris.erase(std::unique(uris.begin(), uris.end()), uris.end());

    const auto query =
        sqlgen::read<std::vector<observation_lineage_entity>> |
        where("tenant_id"_c == tid && "series_id"_c == sid && "oresmd_uri"_c.in(uris) &&
              "observation_datetime"_c >= datetime::to_iso8601_utc(first) &&
              "observation_datetime"_c <= datetime::to_iso8601_utc(last) &&
              "valid_to"_c == max.value()) |
        order_by("id"_c);
    const auto rows = execute_read_query<observation_lineage_entity, domain::observation_lineage>(
        ctx,
        query,
        [](const auto& entities) { return observation_lineage_mapper::map(entities); },
        lg(),
        "Reading latest observation lineages for points");

    std::vector<domain::observation_lineage> result;
    for (const auto& row : rows) {
        const auto stated = std::ranges::any_of(points, [&](const point_key& point) {
            return point.oresmd_uri == row.oresmd_uri &&
                   point.observation_datetime == row.observation_datetime;
        });
        if (stated)
            result.push_back(row);
    }
    return result;
}

}
