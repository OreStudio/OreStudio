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
#include "ores.reporting.core/repository/engine_settings_fixings_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/list_filter.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.api/domain/engine_settings_fixings.hpp"
#include "ores.reporting.api/domain/engine_settings_fixings_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.api/messaging/engine_settings_fixings_protocol.hpp"
#include "ores.reporting.core/repository/engine_settings_fixings_entity.hpp"
#include "ores.reporting.core/repository/engine_settings_fixings_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
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

namespace ores::reporting::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string engine_settings_fixings_repository::sql() {
    return generate_create_table_sql<engine_settings_fixings_entity>(lg());
}

bool engine_settings_fixings_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
sqlgen::dynamic::OrderBy
engine_settings_fixings_list_order(const ores::utility::domain::order& order,
                                   std::initializer_list<std::string> default_columns,
                                   bool default_descending) {
    if (order.field.empty())
        return make_order(default_columns, default_descending != order.descending, {"id"});
    if (!engine_settings_fixings_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of engine settings fixings cannot be ordered by " +
                                    order.field + ".");
    return make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<sqlgen::dynamic::Condition> engine_settings_fixings_filter_condition(
    const std::optional<messaging::engine_settings_fixings_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<sqlgen::dynamic::Condition> r;
    if (filter->engine_settings_id)
        r.push_back(equals("engine_settings_id", filter_value(*filter->engine_settings_id)));
    if (filter->id_one_of) {
        std::vector<sqlgen::dynamic::Value> values;
        for (const auto& v : *filter->id_one_of)
            values.push_back(filter_value(v));
        r.push_back(one_of("id", std::move(values)));
    }
    if (filter->engine_settings_id_one_of) {
        std::vector<sqlgen::dynamic::Value> values;
        for (const auto& v : *filter->engine_settings_id_one_of)
            values.push_back(filter_value(v));
        r.push_back(one_of("engine_settings_id", std::move(values)));
    }
    return all_of(std::move(r));
}

}

ores::utility::domain::precondition
engine_settings_fixings_repository::replace_claim(context ctx,
                                                  const domain::engine_settings_fixings& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::engine_settings_fixings
engine_settings_fixings_repository::apply_claim(context ctx,
                                                const domain::engine_settings_fixings& v,
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

void engine_settings_fixings_repository::write(context ctx,
                                               const domain::engine_settings_fixings& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void engine_settings_fixings_repository::write(
    context ctx, const std::vector<domain::engine_settings_fixings>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void engine_settings_fixings_repository::write(context ctx,
                                               const domain::engine_settings_fixings& v,
                                               const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing engine settings fixings. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(ctx,
                        engine_settings_fixings_mapper::map(t),
                        lg(),
                        "Writing engine settings fixings to database.");
}

void engine_settings_fixings_repository::write(
    context ctx,
    const std::vector<domain::engine_settings_fixings>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing engine settings fixings. Count: " << v.size();
    std::vector<domain::engine_settings_fixings> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(ctx,
                        engine_settings_fixings_mapper::map(batch),
                        lg(),
                        "Writing engine settings fixings to database.");
}

std::vector<domain::engine_settings_fixings>
engine_settings_fixings_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<engine_settings_fixings_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<engine_settings_fixings_entity, domain::engine_settings_fixings>(
        ctx,
        query,
        [](const auto& entities) { return engine_settings_fixings_mapper::map(entities); },
        lg(),
        "Reading latest engine settings fixings");
}

std::vector<domain::engine_settings_fixings>
engine_settings_fixings_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest engine settings fixings. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<engine_settings_fixings_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    auto found =
        execute_read_query<engine_settings_fixings_entity, domain::engine_settings_fixings>(
            ctx,
            query,
            [](const auto& entities) { return engine_settings_fixings_mapper::map(entities); },
            lg(),
            "Reading latest engine settings fixings by id.");
    // The store keys the row by its party too, and a session sees the rows of
    // every party below its own, so one key can name several rows. The acting
    // party's row is the one the caller means when it holds one; otherwise
    // every row stays, so another party's row can still be found by its key,
    // ordered by party so the first row is the same on every read.
    const auto party = ctx.party_id();
    const auto acting = [&](const auto& row) {
        return row.party_id == *party;
    };
    if (party.has_value() && std::ranges::any_of(found, acting))
        std::erase_if(found, [&](const auto& row) { return !acting(row); });
    else
        std::sort(found.begin(), found.end(), [](const auto& a, const auto& b) {
            return a.party_id < b.party_id;
        });
    return found;
}


std::vector<domain::engine_settings_fixings>
engine_settings_fixings_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all engine settings fixings versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<engine_settings_fixings_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<engine_settings_fixings_entity, domain::engine_settings_fixings>(
        ctx,
        query,
        [](const auto& entities) { return engine_settings_fixings_mapper::map(entities); },
        lg(),
        "Reading all engine settings fixings versions by id.");
}

std::optional<domain::engine_settings_fixings> engine_settings_fixings_repository::read_at_version(
    context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading engine settings fixings at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<engine_settings_fixings_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities =
        execute_read_query<engine_settings_fixings_entity, domain::engine_settings_fixings>(
            ctx,
            query,
            [](const auto& entities) { return engine_settings_fixings_mapper::map(entities); },
            lg(),
            "Reading engine settings fixings at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::engine_settings_fixings>
engine_settings_fixings_repository::read_latest_by_engine_settings_id(
    context ctx,
    const std::string& engine_settings_id,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::engine_settings_fixings_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest engine settings fixings. engine_settings_id: "
                               << engine_settings_id << " offset: " << offset
                               << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<engine_settings_fixings_entity>> |
                       where("tenant_id"_c == tid && "engine_settings_id"_c == engine_settings_id &&
                             "valid_to"_c == max.value()) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<engine_settings_fixings_entity,
                                      domain::engine_settings_fixings>(
        ctx,
        query,
        engine_settings_fixings_list_order(order, {"id"}, false),
        engine_settings_fixings_filter_condition(filter),
        [](const auto& entities) { return engine_settings_fixings_mapper::map(entities); },
        lg(),
        "Reading latest engine settings fixings by engine_settings_id.");
}

std::uint32_t
engine_settings_fixings_repository::get_total_engine_settings_fixings_count_by_engine_settings_id(
    context ctx,
    const std::string& engine_settings_id,
    const std::optional<messaging::engine_settings_fixings_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active engine settings fixings count. engine_settings_id: "
        << engine_settings_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<engine_settings_fixings_entity>> |
                       where("tenant_id"_c == tid && "engine_settings_id"_c == engine_settings_id &&
                             "valid_to"_c == max.value());

    return execute_count_query<engine_settings_fixings_entity>(
        ctx,
        query,
        engine_settings_fixings_filter_condition(filter),
        lg(),
        "Counting engine settings fixings by engine_settings_id");
}


std::vector<domain::engine_settings_fixings>
engine_settings_fixings_repository::read_by_engine_settings_id_as_of(
    context ctx,
    const std::string& engine_settings_id,
    std::chrono::system_clock::time_point valid_from_bound,
    std::chrono::system_clock::time_point valid_to_bound) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading engine settings fixings as of window. engine_settings_id: "
        << engine_settings_id;

    const auto vf(
        make_timestamp(ores::platform::time::datetime::to_db_string(valid_from_bound), lg()));
    const auto vt(
        make_timestamp(ores::platform::time::datetime::to_db_string(valid_to_bound), lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<engine_settings_fixings_entity>> |
                       where("tenant_id"_c == tid && "engine_settings_id"_c == engine_settings_id &&
                             "valid_from"_c < vt.value() && "valid_to"_c > vf.value()) |
                       order_by("id"_c);

    return execute_read_query<engine_settings_fixings_entity, domain::engine_settings_fixings>(
        ctx,
        query,
        [](const auto& entities) { return engine_settings_fixings_mapper::map(entities); },
        lg(),
        "Reading engine settings fixings as of window by engine_settings_id.");
}

engine_settings_fixings_repository::remove_status engine_settings_fixings_repository::remove(
    context ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing engine settings fixings. " << "id: " << id;
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
    // The row checked above is one party's row of the key, and the removal
    // closes that row only, not every visible party's row of the same key.
    const auto party = boost::uuids::to_string(current.front().party_id);
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<engine_settings_fixings_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "party_id"_c == party &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing engine settings fixings from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    // Another party's row of the key stays open by design, so only a row of
    // the party removed counts.
    const auto removed_party = current.front().party_id;
    if (std::ranges::any_of(read_latest(ctx, id),
                            [&](const auto& row) { return row.party_id == removed_party; }))
        return remove_status::conflicting;
    return remove_status::removed;
}

void engine_settings_fixings_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::engine_settings_fixings> engine_settings_fixings_repository::read_latest(
    context ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::engine_settings_fixings_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest engine settings fixings with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<engine_settings_fixings_entity>> |
                       where("tenant_id"_c == tid) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<engine_settings_fixings_entity,
                                      domain::engine_settings_fixings>(
        ctx,
        query,
        engine_settings_fixings_list_order(order, {"id"}, false),
        narrowed(valid_at(as_of), engine_settings_fixings_filter_condition(filter)),
        [](const auto& entities) { return engine_settings_fixings_mapper::map(entities); },
        lg(),
        "Reading latest engine settings fixings with pagination.");
}

std::uint32_t engine_settings_fixings_repository::get_total_engine_settings_fixings_count(
    context ctx,
    const std::optional<messaging::engine_settings_fixings_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active engine settings fixings count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<engine_settings_fixings_entity>> | where("tenant_id"_c == tid);

    return execute_count_query<engine_settings_fixings_entity>(
        ctx,
        query,
        narrowed(valid_at(as_of), engine_settings_fixings_filter_condition(filter)),
        lg(),
        "Counting engine settings fixings");
}

std::vector<domain::engine_settings_fixings>
engine_settings_fixings_repository::read_latest(context ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<engine_settings_fixings_entity>> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    auto result =
        execute_read_query<engine_settings_fixings_entity, domain::engine_settings_fixings>(
            ctx,
            query,
            [](const auto& entities) { return engine_settings_fixings_mapper::map(entities); },
            lg(),
            "Reading latest engine settings fixings by ids.");
    return result;
}

void engine_settings_fixings_repository::remove(context ctx, const std::vector<std::string>& ids) {
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
    const auto query = sqlgen::delete_from<engine_settings_fixings_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing engine settings fixings.");
}


}
