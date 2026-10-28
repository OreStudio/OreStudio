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
#include "ores.reporting.core/repository/report_error_event_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.reporting.api/domain/report_error_event.hpp"
#include "ores.reporting.api/domain/report_error_event_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.api/messaging/report_error_event_protocol.hpp"
#include "ores.reporting.core/repository/report_error_event_entity.hpp"
#include "ores.reporting.core/repository/report_error_event_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/lexical_cast.hpp>
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

namespace ores::reporting::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string report_error_event_repository::sql() {
    return query_builder::insert_statement_sql<report_error_event_entity>(lg());
}

bool report_error_event_repository::is_sortable(std::string_view field) {
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
report_error_event_list_order(const ores::utility::domain::order& order,
                              std::initializer_list<std::string> default_columns,
                              bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"id"});
    if (!report_error_event_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of report error events cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition> report_error_event_filter_condition(
    const std::optional<messaging::report_error_events_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->report_instance_id)
        r.push_back(query_builder::equals(
            "report_instance_id", query_builder::filter_value(*filter->report_instance_id)));
    if (filter->severity)
        r.push_back(
            query_builder::equals("severity", query_builder::filter_value(*filter->severity)));
    if (filter->id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("id", std::move(values)));
    }
    if (filter->report_instance_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->report_instance_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("report_instance_id", std::move(values)));
    }
    if (filter->severity_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->severity_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("severity", std::move(values)));
    }
    return query_builder::all_of(std::move(r));
}

}

ores::utility::domain::precondition
report_error_event_repository::replace_claim(context ctx, const domain::report_error_event& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // by the read in apply_claim.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

domain::report_error_event
report_error_event_repository::apply_claim(context ctx,
                                           const domain::report_error_event& v,
                                           const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "report_error_event_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist &&
        !read_latest(ctx, boost::uuids::to_string(v.id)).empty())
        throw std::invalid_argument(
            "report_error_event_repository::write: a current row already exists");
    return t;
}

void report_error_event_repository::write(context ctx, const domain::report_error_event& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void report_error_event_repository::write(context ctx,
                                          const std::vector<domain::report_error_event>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void report_error_event_repository::write(context ctx,
                                          const domain::report_error_event& v,
                                          const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing report error event. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    query_builder::execute_write_op(ctx,
                                    query_builder::insert(report_error_event_mapper::map(t)),
                                    lg(),
                                    "Writing report error event to database.");
}

void report_error_event_repository::write(
    context ctx,
    const std::vector<domain::report_error_event>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing report error events. Count: " << v.size();
    std::vector<domain::report_error_event> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    query_builder::execute_write_op(ctx,
                                    query_builder::insert(report_error_event_mapper::map(batch)),
                                    lg(),
                                    "Writing report error events to database.");
}

std::vector<domain::report_error_event> report_error_event_repository::read_latest(context ctx) {
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<report_error_event_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::order_by("id"_c);

    return query_builder::execute_read_query<report_error_event_entity, domain::report_error_event>(
        ctx,
        query,
        [](const auto& entities) { return report_error_event_mapper::map(entities); },
        lg(),
        "Reading latest report error events");
}

std::vector<domain::report_error_event>
report_error_event_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest report error event. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<report_error_event_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id);

    return query_builder::execute_read_query<report_error_event_entity, domain::report_error_event>(
        ctx,
        query,
        [](const auto& entities) { return report_error_event_mapper::map(entities); },
        lg(),
        "Reading latest report error event by id.");
}


std::vector<domain::report_error_event>
report_error_event_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all report error event versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<report_error_event_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id) |
                       query_builder::order_by("id"_c);

    return query_builder::execute_read_query<report_error_event_entity, domain::report_error_event>(
        ctx,
        query,
        [](const auto& entities) { return report_error_event_mapper::map(entities); },
        lg(),
        "Reading all report error event versions by id.");
}


std::vector<domain::report_error_event>
report_error_event_repository::read_latest_by_report_instance_id(
    context ctx,
    const std::string& report_instance_id,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::report_error_events_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest report error events. report_instance_id: "
                               << report_instance_id << " offset: " << offset
                               << " limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<report_error_event_entity>> |
        query_builder::where("tenant_id"_c == tid && "report_instance_id"_c == report_instance_id) |
        query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<report_error_event_entity,
                                                     domain::report_error_event>(
        ctx,
        query,
        report_error_event_list_order(order, {"id"}, false),
        report_error_event_filter_condition(filter),
        [](const auto& entities) { return report_error_event_mapper::map(entities); },
        lg(),
        "Reading latest report error events by report_instance_id.");
}

std::uint32_t report_error_event_repository::get_total_error_count_by_report_instance_id(
    context ctx,
    const std::string& report_instance_id,
    const std::optional<messaging::report_error_events_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active report error events count. report_instance_id: "
        << report_instance_id;

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<report_error_event_entity>> |
        query_builder::where("tenant_id"_c == tid && "report_instance_id"_c == report_instance_id);

    return query_builder::execute_count_query<report_error_event_entity>(
        ctx,
        query,
        report_error_event_filter_condition(filter),
        lg(),
        "Counting report error events by report_instance_id");
}


report_error_event_repository::remove_status report_error_event_repository::remove(
    context ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing report error event. " << "id: " << id;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, id);
    if (current.empty())
        return remove_status::missing;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::delete_from<report_error_event_entity> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id);

    query_builder::execute_delete_query(
        ctx, query, lg(), "Removing report error event from database.");
    return remove_status::removed;
}

void report_error_event_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::report_error_event> report_error_event_repository::read_latest(
    context ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::report_error_events_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest report error events with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<report_error_event_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<report_error_event_entity,
                                                     domain::report_error_event>(
        ctx,
        query,
        report_error_event_list_order(order, {"id"}, false),
        report_error_event_filter_condition(filter),
        [](const auto& entities) { return report_error_event_mapper::map(entities); },
        lg(),
        "Reading latest report error events with pagination.");
}

std::uint32_t report_error_event_repository::get_total_error_count(
    context ctx, const std::optional<messaging::report_error_events_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active report error event count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<report_error_event_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<report_error_event_entity>(
        ctx,
        query,
        report_error_event_filter_condition(filter),
        lg(),
        "Counting report error events");
}

std::vector<domain::report_error_event>
report_error_event_repository::read_latest(context ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<report_error_event_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c.in(ids));
    auto result =
        query_builder::execute_read_query<report_error_event_entity, domain::report_error_event>(
            ctx,
            query,
            [](const auto& entities) { return report_error_event_mapper::map(entities); },
            lg(),
            "Reading latest report error events by ids.");
    return result;
}

void report_error_event_repository::remove(context ctx, const std::vector<std::string>& ids) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (ids.empty())
        return;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::delete_from<report_error_event_entity> |
                       query_builder::where("tenant_id"_c == tid && "id"_c.in(ids));
    query_builder::execute_delete_query(ctx, query, lg(), "Batch removing report error events.");
}


std::vector<messaging::report_error_count>
report_error_event_repository::read_counts_by_report_instance_ids(
    context ctx, const std::vector<boost::uuids::uuid>& report_instance_ids) {
    if (report_instance_ids.empty())
        return {};

    BOOST_LOG_SEV(lg(), debug) << "Counting the problems of " << report_instance_ids.size()
                               << " run(s).";

    // One bound array rather than one parameter per id, so a list of forty
    // runs is still one statement.
    std::string id_array("{");
    for (std::size_t i = 0; i < report_instance_ids.size(); ++i) {
        if (i > 0)
            id_array += ',';
        id_array += boost::uuids::to_string(report_instance_ids[i]);
    }
    id_array += '}';

    // The tenant and the policy on the table narrow the rows exactly as they do
    // for the generated reads; no party is stated here.
    const auto rows = query_builder::execute_parameterized_multi_column_query(
        ctx,
        "SELECT report_instance_id::text,"
        "       count(*) FILTER (WHERE severity = 'error')::text,"
        "       count(*) FILTER (WHERE severity = 'warning')::text"
        " FROM ores_reporting_report_error_events_tbl"
        " WHERE tenant_id = $1::uuid"
        "   AND report_instance_id = ANY($2::uuid[])"
        " GROUP BY report_instance_id",
        {ctx.tenant_id().to_string(), id_array},
        lg(),
        "Counting the problems of each run");

    std::vector<messaging::report_error_count> counts;
    counts.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() < 3 || !row[0] || !row[1] || !row[2])
            continue;
        messaging::report_error_count count;
        count.report_instance_id = boost::lexical_cast<boost::uuids::uuid>(*row[0]);
        count.errors = std::stoll(*row[1]);
        count.warnings = std::stoll(*row[2]);
        counts.push_back(std::move(count));
    }
    return counts;
}

}
