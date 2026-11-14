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
#include "ores.reporting.core/repository/report_text_value_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.reporting.api/domain/report_text_value.hpp"
#include "ores.reporting.api/domain/report_text_value_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.api/messaging/report_text_value_protocol.hpp"
#include "ores.reporting.core/repository/report_text_value_entity.hpp"
#include "ores.reporting.core/repository/report_text_value_mapper.hpp"
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

namespace ores::reporting::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string report_text_value_repository::sql() {
    return query_builder::insert_statement_sql<report_text_value_entity>(lg());
}

bool report_text_value_repository::is_sortable(std::string_view field) {
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
report_text_value_list_order(const ores::utility::domain::order& order,
                             std::initializer_list<std::string> default_columns,
                             bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"id"});
    if (!report_text_value_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of report text values cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition> report_text_value_filter_condition(
    const std::optional<messaging::report_text_values_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->report_instance_id)
        r.push_back(query_builder::equals(
            "report_instance_id", query_builder::filter_value(*filter->report_instance_id)));
    if (filter->family)
        r.push_back(query_builder::equals("family", query_builder::filter_value(*filter->family)));
    if (filter->subject_id)
        r.push_back(
            query_builder::equals("subject_id", query_builder::filter_value(*filter->subject_id)));
    if (filter->name)
        r.push_back(query_builder::equals("name", query_builder::filter_value(*filter->name)));
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
    if (filter->family_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->family_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("family", std::move(values)));
    }
    if (filter->subject_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->subject_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("subject_id", std::move(values)));
    }
    if (filter->name_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->name_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("name", std::move(values)));
    }
    return query_builder::all_of(std::move(r));
}

}

ores::utility::domain::precondition
report_text_value_repository::replace_claim(const context& ctx,
                                            const domain::report_text_value& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // by the read in apply_claim.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

domain::report_text_value
report_text_value_repository::apply_claim(const context& ctx,
                                          const domain::report_text_value& v,
                                          const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "report_text_value_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist &&
        !read_latest(ctx, boost::uuids::to_string(v.id)).empty())
        throw std::invalid_argument(
            "report_text_value_repository::write: a current row already exists");
    return t;
}

void report_text_value_repository::write(const context& ctx, const domain::report_text_value& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void report_text_value_repository::insert(const context& ctx, const domain::report_text_value& v) {
    BOOST_LOG_SEV(lg(), debug) << "Inserting report text value. " << "id: " << v.id;
    query_builder::execute_write_query(
        ctx, report_text_value_mapper::map(v), lg(), "Inserting report text value into database.");
}

void report_text_value_repository::insert(const context& ctx,
                                          const std::vector<domain::report_text_value>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Inserting report text values. Count: " << v.size();
    query_builder::execute_write_query(
        ctx, report_text_value_mapper::map(v), lg(), "Inserting report text values into database.");
}

void report_text_value_repository::write(const context& ctx,
                                         const std::vector<domain::report_text_value>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void report_text_value_repository::write(const context& ctx,
                                         const domain::report_text_value& v,
                                         const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing report text value. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    query_builder::execute_write_query(
        ctx, report_text_value_mapper::map(t), lg(), "Writing report text value to database.");
}

void report_text_value_repository::write(
    const context& ctx,
    const std::vector<domain::report_text_value>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing report text values. Count: " << v.size();
    std::vector<domain::report_text_value> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    query_builder::execute_write_query(
        ctx, report_text_value_mapper::map(batch), lg(), "Writing report text values to database.");
}

std::vector<domain::report_text_value>
report_text_value_repository::read_latest(const context& ctx) {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<report_text_value_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("id"_c);

    return query_builder::execute_read_query<report_text_value_entity, domain::report_text_value>(
        ctx,
        query,
        [](const auto& entities) { return report_text_value_mapper::map(entities); },
        lg(),
        "Reading latest report text values");
}

std::vector<domain::report_text_value>
report_text_value_repository::read_latest(const context& ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest report text value. " << "id: " << id;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<report_text_value_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max);

    return query_builder::execute_read_query<report_text_value_entity, domain::report_text_value>(
        ctx,
        query,
        [](const auto& entities) { return report_text_value_mapper::map(entities); },
        lg(),
        "Reading latest report text value by id.");
}


std::vector<domain::report_text_value>
report_text_value_repository::read_all(const context& ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all report text value versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<report_text_value_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id) |
                       query_builder::order_by("valid_from"_c.desc());

    return query_builder::execute_read_query<report_text_value_entity, domain::report_text_value>(
        ctx,
        query,
        [](const auto& entities) { return report_text_value_mapper::map(entities); },
        lg(),
        "Reading all report text value versions by id.");
}


report_text_value_repository::remove_status report_text_value_repository::remove(
    const context& ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing report text value. " << "id: " << id;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, id);
    if (current.empty())
        return remove_status::missing;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<report_text_value_entity> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max);

    query_builder::execute_delete_query(
        ctx, query, lg(), "Removing report text value from database.");
    return remove_status::removed;
}

void report_text_value_repository::remove(const context& ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::report_text_value> report_text_value_repository::read_latest(
    const context& ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::report_text_values_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest report text values with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<report_text_value_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<report_text_value_entity,
                                                     domain::report_text_value>(
        ctx,
        query,
        report_text_value_list_order(order, {"id"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of),
                                report_text_value_filter_condition(filter)),
        [](const auto& entities) { return report_text_value_mapper::map(entities); },
        lg(),
        "Reading latest report text values with pagination.");
}

std::uint32_t report_text_value_repository::get_total_value_count(
    const context& ctx,
    const std::optional<messaging::report_text_values_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active report text value count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<report_text_value_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<report_text_value_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of),
                                report_text_value_filter_condition(filter)),
        lg(),
        "Counting report text values");
}

std::vector<domain::report_text_value>
report_text_value_repository::read_latest(const context& ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<report_text_value_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max);
    auto result =
        query_builder::execute_read_query<report_text_value_entity, domain::report_text_value>(
            ctx,
            query,
            [](const auto& entities) { return report_text_value_mapper::map(entities); },
            lg(),
            "Reading latest report text values by ids.");
    return result;
}

void report_text_value_repository::remove(const context& ctx, const std::vector<std::string>& ids) {
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
        query_builder::delete_from<report_text_value_entity> |
        query_builder::where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max);
    query_builder::execute_delete_query(ctx, query, lg(), "Batch removing report text values.");
}


}
