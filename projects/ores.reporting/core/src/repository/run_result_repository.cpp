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
#include "ores.reporting.core/repository/run_result_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.reporting.api/domain/run_result.hpp"
#include "ores.reporting.api/domain/run_result_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.api/messaging/run_result_protocol.hpp"
#include "ores.reporting.core/repository/run_result_entity.hpp"
#include "ores.reporting.core/repository/run_result_mapper.hpp"
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

std::string run_result_repository::sql() {
    return query_builder::insert_statement_sql<run_result_entity>(lg());
}

bool run_result_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order run_result_list_order(const ores::utility::domain::order& order,
                                           std::initializer_list<std::string> default_columns,
                                           bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"id"});
    if (!run_result_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of run results cannot be ordered by " + order.field +
                                    ".");
    return query_builder::make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition>
run_result_filter_condition(const std::optional<messaging::run_results_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->party_id)
        r.push_back(
            query_builder::equals("party_id", query_builder::filter_value(*filter->party_id)));
    if (filter->report_instance_id)
        r.push_back(query_builder::equals(
            "report_instance_id", query_builder::filter_value(*filter->report_instance_id)));
    if (filter->report_definition_id)
        r.push_back(query_builder::equals(
            "report_definition_id", query_builder::filter_value(*filter->report_definition_id)));
    if (filter->subject_type)
        r.push_back(query_builder::equals("subject_type",
                                          query_builder::filter_value(*filter->subject_type)));
    if (filter->subject_id)
        r.push_back(
            query_builder::equals("subject_id", query_builder::filter_value(*filter->subject_id)));
    if (filter->measure)
        r.push_back(
            query_builder::equals("measure", query_builder::filter_value(*filter->measure)));
    if (filter->currency)
        r.push_back(
            query_builder::equals("currency", query_builder::filter_value(*filter->currency)));
    if (filter->counterparty_id)
        r.push_back(
            *filter->counterparty_id ?
                query_builder::equals("counterparty_id",
                                      query_builder::filter_value(**filter->counterparty_id)) :
                query_builder::is_null("counterparty_id"));
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
    if (filter->report_instance_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->report_instance_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("report_instance_id", std::move(values)));
    }
    if (filter->report_definition_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->report_definition_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("report_definition_id", std::move(values)));
    }
    if (filter->subject_type_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->subject_type_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("subject_type", std::move(values)));
    }
    if (filter->subject_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->subject_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("subject_id", std::move(values)));
    }
    if (filter->measure_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->measure_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("measure", std::move(values)));
    }
    if (filter->currency_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->currency_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("currency", std::move(values)));
    }
    if (filter->counterparty_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->counterparty_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("counterparty_id", std::move(values)));
    }
    return query_builder::all_of(std::move(r));
}

}

ores::utility::domain::precondition
run_result_repository::replace_claim(context ctx, const domain::run_result& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // by the read in apply_claim.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

domain::run_result run_result_repository::apply_claim(
    context ctx, const domain::run_result& v, const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "run_result_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist &&
        !read_latest(ctx, boost::uuids::to_string(v.id)).empty())
        throw std::invalid_argument("run_result_repository::write: a current row already exists");
    return t;
}

void run_result_repository::write(context ctx, const domain::run_result& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void run_result_repository::write(context ctx, const std::vector<domain::run_result>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void run_result_repository::write(context ctx,
                                  const domain::run_result& v,
                                  const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing run result. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    query_builder::execute_write_op(ctx,
                                    query_builder::insert_or_replace(run_result_mapper::map(t)),
                                    lg(),
                                    "Writing run result to database.");
}

void run_result_repository::write(context ctx,
                                  const std::vector<domain::run_result>& v,
                                  const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing run results. Count: " << v.size();
    std::vector<domain::run_result> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    query_builder::execute_write_op(ctx,
                                    query_builder::insert_or_replace(run_result_mapper::map(batch)),
                                    lg(),
                                    "Writing run results to database.");
}

std::vector<domain::run_result> run_result_repository::read_latest(context ctx) {
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<run_result_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::order_by("id"_c);

    return query_builder::execute_read_query<run_result_entity, domain::run_result>(
        ctx,
        query,
        [](const auto& entities) { return run_result_mapper::map(entities); },
        lg(),
        "Reading latest run results");
}

std::vector<domain::run_result> run_result_repository::read_latest(context ctx,
                                                                   const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest run result. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<run_result_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id);

    return query_builder::execute_read_query<run_result_entity, domain::run_result>(
        ctx,
        query,
        [](const auto& entities) { return run_result_mapper::map(entities); },
        lg(),
        "Reading latest run result by id.");
}


std::vector<domain::run_result> run_result_repository::read_all(context ctx,
                                                                const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all run result versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<run_result_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id) |
                       query_builder::order_by("id"_c);

    return query_builder::execute_read_query<run_result_entity, domain::run_result>(
        ctx,
        query,
        [](const auto& entities) { return run_result_mapper::map(entities); },
        lg(),
        "Reading all run result versions by id.");
}


run_result_repository::remove_status run_result_repository::remove(
    context ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing run result. " << "id: " << id;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, id);
    if (current.empty())
        return remove_status::missing;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::delete_from<run_result_entity> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id);

    query_builder::execute_delete_query(ctx, query, lg(), "Removing run result from database.");
    return remove_status::removed;
}

void run_result_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::run_result>
run_result_repository::read_latest(context ctx,
                                   std::uint32_t offset,
                                   std::uint32_t limit,
                                   const ores::utility::domain::order& order,
                                   const std::optional<messaging::run_results_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest run results with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<run_result_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<run_result_entity, domain::run_result>(
        ctx,
        query,
        run_result_list_order(order, {"id"}, false),
        run_result_filter_condition(filter),
        [](const auto& entities) { return run_result_mapper::map(entities); },
        lg(),
        "Reading latest run results with pagination.");
}

std::uint32_t run_result_repository::get_total_result_count(
    context ctx, const std::optional<messaging::run_results_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active run result count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<run_result_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<run_result_entity>(
        ctx, query, run_result_filter_condition(filter), lg(), "Counting run results");
}

std::vector<domain::run_result>
run_result_repository::read_latest(context ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<run_result_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c.in(ids));
    auto result = query_builder::execute_read_query<run_result_entity, domain::run_result>(
        ctx,
        query,
        [](const auto& entities) { return run_result_mapper::map(entities); },
        lg(),
        "Reading latest run results by ids.");
    return result;
}

void run_result_repository::remove(context ctx, const std::vector<std::string>& ids) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (ids.empty())
        return;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::delete_from<run_result_entity> |
                       query_builder::where("tenant_id"_c == tid && "id"_c.in(ids));
    query_builder::execute_delete_query(ctx, query, lg(), "Batch removing run results.");
}


}
