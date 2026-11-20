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
#include "ores.reporting.core/repository/report_cashflow_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.reporting.api/domain/report_cashflow.hpp"
#include "ores.reporting.api/domain/report_cashflow_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.api/messaging/report_cashflow_protocol.hpp"
#include "ores.reporting.core/repository/report_cashflow_entity.hpp"
#include "ores.reporting.core/repository/report_cashflow_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
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

namespace ores::reporting::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string report_cashflow_repository::sql() {
    return query_builder::insert_statement_sql<report_cashflow_entity>(lg());
}

bool report_cashflow_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order report_cashflow_list_order(const ores::utility::domain::order& order,
                                                std::initializer_list<std::string> default_columns,
                                                bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"id"});
    if (!report_cashflow_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of report cashflows cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition>
report_cashflow_filter_condition(const std::optional<messaging::report_cashflows_filter>& filter) {
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
    if (filter->measure)
        r.push_back(
            query_builder::equals("measure", query_builder::filter_value(*filter->measure)));
    if (filter->part)
        r.push_back(query_builder::equals("part", query_builder::filter_value(*filter->part)));
    if (filter->flow_type)
        r.push_back(
            query_builder::equals("flow_type", query_builder::filter_value(*filter->flow_type)));
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
    if (filter->measure_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->measure_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("measure", std::move(values)));
    }
    if (filter->part_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->part_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("part", std::move(values)));
    }
    if (filter->flow_type_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->flow_type_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("flow_type", std::move(values)));
    }
    return query_builder::all_of(std::move(r));
}

}

namespace {

/**
 * @brief Whether a claim is checked against the row its key names now.
 */
bool report_cashflow_claim_reads_current(const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    // No version to state, so only a create needs to know whether a row exists.
    return claim.kind == precondition_kind::must_not_exist;
}

using report_cashflow_rows_by_key =
    std::map<std::tuple<std::string>, std::vector<domain::report_cashflow>>;

/**
 * @brief The current rows the objects name, by the text form of their key.
 *
 * One batch read for the set, so a batch checks its claims against one read
 * rather than one read per row.
 */
report_cashflow_rows_by_key
report_cashflow_current_rows(report_cashflow_repository& repo,
                             const ores::database::context& ctx,
                             const std::vector<domain::report_cashflow>& of) {
    std::vector<std::string> ids;
    ids.reserve(of.size());
    for (const auto& row : of) {
        ids.push_back(boost::uuids::to_string(row.id));
    }
    report_cashflow_rows_by_key current;
    for (auto& item : repo.read_latest(ctx, ids))
        current[std::make_tuple(boost::uuids::to_string(item.id))].push_back(std::move(item));
    return current;
}

/**
 * @brief The current rows that the key of an object names, or none.
 */
const std::vector<domain::report_cashflow>&
report_cashflow_rows_of(const report_cashflow_rows_by_key& current,
                        const domain::report_cashflow& item) {
    static const std::vector<domain::report_cashflow> none;
    const auto found = current.find(std::make_tuple(boost::uuids::to_string(item.id)));
    return found == current.end() ? none : found->second;
}

/**
 * @brief The claim a replace makes: the version the row carries now, or
 * that no row exists yet.
 */
ores::utility::domain::precondition
report_cashflow_replace_claim(const std::vector<domain::report_cashflow>& current) {
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
domain::report_cashflow
report_cashflow_apply_claim(const domain::report_cashflow& v,
                            const ores::utility::domain::precondition& claim,
                            const std::vector<domain::report_cashflow>& current) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "report_cashflow_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist && !current.empty())
        throw std::invalid_argument(
            "report_cashflow_repository::write: a current row already exists");
    return t;
}

}

void report_cashflow_repository::write(const context& ctx, const domain::report_cashflow& v) {
    write(ctx, v, report_cashflow_replace_claim(read_latest(ctx, boost::uuids::to_string(v.id))));
}

void report_cashflow_repository::insert(const context& ctx, const domain::report_cashflow& v) {
    ORES_LOG_SEV(lg(), debug) << "Inserting report cashflow. " << "id: " << v.id;
    query_builder::execute_write_query(
        ctx, report_cashflow_mapper::map(v), lg(), "Inserting report cashflow into database.");
}

void report_cashflow_repository::insert(const context& ctx,
                                        const std::vector<domain::report_cashflow>& v) {
    ORES_LOG_SEV(lg(), debug) << "Inserting report cashflows. Count: " << v.size();
    query_builder::execute_write_query(
        ctx, report_cashflow_mapper::map(v), lg(), "Inserting report cashflows into database.");
}

void report_cashflow_repository::write(const context& ctx,
                                       const std::vector<domain::report_cashflow>& v) {
    const auto current = report_cashflow_current_rows(*this, ctx, v);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(report_cashflow_replace_claim(report_cashflow_rows_of(current, item)));
    write(ctx, v, claims);
}

void report_cashflow_repository::write(const context& ctx,
                                       const domain::report_cashflow& v,
                                       const ores::utility::domain::precondition& claim) {
    ORES_LOG_SEV(lg(), debug) << "Writing report cashflow. " << "id: " << v.id;
    const auto t = report_cashflow_apply_claim(v,
                                               claim,
                                               report_cashflow_claim_reads_current(claim) ?
                                                   read_latest(ctx, boost::uuids::to_string(v.id)) :
                                                   std::vector<domain::report_cashflow>{});
    query_builder::execute_write_query(
        ctx, report_cashflow_mapper::map(t), lg(), "Writing report cashflow to database.");
}

void report_cashflow_repository::write(
    const context& ctx,
    const std::vector<domain::report_cashflow>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    ORES_LOG_SEV(lg(), debug) << "Writing report cashflows. Count: " << v.size();
    if (claims.size() != v.size())
        throw std::invalid_argument(
            "report_cashflow_repository::write: one claim is needed for each row");
    std::vector<domain::report_cashflow> to_read;
    for (std::size_t i = 0; i < v.size(); ++i) {
        if (report_cashflow_claim_reads_current(claims[i]))
            to_read.push_back(v[i]);
    }
    const auto current = report_cashflow_current_rows(*this, ctx, to_read);
    std::vector<domain::report_cashflow> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(
            report_cashflow_apply_claim(v[i], claims[i], report_cashflow_rows_of(current, v[i])));
    query_builder::execute_write_query(
        ctx, report_cashflow_mapper::map(batch), lg(), "Writing report cashflows to database.");
}

std::vector<domain::report_cashflow> report_cashflow_repository::read_latest(const context& ctx) {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<report_cashflow_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("id"_c);

    return query_builder::execute_read_query<report_cashflow_entity, domain::report_cashflow>(
        ctx,
        query,
        [](const auto& entities) { return report_cashflow_mapper::map(entities); },
        lg(),
        "Reading latest report cashflows");
}

std::vector<domain::report_cashflow>
report_cashflow_repository::read_latest(const context& ctx, const std::string& id) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest report cashflow. " << "id: " << id;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<report_cashflow_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max);

    return query_builder::execute_read_query<report_cashflow_entity, domain::report_cashflow>(
        ctx,
        query,
        [](const auto& entities) { return report_cashflow_mapper::map(entities); },
        lg(),
        "Reading latest report cashflow by id.");
}


std::vector<domain::report_cashflow> report_cashflow_repository::read_all(const context& ctx,
                                                                          const std::string& id) {
    ORES_LOG_SEV(lg(), debug) << "Reading all report cashflow versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<report_cashflow_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id) |
                       query_builder::order_by("valid_from"_c.desc());

    return query_builder::execute_read_query<report_cashflow_entity, domain::report_cashflow>(
        ctx,
        query,
        [](const auto& entities) { return report_cashflow_mapper::map(entities); },
        lg(),
        "Reading all report cashflow versions by id.");
}


report_cashflow_repository::remove_status report_cashflow_repository::remove(
    const context& ctx, const std::string& id, std::optional<std::uint32_t> version) {
    ORES_LOG_SEV(lg(), debug) << "Removing report cashflow. " << "id: " << id;
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
        query_builder::delete_from<report_cashflow_entity> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max);

    query_builder::execute_delete_query(
        ctx, query, lg(), "Removing report cashflow from database.");
    return remove_status::removed;
}

void report_cashflow_repository::remove(const context& ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::report_cashflow> report_cashflow_repository::read_latest(
    const context& ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::report_cashflows_filter>& filter,
    const std::optional<std::string>& as_of) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest report cashflows with offset: " << offset
                              << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<report_cashflow_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<report_cashflow_entity,
                                                     domain::report_cashflow>(
        ctx,
        query,
        report_cashflow_list_order(order, {"id"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of),
                                report_cashflow_filter_condition(filter)),
        [](const auto& entities) { return report_cashflow_mapper::map(entities); },
        lg(),
        "Reading latest report cashflows with pagination.");
}

std::uint32_t report_cashflow_repository::get_total_cashflow_count(
    const context& ctx,
    const std::optional<messaging::report_cashflows_filter>& filter,
    const std::optional<std::string>& as_of) {
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active report cashflow count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<report_cashflow_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<report_cashflow_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of),
                                report_cashflow_filter_condition(filter)),
        lg(),
        "Counting report cashflows");
}

std::vector<domain::report_cashflow>
report_cashflow_repository::read_latest(const context& ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    // Every key is a bind and the store's bind count is finite, so a larger
    // key set is read one chunk at a time.
    constexpr std::size_t chunk_size = 1000;
    if (ids.size() > chunk_size) {
        std::vector<domain::report_cashflow> all;
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
        query_builder::read<std::vector<report_cashflow_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max);
    auto result =
        query_builder::execute_read_query<report_cashflow_entity, domain::report_cashflow>(
            ctx,
            query,
            [](const auto& entities) { return report_cashflow_mapper::map(entities); },
            lg(),
            "Reading latest report cashflows by ids.");
    return result;
}

void report_cashflow_repository::remove(const context& ctx, const std::vector<std::string>& ids) {
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
        query_builder::delete_from<report_cashflow_entity> |
        query_builder::where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max);
    query_builder::execute_delete_query(ctx, query, lg(), "Batch removing report cashflows.");
}


}
