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
#include "ores.trading.core/repository/trade_summary_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/range_bound.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/trade_summary.hpp"
#include "ores.trading.api/domain/trade_summary_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/trade_summary_protocol.hpp"
#include "ores.trading.core/repository/trade_summary_entity.hpp"
#include "ores.trading.core/repository/trade_summary_mapper.hpp"
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

namespace ores::trading::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string trade_summary_repository::sql() {
    return query_builder::insert_statement_sql<trade_summary_entity>(lg());
}

bool trade_summary_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {
        "row_kind",
        "instrument_type",
        "status",
        "authorisation_status",
        "trade_date",
    };
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order trade_summary_list_order(const ores::utility::domain::order& order,
                                              std::initializer_list<std::string> default_columns,
                                              bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"id"});
    if (!trade_summary_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of trade summaries cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition>
trade_summary_filter_condition(const std::optional<messaging::trade_summaries_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->row_kind)
        r.push_back(
            query_builder::equals("row_kind", query_builder::filter_value(*filter->row_kind)));
    if (filter->party_id)
        r.push_back(
            query_builder::equals("party_id", query_builder::filter_value(*filter->party_id)));
    if (filter->counterparty_id)
        r.push_back(
            *filter->counterparty_id ?
                query_builder::equals("counterparty_id",
                                      query_builder::filter_value(**filter->counterparty_id)) :
                query_builder::is_null("counterparty_id"));
    if (filter->book_id)
        r.push_back(
            *filter->book_id ?
                query_builder::equals("book_id", query_builder::filter_value(**filter->book_id)) :
                query_builder::is_null("book_id"));
    if (filter->instrument_type)
        r.push_back(query_builder::equals("instrument_type",
                                          query_builder::filter_value(*filter->instrument_type)));
    if (filter->status)
        r.push_back(*filter->status ? query_builder::equals(
                                          "status", query_builder::filter_value(**filter->status)) :
                                      query_builder::is_null("status"));
    if (filter->authorisation_status)
        r.push_back(query_builder::equals(
            "authorisation_status", query_builder::filter_value(*filter->authorisation_status)));
    if (filter->trade_date)
        r.push_back(*filter->trade_date ?
                        query_builder::equals("trade_date",
                                              query_builder::filter_value(**filter->trade_date)) :
                        query_builder::is_null("trade_date"));
    if (filter->structure_id)
        r.push_back(*filter->structure_id ?
                        query_builder::equals("structure_id",
                                              query_builder::filter_value(**filter->structure_id)) :
                        query_builder::is_null("structure_id"));
    if (filter->is_leg)
        r.push_back(query_builder::equals("is_leg", query_builder::filter_value(*filter->is_leg)));
    if (filter->id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("id", std::move(values)));
    }
    if (filter->row_kind_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->row_kind_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("row_kind", std::move(values)));
    }
    if (filter->party_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->party_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("party_id", std::move(values)));
    }
    if (filter->counterparty_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->counterparty_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("counterparty_id", std::move(values)));
    }
    if (filter->book_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->book_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("book_id", std::move(values)));
    }
    if (filter->instrument_type_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->instrument_type_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("instrument_type", std::move(values)));
    }
    if (filter->status_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->status_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("status", std::move(values)));
    }
    if (filter->authorisation_status_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->authorisation_status_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("authorisation_status", std::move(values)));
    }
    if (filter->trade_date_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->trade_date_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("trade_date", std::move(values)));
    }
    if (filter->structure_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->structure_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("structure_id", std::move(values)));
    }
    if (filter->is_leg_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->is_leg_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("is_leg", std::move(values)));
    }
    if (filter->trade_date_from) {
        const auto bound = range_bound(*filter->trade_date_from, range_end::from, false);
        if (!bound)
            throw std::invalid_argument("The bound " + *filter->trade_date_from +
                                        " of trade_date_from is not a valid date or time.");
        r.push_back(query_builder::at_least("trade_date", query_builder::filter_value(*bound)));
    }
    if (filter->trade_date_to) {
        const auto bound = range_bound(*filter->trade_date_to, range_end::to, false);
        if (!bound)
            throw std::invalid_argument("The bound " + *filter->trade_date_to +
                                        " of trade_date_to is not a valid date or time.");
        r.push_back(query_builder::at_most("trade_date", query_builder::filter_value(*bound)));
    }
    if (filter->search && !filter->search->empty())
        r.push_back(query_builder::contains_any({"search_text"}, *filter->search));
    return query_builder::all_of(std::move(r));
}

}

ores::utility::domain::precondition
trade_summary_repository::replace_claim(context ctx, const domain::trade_summary& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // by the read in apply_claim.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

domain::trade_summary trade_summary_repository::apply_claim(
    context ctx, const domain::trade_summary& v, const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "trade_summary_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist &&
        !read_latest(ctx, boost::uuids::to_string(v.id)).empty())
        throw std::invalid_argument(
            "trade_summary_repository::write: a current row already exists");
    return t;
}

void trade_summary_repository::write(context ctx, const domain::trade_summary& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void trade_summary_repository::write(context ctx, const std::vector<domain::trade_summary>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void trade_summary_repository::write(context ctx,
                                     const domain::trade_summary& v,
                                     const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trade summary. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    query_builder::execute_write_op(ctx,
                                    query_builder::insert_or_replace(trade_summary_mapper::map(t)),
                                    lg(),
                                    "Writing trade summary to database.");
}

void trade_summary_repository::write(
    context ctx,
    const std::vector<domain::trade_summary>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trade summaries. Count: " << v.size();
    std::vector<domain::trade_summary> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    query_builder::execute_write_op(
        ctx,
        query_builder::insert_or_replace(trade_summary_mapper::map(batch)),
        lg(),
        "Writing trade summaries to database.");
}

std::vector<domain::trade_summary> trade_summary_repository::read_latest(context ctx) {
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_summary_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::order_by("id"_c);

    return query_builder::execute_read_query<trade_summary_entity, domain::trade_summary>(
        ctx,
        query,
        [](const auto& entities) { return trade_summary_mapper::map(entities); },
        lg(),
        "Reading latest trade summaries");
}

std::vector<domain::trade_summary> trade_summary_repository::read_latest(context ctx,
                                                                         const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest trade summary. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_summary_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id);

    return query_builder::execute_read_query<trade_summary_entity, domain::trade_summary>(
        ctx,
        query,
        [](const auto& entities) { return trade_summary_mapper::map(entities); },
        lg(),
        "Reading latest trade summary by id.");
}


std::vector<domain::trade_summary> trade_summary_repository::read_all(context ctx,
                                                                      const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all trade summary versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_summary_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id) |
                       query_builder::order_by("id"_c);

    return query_builder::execute_read_query<trade_summary_entity, domain::trade_summary>(
        ctx,
        query,
        [](const auto& entities) { return trade_summary_mapper::map(entities); },
        lg(),
        "Reading all trade summary versions by id.");
}


trade_summary_repository::remove_status trade_summary_repository::remove(
    context ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing trade summary. " << "id: " << id;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, id);
    if (current.empty())
        return remove_status::missing;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::delete_from<trade_summary_entity> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id);

    query_builder::execute_delete_query(ctx, query, lg(), "Removing trade summary from database.");
    return remove_status::removed;
}

void trade_summary_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::trade_summary> trade_summary_repository::read_latest(
    context ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::trade_summaries_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest trade summaries with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_summary_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<trade_summary_entity, domain::trade_summary>(
        ctx,
        query,
        trade_summary_list_order(order, {"id"}, false),
        trade_summary_filter_condition(filter),
        [](const auto& entities) { return trade_summary_mapper::map(entities); },
        lg(),
        "Reading latest trade summaries with pagination.");
}

std::uint32_t trade_summary_repository::get_total_trade_summary_count(
    context ctx, const std::optional<messaging::trade_summaries_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active trade summary count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_summary_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<trade_summary_entity>(
        ctx, query, trade_summary_filter_condition(filter), lg(), "Counting trade summaries");
}

std::vector<domain::trade_summary>
trade_summary_repository::read_latest(context ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_summary_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c.in(ids));
    auto result = query_builder::execute_read_query<trade_summary_entity, domain::trade_summary>(
        ctx,
        query,
        [](const auto& entities) { return trade_summary_mapper::map(entities); },
        lg(),
        "Reading latest trade summaries by ids.");
    return result;
}

void trade_summary_repository::remove(context ctx, const std::vector<std::string>& ids) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (ids.empty())
        return;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::delete_from<trade_summary_entity> |
                       query_builder::where("tenant_id"_c == tid && "id"_c.in(ids));
    query_builder::execute_delete_query(ctx, query, lg(), "Batch removing trade summaries.");
}


}
