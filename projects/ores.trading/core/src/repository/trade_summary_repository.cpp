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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/range_bound.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
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


namespace {

/**
 * The filter as hand-written SQL states it: a clause and its text parameters.
 *
 * The generated condition binds binary parameters, which the text helper cannot
 * take, so the rail renders the same fields itself. Every field the entity
 * filter states is stated here, and the two move together: a field added to the
 * filter and not to this is a rail that counts rows the page does not show.
 */
struct rendered_filter {
    std::string sql;
    std::vector<std::optional<std::string>> params;
};

rendered_filter
render_trade_summary_filter(const std::optional<messaging::trade_summaries_filter>& filter) {
    rendered_filter out;
    if (!filter)
        return out;

    /*
     * The clause states its placeholder as =?= and it is numbered here, so a
     * field added in the middle moves every later placeholder with it. The
     * helper binds by number, so a statement that mixed the two spellings would
     * put the values against the wrong columns.
     */
    const auto add = [&out](const std::string& clause, std::optional<std::string> value) {
        std::string numbered;
        numbered.reserve(clause.size() + 4);
        for (const char c : clause) {
            if (c == '?')
                numbered += "$" + std::to_string(out.params.size() + 1);
            else
                numbered += c;
        }
        out.sql += out.sql.empty() ? numbered : " and " + numbered;
        out.params.push_back(std::move(value));
    };
    /*
     * An array literal quotes an element with double quotes, and only with
     * double quotes: a single-quoted element keeps its apostrophes and matches
     * nothing. A backslash and a double quote inside the element are escaped,
     * because both end the element otherwise.
     */
    const auto element = [](const std::string& v) {
        std::string out = "\"";
        for (const char c : v) {
            if (c == '\\' || c == '"')
                out += '\\';
            out += c;
        }
        out += '"';
        return out;
    };
    const auto text_array = [&element](const auto& values) {
        std::string literal = "{";
        bool first = true;
        for (const auto& v : values) {
            if (!first)
                literal += ",";
            literal += element(v);
            first = false;
        }
        literal += "}";
        return literal;
    };

    if (filter->row_kind)
        add("t.row_kind = ?", *filter->row_kind);
    if (filter->party_id)
        add("t.party_id = ?::uuid", boost::uuids::to_string(*filter->party_id));
    if (filter->counterparty_id)
        add(*filter->counterparty_id ? "t.counterparty_id = ?::uuid" : "t.counterparty_id is null",
            *filter->counterparty_id ?
                std::optional(boost::uuids::to_string(**filter->counterparty_id)) :
                std::nullopt);
    if (filter->book_id)
        add(*filter->book_id ? "t.book_id = ?::uuid" : "t.book_id is null",
            *filter->book_id ? std::optional(boost::uuids::to_string(**filter->book_id)) :
                               std::nullopt);
    if (filter->instrument_type)
        add("t.instrument_type = ?", *filter->instrument_type);
    if (filter->status)
        add(*filter->status ? "t.status = ?" : "t.status is null",
            *filter->status ? *filter->status : std::nullopt);
    if (filter->authorisation_status)
        add("t.authorisation_status = ?", *filter->authorisation_status);
    if (filter->trade_date)
        add(*filter->trade_date ? "t.trade_date = ?::date" : "t.trade_date is null",
            *filter->trade_date ? std::optional(ores::platform::time::datetime::to_iso8601_date(
                                      **filter->trade_date)) :
                                  std::nullopt);
    if (filter->structure_id)
        add(*filter->structure_id ? "t.structure_id = ?::uuid" : "t.structure_id is null",
            *filter->structure_id ? std::optional(boost::uuids::to_string(**filter->structure_id)) :
                                    std::nullopt);
    if (filter->is_leg)
        add("t.is_leg = ?::boolean", *filter->is_leg ? "true" : "false");

    if (filter->id_one_of) {
        std::vector<std::string> ids;
        for (const auto& id : *filter->id_one_of)
            ids.push_back(boost::uuids::to_string(id));
        add("t.id::text = any(?::text[])", text_array(ids));
    }
    if (filter->row_kind_one_of)
        add("t.row_kind = any(?::text[])", text_array(*filter->row_kind_one_of));
    if (filter->party_id_one_of) {
        std::vector<std::string> ids;
        for (const auto& id : *filter->party_id_one_of)
            ids.push_back(boost::uuids::to_string(id));
        add("t.party_id::text = any(?::text[])", text_array(ids));
    }
    if (filter->counterparty_id_one_of) {
        std::vector<std::string> ids;
        for (const auto& id : *filter->counterparty_id_one_of)
            ids.push_back(boost::uuids::to_string(id));
        add("t.counterparty_id::text = any(?::text[])", text_array(ids));
    }
    if (filter->book_id_one_of) {
        std::vector<std::string> ids;
        for (const auto& id : *filter->book_id_one_of)
            ids.push_back(boost::uuids::to_string(id));
        add("t.book_id::text = any(?::text[])", text_array(ids));
    }
    if (filter->instrument_type_one_of)
        add("t.instrument_type = any(?::text[])", text_array(*filter->instrument_type_one_of));
    if (filter->status_one_of)
        add("t.status = any(?::text[])", text_array(*filter->status_one_of));
    if (filter->authorisation_status_one_of)
        add("t.authorisation_status = any(?::text[])",
            text_array(*filter->authorisation_status_one_of));
    if (filter->trade_date_one_of) {
        std::vector<std::string> dates;
        for (const auto& d : *filter->trade_date_one_of)
            dates.push_back(ores::platform::time::datetime::to_iso8601_date(d));
        add("t.trade_date::text = any(?::text[])", text_array(dates));
    }
    if (filter->structure_id_one_of) {
        std::vector<std::string> ids;
        for (const auto& id : *filter->structure_id_one_of)
            ids.push_back(boost::uuids::to_string(id));
        add("t.structure_id::text = any(?::text[])", text_array(ids));
    }
    if (filter->is_leg_one_of) {
        std::vector<std::string> flags;
        for (const auto leg : *filter->is_leg_one_of)
            flags.push_back(leg ? "true" : "false");
        add("t.is_leg::text = any(?::text[])", text_array(flags));
    }
    if (filter->trade_date_from)
        add("t.trade_date >= ?::date", *filter->trade_date_from);
    if (filter->trade_date_to)
        add("t.trade_date <= ?::date", *filter->trade_date_to);
    if (filter->search && !filter->search->empty())
        add("t.search_text ilike ?", "%" + *filter->search + "%");
    return out;
}

}

std::vector<std::pair<std::string, std::uint64_t>> trade_summary_repository::count_blotter_rail(
    context ctx,
    const std::string& dimension,
    const std::optional<messaging::trade_summaries_filter>& filter) {
    const auto rendered = render_trade_summary_filter(filter);
    const auto tenant = ctx.tenant_id().to_string();
    const auto tenant_placeholder = "$" + std::to_string(rendered.params.size() + 1) + "::uuid";
    const std::string where = rendered.sql.empty() ?
                                  "t.tenant_id = " + tenant_placeholder :
                                  "(" + rendered.sql + ") and t.tenant_id = " + tenant_placeholder;

    auto params = rendered.params;
    params.push_back(tenant);

    std::string sql;
    if (dimension == "book") {
        sql = "select coalesce(t.book_id::text, '') as node_id, count(*)::text as n"
              " from ores_trading_trade_summaries_vw t where " +
              where + " group by 1";
    } else if (dimension == "counterparty") {
        sql = "select coalesce(t.counterparty_id::text, '') as node_id, count(*)::text as n"
              " from ores_trading_trade_summaries_vw t where " +
              where + " group by 1";
    } else if (dimension == "instrument_type") {
        sql = "select coalesce(t.instrument_type, '') as node_id, count(*)::text as n"
              " from ores_trading_trade_summaries_vw t where " +
              where + " group by 1";
    } else if (dimension == "status") {
        sql = "select coalesce(t.status, '') as node_id, count(*)::text as n"
              " from ores_trading_trade_summaries_vw t where " +
              where + " group by 1";
    } else if (dimension == "asset_class") {
        sql = "select coalesce(c.asset_class, '') as node_id, count(*)::text as n"
              " from ores_trading_trade_summaries_vw t"
              " left join ores_refdata_instrument_codes_tbl c"
              "   on c.name = t.instrument_type"
              "  and c.valid_to = ores_utility_infinity_timestamp_fn()"
              " where " +
              where + " group by 1";
    } else if (dimension == "portfolio") {
        sql = "with recursive book_counts as ("
              "  select t.book_id, count(*) as n from ores_trading_trade_summaries_vw t"
              "  where " +
              where +
              " and t.book_id is not null group by 1"
              "), ancestor as ("
              "  select b.id as book_id, b.parent_portfolio_id as portfolio_id"
              "  from ores_refdata_books_tbl b"
              "  where b.valid_to = ores_utility_infinity_timestamp_fn()"
              "  union"
              "  select a.book_id, p.parent_portfolio_id from ancestor a"
              "  join ores_refdata_portfolios_tbl p on p.id = a.portfolio_id"
              "   and p.valid_to = ores_utility_infinity_timestamp_fn()"
              ")"
              " select a.portfolio_id::text as node_id, sum(bc.n)::text as n"
              " from ancestor a join book_counts bc on bc.book_id = a.book_id"
              " where a.portfolio_id is not null group by 1";
    } else {
        throw std::invalid_argument("The blotter has no rail for the dimension '" + dimension +
                                    "'.");
    }

    const auto rows = ores::database::repository::execute_parameterized_multi_column_query(
        ctx, sql, params, lg(), "Counting the blotter's rail by " + dimension);

    std::vector<std::pair<std::string, std::uint64_t>> counts;
    counts.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() < 2 || !row[0] || !row[1])
            continue;
        counts.emplace_back(*row[0], static_cast<std::uint64_t>(std::stoull(*row[1])));
    }
    return counts;
}

namespace {

/** The separator between a key's sort value and its row id, which no value holds. */
constexpr char blotter_key_separator = '\x1f';

/**
 * The column a page is ordered by, and whether it descends.
 *
 * An empty field states the default order, which the stated direction reverses,
 * as the list does. A field the list cannot sort by is refused here too, so the
 * page and the list cannot disagree about what an order means.
 */
std::pair<std::string, bool> blotter_sort(trade_summary_repository& repo,
                                          const ores::utility::domain::order& order) {
    if (order.field.empty())
        return {"trade_date", !order.descending};
    if (!repo.is_sortable(order.field))
        throw std::invalid_argument("A blotter page cannot be ordered by " + order.field + ".");
    return {order.field, order.descending};
}

}

std::string trade_summary_repository::blotter_page_key(const domain::trade_summary& row,
                                                       const std::string& field) {
    std::string value;
    if (field == "trade_date" || field.empty()) {
        value = row.trade_date ? ores::platform::time::datetime::to_iso8601_date(*row.trade_date) :
                                 std::string{};
    } else if (field == "status") {
        value = row.status.value_or(std::string{});
    } else if (field == "authorisation_status") {
        value = row.authorisation_status;
    } else if (field == "instrument_type") {
        value = row.instrument_type;
    } else if (field == "row_kind") {
        value = row.row_kind;
    } else {
        throw std::invalid_argument("A blotter page cannot be ordered by " + field + ".");
    }
    return value + blotter_key_separator + boost::uuids::to_string(row.id);
}

std::vector<domain::trade_summary> trade_summary_repository::read_blotter_page(
    context ctx,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::string& after,
    const std::optional<messaging::trade_summaries_filter>& filter) {
    namespace qb = ores::database::query_builder;

    trade_summary_repository self;
    const auto [field, descending] = blotter_sort(self, order);

    /*
     * The key is the sort column and the row id, both as text: a date spelled
     * ISO, a uuid spelled canonically, and either compared the way the order
     * places them. A row that states no value for the column sorts as the empty
     * string, which is where =nulls last= would put it.
     */
    const auto sort_expression = "coalesce(" + field + "::text, '')";
    const auto id_expression = "id::text";

    auto where = qb::condition{"tenant_id = ?",
                               {ores::database::pq::text_param(ctx.tenant_id().to_string())}};
    if (const auto condition = trade_summary_filter_condition(filter))
        where = std::move(where) && std::move(*condition);

    if (!after.empty()) {
        const auto at = after.find(blotter_key_separator);
        if (at == std::string::npos)
            throw std::invalid_argument("The blotter page key is not a key this service wrote.");
        const auto value = after.substr(0, at);
        const auto id = after.substr(at + 1);
        where = std::move(where) && qb::condition{"(" + sort_expression + ", " + id_expression +
                                                      ") " + (descending ? "<" : ">") + " (?, ?)",
                                                  {ores::database::pq::text_param(value),
                                                   ores::database::pq::text_param(id)}};
    }

    auto sorting = qb::order{{sort_expression, descending}, {id_expression, descending}};
    auto statement =
        qb::detail::select_sql<trade_summary_entity>(where, sorting, limit, std::nullopt);
    /*
     * The statement's text varies with the key and the filter, so it runs
     * unnamed rather than as a prepared statement.
     */
    statement.variadic = true;
    const auto rows = qb::detail::select<trade_summary_entity>(ctx, statement);
    return trade_summary_mapper::map(rows);
}

}
