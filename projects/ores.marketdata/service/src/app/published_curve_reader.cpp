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
#include "ores.marketdata.service/app/published_curve_reader.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/datum/schema.hpp"
#include "ores.marketdata.api/datum/value.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::marketdata::service::app {

using namespace ores::logging;
using ores::database::repository::execute_parameterized_multi_column_query;
using ores::platform::time::datetime;

namespace {

inline std::string_view published_curve_reader_logger_name =
    "ores.marketdata.service.published_curve_reader";

[[nodiscard]] auto& published_curve_reader_lg() {
    static auto instance = make_logger(published_curve_reader_logger_name);
    return instance;
}

using row = std::vector<std::optional<std::string>>;

/**
 * @brief The columns both reads select, in the order they select them.
 *
 * The two reads select the same set facts and differ in their filters, so the
 * indices below name one projection and not two.
 */
constexpr std::size_t col_publish_id = 0;
constexpr std::size_t col_request_id = 1;
constexpr std::size_t col_party_id = 2;
constexpr std::size_t col_series_id = 3;
constexpr std::size_t col_config_id = 4;
constexpr std::size_t col_series_uri = 5;
constexpr std::size_t col_as_of = 6;
constexpr std::size_t col_published_at = 7;
constexpr std::size_t col_published_by = 8;
constexpr std::size_t col_withdrawn_at = 9;
constexpr std::size_t col_withdrawn_by = 10;
constexpr std::size_t col_withdrawn_reason = 11;
constexpr std::size_t col_withdrawal_request = 12;
constexpr std::size_t col_superseded_at = 13;
constexpr std::size_t col_superseded_by_request = 14;
constexpr std::size_t col_superseded_as_of = 15;
constexpr std::size_t col_point_count = 16;

/**
 * @brief The publish record's facts, shared by both reads.
 *
 * The series is joined rather than read per row: its key is the only name a
 * curve has, and the point count is a lateral count over the observations the
 * publish wrote, so neither is a second round trip.
 */
constexpr const char* set_columns =
    " cp.id::text, cp.request_id::text, cp.party_id::text, cp.output_series_id::text,"
    " cp.bootstrap_config_id::text, coalesce(s.oresmd_uri, ''),"
    " cp.as_of::text, cp.published_at::text, cp.published_by,"
    " cp.withdrawn_at::text, cp.withdrawn_by, cp.withdrawn_reason_code,"
    " coalesce(cp.withdrawal_request_id::text, ''),"
    " cp.superseded_at::text, coalesce(cp.superseded_by_request_id::text, ''),"
    " cp.superseded_as_of::text,"
    " (select count(*)"
    "    from ores_marketdata_market_observations_tbl o"
    "   where o.tenant_id = cp.tenant_id"
    "     and o.series_id = cp.output_series_id"
    "     and o.observation_datetime = cp.as_of"
    "     and o.valid_to = ores_utility_infinity_timestamp_fn())::text";

constexpr const char* set_from = " from ores_marketdata_curve_publishes_tbl cp"
                                 " left join ores_marketdata_market_series_tbl s"
                                 "   on s.tenant_id = cp.tenant_id"
                                 "  and s.id = cp.output_series_id"
                                 "  and s.valid_to = ores_utility_infinity_timestamp_fn()";

/// A column's text, or the empty string when the store holds null there.
std::string text_at(const row& r, std::size_t column) {
    if (column >= r.size() || !r[column])
        return {};
    return *r[column];
}

/// An instant's column, or null when the store holds null there.
std::optional<std::chrono::system_clock::time_point> instant_at(const row& r, std::size_t column) {
    const auto value = text_at(r, column);
    if (value.empty())
        return std::nullopt;
    return datetime::from_db_string(value);
}

std::chrono::system_clock::time_point required_instant_at(const row& r, std::size_t column) {
    const auto value = instant_at(r, column);
    if (!value)
        throw std::runtime_error("the store holds no instant where one is required");
    return *value;
}

std::uint32_t count_at(const row& r, std::size_t column) {
    const auto value = text_at(r, column);
    return value.empty() ? 0U : static_cast<std::uint32_t>(std::stoul(value));
}

/// The text of a statement's parameter, written as a PostgreSQL literal list.
std::string braced(const std::vector<std::string>& values) {
    std::string out{"{"};
    for (std::size_t i = 0; i < values.size(); ++i) {
        if (i > 0)
            out += ',';
        out += values[i];
    }
    out += '}';
    return out;
}

/**
 * @brief The curve's name: the curve id its output series carries.
 *
 * A bootstrap configuration carries no code -- it holds a currency, an index
 * tenor and a convention -- so the only name a published curve has is the one
 * its output series was written under. The reader takes the curve id out of the
 * series' own key rather than inventing a name, and falls back to the whole key
 * when the series holds no curve id.
 */
std::string curve_name_of(const std::string& series_uri) {
    if (series_uri.empty())
        return {};
    const auto parsed = datum::oresmd_uri_codec::read(series_uri);
    if (!parsed)
        return series_uri;
    for (const auto& field : parsed->fields())
        if (datum::name_of(field.name) == "curve_id")
            return datum::text_of(field.held);
    return series_uri;
}

/**
 * @brief One stored point as a reader draws it: where it stands and what its
 * value denotes.
 *
 * The pillar is the datum's own coordinate, so a reader reads the term the
 * publish wrote rather than one derived from the pillar's position in the set. A
 * datum the reader cannot parse is still a point: its pillar is left empty and
 * its key travels, so the screen shows the value and names the point it cannot.
 */
messaging::published_curve_point point_of(const std::string& observation_datetime,
                                          const std::string& value,
                                          const std::string& oresmd_uri) {
    messaging::published_curve_point point;
    point.value = value;
    point.oresmd_uri = oresmd_uri;
    point.observation_datetime = datetime::from_db_string(observation_datetime);

    const auto parsed = datum::oresmd_uri_codec::read(oresmd_uri);
    if (!parsed)
        return point;
    point.value_kind = datum::oresmd_uri_codec::instrument_spelling(parsed->type());
    for (const auto& field : parsed->fields()) {
        if (datum::name_of(field.name) != "term")
            continue;
        point.pillar = datum::text_of(field.held);
        break;
    }
    return point;
}

/**
 * @brief The state the record's own facts name.
 *
 * The state is not a stored column: a set ends once, and the facts that end it
 * -- an instant with an account and a reason, or a link to the successor -- are
 * what it is read from. Storing the state beside them would be a second source
 * of truth for one fact.
 */
std::string state_of(const row& r) {
    if (instant_at(r, col_withdrawn_at))
        return "withdrawn";
    if (instant_at(r, col_superseded_at))
        return "superseded";
    if (instant_at(r, col_published_at))
        return "published";
    return "candidate";
}

/// One set from a row, without its party name and without its points.
messaging::published_curve_set set_of(const row& r) {
    messaging::published_curve_set set;
    set.publish_id = text_at(r, col_publish_id);
    set.request_id = text_at(r, col_request_id);
    set.party_id = text_at(r, col_party_id);
    set.series_id = text_at(r, col_series_id);
    set.bootstrap_config_id = text_at(r, col_config_id);
    set.series_uri = text_at(r, col_series_uri);
    set.curve = curve_name_of(set.series_uri);
    set.as_of = required_instant_at(r, col_as_of);
    set.published_at = instant_at(r, col_published_at);
    set.published_by = text_at(r, col_published_by);
    set.state_code = state_of(r);
    set.point_count = count_at(r, col_point_count);
    set.withdrawn_at = instant_at(r, col_withdrawn_at);
    set.withdrawn_by = text_at(r, col_withdrawn_by);
    set.withdrawn_reason_code = text_at(r, col_withdrawn_reason);
    set.withdrawal_request_id = text_at(r, col_withdrawal_request);
    set.superseded_at = instant_at(r, col_superseded_at);
    set.superseded_by_request_id = text_at(r, col_superseded_by_request);
    set.superseded_as_of = instant_at(r, col_superseded_as_of);
    return set;
}

/**
 * @brief Names each party the sets belong to, by its short code.
 *
 * A party is reference data, so it is read through the party repository and not
 * joined here. The rows are read once for the whole page rather than once per
 * set, because a page holds as many sets as the screen draws.
 */
void name_parties(const ores::database::context& ctx,
                  std::vector<messaging::published_curve_set>& sets) {
    std::vector<std::string> ids;
    for (const auto& set : sets)
        if (!set.party_id.empty() && std::find(ids.begin(), ids.end(), set.party_id) == ids.end())
            ids.push_back(set.party_id);

    std::map<std::string, std::string> code_of;
    if (!ids.empty()) {
        ores::refdata::repository::party_repository repo;
        for (const auto& party : repo.read_latest(ctx, ids))
            code_of.emplace(boost::uuids::to_string(party.id), party.short_code);
    }

    for (auto& set : sets) {
        const auto found = code_of.find(set.party_id);
        if (found != code_of.end())
            set.party = found->second;
    }
}

}

messaging::list_published_curves_response
list_published_curves(const ores::database::context& ctx,
                      const messaging::list_published_curves_request& request) {
    messaging::list_published_curves_response response;

    const auto tenant = ctx.tenant_id().to_string();
    // A parameter that is absent binds as SQL NULL: an empty text cannot be
    // read as a uuid, and the statement compares the parameter with is null.
    const std::optional<std::string> party =
        request.party_id.empty() ? std::nullopt : std::optional{request.party_id};
    const auto request_ids = braced(request.request_id_one_of);

    const auto filter =
        " where cp.tenant_id = $1::uuid"
        "   and cp.valid_to = ores_utility_infinity_timestamp_fn()"
        "   and ($2::uuid is null or cp.party_id = $2::uuid)"
        "   and ($3 = '' or coalesce(s.oresmd_uri, '') ilike '%' || $3 || '%')"
        "   and ($4::boolean or cp.published_at is not null)"
        "   and ($5::boolean or (cp.withdrawn_at is null and cp.superseded_at is null))"
        "   and (cardinality($6::uuid[]) = 0 or cp.request_id = any($6::uuid[]))";
    const std::vector<std::optional<std::string>> filter_params{
        tenant,
        party,
        request.search,
        request.include_candidates ? "true" : "false",
        request.include_ended ? "true" : "false",
        request_ids};

    const auto page_sql = std::string("select") + set_columns + set_from + filter +
                          " order by (cp.published_at is null), cp.published_at desc nulls last,"
                          " cp.as_of desc, coalesce(s.oresmd_uri, '')"
                          " offset $7::bigint limit $8::bigint";
    auto page_params = filter_params;
    page_params.push_back(std::to_string(request.offset));
    page_params.push_back(std::to_string(std::max(1U, request.limit)));

    const auto rows = execute_parameterized_multi_column_query(
        ctx, page_sql, page_params, published_curve_reader_lg(), "Reading the published curves");

    std::vector<messaging::published_curve_set> sets;
    sets.reserve(rows.size());
    for (const auto& r : rows)
        sets.push_back(set_of(r));
    name_parties(ctx, sets);
    response.sets = std::move(sets);

    // The total is the whole filtered set and not the page, so a screen pages
    // without a second read of its own. A page past the end states zero rows and
    // still names the size of the set it paged past.
    const auto count_sql = std::string("select count(*)::text") + set_from + filter;
    const auto counted = execute_parameterized_multi_column_query(ctx,
                                                                  count_sql,
                                                                  filter_params,
                                                                  published_curve_reader_lg(),
                                                                  "Counting the published curves");
    response.total = counted.empty() ? 0U : count_at(counted.front(), 0);

    response.success = true;
    BOOST_LOG_SEV(published_curve_reader_lg(), debug)
        << "Read " << response.sets.size() << " published curve(s) of " << response.total;
    return response;
}

messaging::get_published_curve_sets_response
get_published_curve_sets(const ores::database::context& ctx,
                         const messaging::get_published_curve_sets_request& request) {
    messaging::get_published_curve_sets_response response;
    if (request.series_id.empty()) {
        response.message =
            "Name the series whose published sets are wanted, so the read answers one curve.";
        return response;
    }

    const auto tenant = ctx.tenant_id().to_string();
    const auto limit = std::to_string(std::max(1U, request.limit));

    // The newest published sets, counted from the newest and returned oldest
    // first, so the caller draws them in the order they happened and colours the
    // last one strongest.
    //
    // published_at keeps whole seconds, so two publishes in one second tie. A
    // superseded set is older than the set that superseded it, so it sorts as
    // the older of a tie.
    const auto published_sql =
        std::string("select * from (select") + set_columns + set_from +
        " where cp.tenant_id = $1::uuid"
        "   and cp.output_series_id = $2::uuid"
        "   and cp.valid_to = ores_utility_infinity_timestamp_fn()"
        "   and cp.published_at is not null"
        " order by cp.published_at desc, (cp.superseded_at is null) desc, cp.as_of desc"
        " limit $3::bigint) newest"
        " order by published_at, (superseded_at is null), as_of";
    const auto published_rows =
        execute_parameterized_multi_column_query(ctx,
                                                 published_sql,
                                                 {tenant, request.series_id, limit},
                                                 published_curve_reader_lg(),
                                                 "Reading the published sets of a curve");

    std::vector<messaging::published_curve_set> sets;
    sets.reserve(published_rows.size());
    for (const auto& r : published_rows)
        sets.push_back(set_of(r));

    // A candidate is a set nobody has approved, so it is not counted against the
    // number of published sets the caller asked for: it follows them.
    if (request.include_candidates) {
        const auto candidate_sql = std::string("select") + set_columns + set_from +
                                   " where cp.tenant_id = $1::uuid"
                                   "   and cp.output_series_id = $2::uuid"
                                   "   and cp.valid_to = ores_utility_infinity_timestamp_fn()"
                                   "   and cp.published_at is null"
                                   " order by cp.as_of, cp.id"
                                   " limit $3::bigint";
        const auto candidate_rows =
            execute_parameterized_multi_column_query(ctx,
                                                     candidate_sql,
                                                     {tenant, request.series_id, limit},
                                                     published_curve_reader_lg(),
                                                     "Reading the candidates of a curve");
        for (const auto& r : candidate_rows)
            sets.push_back(set_of(r));
    }

    const auto counted = execute_parameterized_multi_column_query(
        ctx,
        "select count(*)::text"
        " from ores_marketdata_curve_publishes_tbl cp"
        " where cp.tenant_id = $1::uuid"
        "   and cp.output_series_id = $2::uuid"
        "   and cp.valid_to = ores_utility_infinity_timestamp_fn()"
        "   and cp.published_at is not null",
        {tenant, request.series_id},
        published_curve_reader_lg(),
        "Counting the published sets of a curve");
    response.published_count = counted.empty() ? 0U : count_at(counted.front(), 0);

    // The points of every set in one read: each set holds the observations the
    // publish wrote at its own as-of, and a set whose as-of is repeated holds the
    // same rows, so the read takes the instants and groups by them.
    std::vector<std::string> instants;
    for (const auto& set : sets) {
        const auto at = datetime::to_db_string(set.as_of);
        if (std::find(instants.begin(), instants.end(), at) == instants.end())
            instants.push_back(at);
    }
    // Keyed by the instant each point was observed at, which is the set's as-of:
    // the store's text and the wire's differ in spelling, so the key is the
    // instant and not either spelling of it.
    std::map<std::chrono::system_clock::time_point, std::vector<messaging::published_curve_point>>
        points_of;
    if (!instants.empty()) {
        const auto points_rows = execute_parameterized_multi_column_query(
            ctx,
            "select o.observation_datetime::text,"
            " o.value, o.oresmd_uri"
            " from ores_marketdata_market_observations_tbl o"
            " where o.tenant_id = $1::uuid"
            "   and o.series_id = $2::uuid"
            "   and o.observation_datetime = any($3::timestamptz[])"
            "   and o.valid_to = ores_utility_infinity_timestamp_fn()"
            " order by o.observation_datetime, o.oresmd_uri",
            {tenant, request.series_id, braced(instants)},
            published_curve_reader_lg(),
            "Reading the points of a curve's published sets");
        for (const auto& r : points_rows)
            points_of[datetime::from_db_string(text_at(r, 0))].push_back(
                point_of(text_at(r, 0), text_at(r, 1), text_at(r, 2)));
    }

    for (auto& set : sets) {
        const auto found = points_of.find(set.as_of);
        if (found != points_of.end())
            set.points = found->second;
    }

    name_parties(ctx, sets);
    response.sets = std::move(sets);
    response.success = true;
    BOOST_LOG_SEV(published_curve_reader_lg(), debug)
        << "Read " << response.sets.size() << " set(s) of series " << request.series_id
        << " out of " << response.published_count << " published";
    return response;
}

}
