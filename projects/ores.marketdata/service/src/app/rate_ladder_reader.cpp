/* -*- mode: c++; tab-width: 4; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
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
#include "ores.marketdata.service/app/rate_ladder_reader.hpp"

#include "ores.analytics.quant/service/day_count_calculator.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.api/datum/schema.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.service/app/instrument_naming.hpp"
#include "../curve_pillar_reader.hpp"
#include "../curve_republish_resolver.hpp"
#include <algorithm>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <map>
#include <string>
#include <vector>

namespace ores::marketdata::service::app {

namespace {

namespace mm = ores::marketdata::messaging;
namespace dm = ores::marketdata::domain;
namespace dt = ores::marketdata::datum;
using namespace ores::logging;

auto& rate_ladder_lg() {
    static auto instance = make_logger("ores.marketdata.service.rate_ladder_reader");
    return instance;
}

/// A ladder series carries no convention of its own, so the read states the one
/// it resolved the dates under rather than leaving the client to guess.
constexpr const char* ladder_tenor_convention = "RATES_SPOT_FORWARD";
constexpr const char* ladder_day_count_convention = "A365F";

constexpr std::int64_t ladder_default_stale_seconds = 3600;

std::int64_t ladder_age_seconds_of(std::chrono::system_clock::time_point as_of,
                            std::chrono::system_clock::time_point now) {
    if (as_of == std::chrono::system_clock::time_point{})
        return 0;
    const auto age = std::chrono::duration_cast<std::chrono::seconds>(now - as_of).count();
    return age < 0 ? 0 : age;
}

/// One provider's quote at one rung of one ladder series.
struct quote_at_rung final {
    std::string provider_code;
    double value = 0.0;
    std::chrono::system_clock::time_point as_of{};
    std::string series_id;
};

/// The quotes of one rung, keyed by tenor, gathered across providers.
using rung_quotes = std::map<std::string, std::vector<quote_at_rung>>;


/// The rung an observation names.
///
/// The codec refuses a `type=series` URI that also carries a coordinate, and a
/// synthetic feed writes exactly that spelling for the points it publishes. So
/// a URI the codec refuses still states its rung in the query, and the rung is
/// what this read needs: take it from the query text rather than drop the
/// point, and keep the strict parse for a properly composed datum.
std::string rung_of(const std::string& uri) {
    if (auto datum = dt::oresmd_uri_codec::read(uri))
        return field_text(*datum, dt::field::term);
    static const std::string marker = "term=";
    const auto at = uri.find(marker);
    if (at == std::string::npos)
        return {};
    const auto from = at + marker.size();
    const auto to = uri.find('&', from);
    return uri.substr(from, to == std::string::npos ? std::string::npos : to - from);
}

double median_of(std::vector<double> values) {
    std::sort(values.begin(), values.end());
    const auto n = values.size();
    if (n == 0)
        return 0.0;
    return n % 2 ? values[n / 2] : (values[n / 2 - 1] + values[n / 2]) / 2.0;
}

}

mm::get_rate_ladder_response get_rate_ladder(const ores::database::context& ctx,
                                             const mm::get_rate_ladder_request& request) {
    mm::get_rate_ladder_response resp;
    resp.currency = request.currency;
    resp.instrument_type = request.instrument_type;
    resp.forward_start = request.forward_start;
    resp.tenor_convention = ladder_tenor_convention;
    resp.day_count_convention = ladder_day_count_convention;

    const auto now = std::chrono::system_clock::now();
    const auto as_of = request.as_of == std::chrono::system_clock::time_point{}
        ? now
        : request.as_of;
    resp.as_of = as_of;

    repository::market_series_repository series_repo;
    repository::market_observation_repository observation_repo;

    rung_quotes quotes;
    std::vector<std::string> provider_codes;

    // The series read answers the tenant, so the party is filtered here: one
    // series identity exists once per party, and a ladder that mixed parties
    // would count their providers and average their quotes.
    std::vector<std::string> allowed;
    if (const auto party = ctx.party_id())
        allowed.push_back(boost::uuids::to_string(*party));
    else
        for (const auto& party : ctx.visible_party_ids())
            allowed.push_back(boost::uuids::to_string(party));
    const auto may_read = [&](const std::string& party) {
        if (!request.party_id.empty())
            return party == request.party_id;
        return allowed.empty() ||
               std::find(allowed.begin(), allowed.end(), party) != allowed.end();
    };

    for (const auto& series : series_repo.read_latest(ctx)) {
        if (!may_read(boost::uuids::to_string(series.party_id)))
            continue;
        auto parsed = dt::oresmd_uri_codec::read(series.oresmd_uri);
        if (!parsed || !parsed->is_series())
            continue;
        if (field_text(*parsed, dt::field::ccy) != request.currency)
            continue;
        if (std::string(dt::oresmd_uri_codec::instrument_spelling(parsed->type())) !=
            request.instrument_type)
            continue;
        if (!request.forward_start.empty() &&
            field_text(*parsed, dt::field::fwd_start) != request.forward_start)
            continue;
        if (!request.providers.empty() &&
            std::find(request.providers.begin(), request.providers.end(),
                      series.provider_code) == request.providers.end())
            continue;

        const auto series_id = boost::uuids::to_string(series.id);
        if (std::find(provider_codes.begin(), provider_codes.end(), series.provider_code) ==
            provider_codes.end())
            provider_codes.push_back(series.provider_code);

        for (const auto& observation : observation_repo.read_latest_for_series(ctx, series.id)) {
            const auto term = rung_of(observation.oresmd_uri);
            if (term.empty())
                continue;
            quote_at_rung quote;
            quote.provider_code = series.provider_code;
            quote.value = observation_value(observation);
            quote.as_of = observation.observation_datetime;
            quote.series_id = series_id;
            quotes[term].push_back(std::move(quote));
        }
    }

    if (quotes.empty()) {
        resp.success = false;
        resp.message = "No provider quotes a " + request.currency + " " +
                       request.instrument_type + " ladder" +
                       (request.forward_start.empty()
                            ? std::string()
                            : " starting " + request.forward_start) +
                       " at this as-of.";
        return resp;
    }

    // The dates are the server's: the tenor catalog and its convention resolve
    // every rung, and the day count turns the distance into a year fraction.
    const auto horizon = std::chrono::floor<std::chrono::days>(as_of);
    const auto refctx = build_refdata_context(ctx,
                                              std::chrono::year_month_day{horizon},
                                              ladder_tenor_convention);
    const auto day_count =
        ores::analytics::quant::service::day_count_convention_code::A365F;

    std::vector<std::pair<std::chrono::year_month_day, std::string>> dated;
    std::string undated;
    for (const auto& [tenor, ignored] : quotes) {
        (void)ignored;
        try {
            dated.emplace_back(resolve_tenor_date(refctx, tenor), tenor);
        } catch (const std::exception& e) {
            // A rung the convention cannot resolve is named in the answer
            // rather than dropped in silence: a person reading a ladder has to
            // know which rung the server could not date, and why.
            BOOST_LOG_SEV(rate_ladder_lg(), warn)
                << "Cannot date rung '" << tenor << "': " << e.what();
            if (!undated.empty())
                undated += "; ";
            undated += tenor + " (" + e.what() + ")";
        }
    }
    std::sort(dated.begin(), dated.end());

    const auto start = std::chrono::year_month_day{horizon};
    for (const auto& [maturity, tenor] : dated) {
        if (request.limit > 0 &&
            static_cast<std::int64_t>(resp.rungs.size()) >= request.limit)
            break;

        mm::rate_ladder_rung rung;
        rung.tenor = tenor;
        rung.maturity_date = maturity;
        rung.year_fraction = ores::analytics::quant::service::day_count_calculator::year_fraction(
            start, maturity, day_count);

        std::vector<double> values;
        for (const auto& q : quotes[tenor]) {
            mm::rung_quote wire;
            wire.provider_code = q.provider_code;
            wire.value = q.value;
            wire.as_of = q.as_of;
            wire.age_seconds = ladder_age_seconds_of(q.as_of, now);
            wire.stale = wire.age_seconds > ladder_default_stale_seconds;
            wire.series_id = q.series_id;
            values.push_back(q.value);
            rung.quotes.push_back(std::move(wire));
        }
        rung.consensus.value = median_of(values);
        rung.consensus.method = "median";
        rung.consensus.providers_used = static_cast<int>(values.size());
        resp.rungs.push_back(std::move(rung));
    }

    resp.provider_count = static_cast<int>(provider_codes.size());
    if (!undated.empty())
        resp.message = "Some rungs could not be dated under " +
                       std::string(ladder_tenor_convention) + ": " + undated;
    resp.success = true;
    return resp;
}

}
