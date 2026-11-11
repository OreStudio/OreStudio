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
#include "ores.marketdata.service/app/fx_forward_ladder_service.hpp"
#include "../curve_pillar_reader.hpp"
#include "../fx_forward_ladder_plan.hpp"
#include "ores.analytics.quant/service/fx_forward_calculator.hpp"
#include "ores.marketdata.core/repository/curve_publish_repository.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.refdata.core/mapping/quant_codes.hpp"
#include "ores.refdata.core/repository/calendar_date_repository.hpp"
#include "ores.refdata.core/repository/currency_pair_convention_calendar_repository.hpp"
#include "ores.refdata.core/repository/currency_pair_convention_repository.hpp"
#include "ores.refdata.core/repository/currency_repository.hpp"
#include "ores.refdata.core/repository/ir_curve_bootstrap_config_repository.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::marketdata::service::app {

namespace {

namespace quant = ores::analytics::quant;
namespace refdata_repo = ores::refdata::repository;
using ores::database::context;
using time_point = std::chrono::system_clock::time_point;

/// A missing input of the ladder, whose message the reply carries.
struct refused final : std::runtime_error {
    using std::runtime_error::runtime_error;
};

struct spot_reading final {
    double value = 0.0;
    time_point observed_at{};
    std::string series_id;
};

struct funding_curve final {
    std::vector<quant::service::bootstrapped_point> points;
    ores::marketdata::messaging::fx_forward_curve_source source;
    quant::service::interpolation_method_code interpolation{};
};

std::chrono::year_month_day date_of(time_point t) {
    return std::chrono::year_month_day{std::chrono::floor<std::chrono::days>(t)};
}

/// The latest spot of a pair at or before the as-of, or nothing when no series holds one.
std::optional<spot_reading> find_spot(context ctx,
                                      const fx_pair_codes& pair,
                                      time_point as_of,
                                      const std::optional<boost::uuids::uuid>& party_id) {
    const auto uri =
        "oresmd://fx/" + pair.base + "?type=series&instrument=fx_spot&quote=rate&ccy=" + pair.quote;
    repository::market_series_repository series_repo;
    repository::market_observation_repository observation_repo;

    std::optional<domain::market_observation> latest;
    for (const auto& series : series_repo.read_latest_by_uri(ctx, uri)) {
        if (party_id && series.party_id != *party_id)
            continue;
        for (auto& observation : observation_repo.read_as_of(ctx, series.id, as_of))
            if (!latest || observation.observation_datetime > latest->observation_datetime)
                latest = std::move(observation);
    }
    if (!latest)
        return std::nullopt;
    return spot_reading{observation_value(*latest),
                        latest->observation_datetime,
                        boost::uuids::to_string(latest->series_id)};
}

spot_reading read_spot(context ctx,
                       const fx_pair_codes& pair,
                       time_point as_of,
                       const std::optional<boost::uuids::uuid>& party_id) {
    auto spot = find_spot(ctx, pair, as_of, party_id);
    if (!spot)
        throw refused("No spot of " + pair.base + "/" + pair.quote +
                      " was observed at or before the as-of.");
    return *spot;
}

/// A currency's spot against USD, in whichever direction the market quotes it. The market quotes
/// one direction per currency, so CCY/USD is tried first and USD/CCY only when it is absent.
std::pair<usd_leg, spot_reading> read_usd_leg(context ctx,
                                              const std::string& currency,
                                              time_point as_of,
                                              const std::optional<boost::uuids::uuid>& party_id) {
    if (auto spot = find_spot(ctx, {currency, "USD"}, as_of, party_id))
        return {usd_leg{.currency = currency, .value = spot->value, .usd_is_base = false}, *spot};
    if (auto spot = find_spot(ctx, {"USD", currency}, as_of, party_id))
        return {usd_leg{.currency = currency, .value = spot->value, .usd_is_base = true}, *spot};
    throw refused("No spot of " + currency +
                  " against USD was observed at or before the as-of, "
                  "so the cross has no USD leg for " +
                  currency + ".");
}

funding_curve read_funding_curve(context ctx,
                                 const std::string& currency,
                                 const std::string& party_id,
                                 time_point as_of) {
    refdata_repo::ir_curve_bootstrap_config_repository config_repo;
    std::vector<ores::refdata::domain::ir_curve_bootstrap_config> configs;
    for (auto& config : config_repo.read_latest(ctx))
        if (config.currency_code == currency && config.curve_family_role == "FUNDING" &&
            (party_id.empty() || boost::uuids::to_string(config.party_id) == party_id))
            configs.push_back(std::move(config));
    if (configs.empty())
        throw refused("No funding curve is configured for " + currency + ".");
    if (configs.size() > 1)
        throw refused("More than one funding curve is configured for " + currency +
                      ". Name the party.");
    const auto& config = configs.front();

    repository::curve_publish_repository publish_repo;
    const auto sets = publish_repo.read_applied_for_series(ctx, config.output_series_id);
    const auto set = select_published_set(sets, as_of);
    if (!set)
        throw refused("No curve of " + currency + " is published at or before the as-of.");

    const auto reference = date_of(set->as_of);
    const auto refctx = build_refdata_context(ctx, reference, config.tenor_convention_code);
    funding_curve out;
    out.points = anchor_curve(read_discount_curve(ctx, config.output_series_id, set->as_of, refctx),
                              reference);
    out.interpolation =
        ores::refdata::mapping::parse_interpolation_method_code(config.interpolation_method);
    out.source = {.currency = currency,
                  .curve_name = config.name,
                  .output_series_id = boost::uuids::to_string(config.output_series_id),
                  .publish_id = boost::uuids::to_string(set->id),
                  .as_of = set->as_of,
                  .published_by = set->published_by,
                  .state_code = set->withdrawn_at  ? "withdrawn" :
                                set->superseded_at ? "superseded" :
                                                     "published"};
    return out;
}

int read_spot_days(context ctx, const fx_pair_codes& pair) {
    refdata_repo::currency_repository repo;
    int days = 0;
    for (const auto& code : {pair.base, pair.quote}) {
        const auto rows = repo.read_latest(ctx, code);
        if (rows.empty())
            throw refused("Currency " + code + " is not defined, so its spot days are not known.");
        days = std::max(days, rows.front().spot_days);
    }
    if (days < 1)
        throw refused("The pair settles on the day of the trade, which the ladder does not serve.");
    return days;
}

quant::domain::business_day_calendar_set
read_calendar(context ctx, const std::string& pair_code, std::string& codes) {
    refdata_repo::currency_pair_convention_calendar_repository links{ctx};
    refdata_repo::calendar_date_repository dates{ctx};
    std::vector<std::vector<ores::refdata::domain::calendar_date>> calendars;
    for (const auto& link : links.read_latest_by_pair(pair_code)) {
        codes += (codes.empty() ? "" : ",") + link.calendar_code;
        calendars.push_back(dates.read_latest_by_calendar(link.calendar_code));
        if (calendars.back().empty())
            throw refused("Calendar " + link.calendar_code + " of " + pair_code +
                          " has no materialised dates.");
    }
    if (calendars.empty())
        throw refused("The convention of " + pair_code + " names no calendar.");
    return joint_calendar(calendars);
}

computed_fx_forward_set answer(context ctx, const fx_forward_inputs& request) {
    const auto pair = parse_pair(request.pair_code);
    if (!pair)
        throw refused(pair.error());

    refdata_repo::currency_pair_convention_repository convention_repo;
    const auto conventions = convention_repo.read_latest(ctx, request.pair_code);
    if (conventions.empty())
        throw refused("No convention is published for " + request.pair_code + ".");
    const auto& convention = conventions.front();

    computed_fx_forward_set computed;
    computed.convention_version = convention.version;
    const auto calendar = read_calendar(ctx, request.pair_code, computed.calendar_codes);
    const int spot_days = read_spot_days(ctx, *pair);
    std::optional<boost::uuids::uuid> party;
    if (!request.party_id.empty())
        party = boost::lexical_cast<boost::uuids::uuid>(request.party_id);
    spot_reading spot;
    if (!is_cross(*pair)) {
        spot = read_spot(ctx, *pair, request.as_of, party);
        computed.spots.push_back({.leg_code = "direct",
                                  .pair_code = request.pair_code,
                                  .series_id = spot.series_id,
                                  .observed_at = spot.observed_at,
                                  .value = spot.value});
    } else {
        const auto [base_leg, base_spot] = read_usd_leg(ctx, pair->base, request.as_of, party);
        const auto [quote_leg, quote_spot] = read_usd_leg(ctx, pair->quote, request.as_of, party);
        const auto leg_pair = [](const usd_leg& l) {
            return l.usd_is_base ? "USD/" + l.currency : l.currency + "/USD";
        };
        computed.spots.push_back({.leg_code = "base_leg",
                                  .pair_code = leg_pair(base_leg),
                                  .series_id = base_spot.series_id,
                                  .observed_at = base_spot.observed_at,
                                  .value = base_spot.value});
        computed.spots.push_back({.leg_code = "quote_leg",
                                  .pair_code = leg_pair(quote_leg),
                                  .series_id = quote_spot.series_id,
                                  .observed_at = quote_spot.observed_at,
                                  .value = quote_spot.value});
        // The cross has no series of its own: the legs name theirs. It is as fresh as its older
        // leg, so a reader never takes it for fresher than it is.
        spot.value = cross_rate(base_leg, quote_leg);
        spot.observed_at = std::min(base_spot.observed_at, quote_spot.observed_at);
    }
    auto base = read_funding_curve(ctx, pair->base, request.party_id, request.as_of);
    auto quote = read_funding_curve(ctx, pair->quote, request.party_id, request.as_of);
    if (base.interpolation != quote.interpolation)
        throw refused("The funding curves of " + request.pair_code +
                      " use different interpolation methods.");

    const auto roll = ores::refdata::mapping::parse_business_day_convention_code(
        convention.business_day_convention.value_or("ModifiedFollowing"));
    auto dates = build_ladder_dates(
        date_of(request.as_of), spot_days, roll, convention.end_of_month.value_or(false), calendar);

    const auto spot_date = dates.spot_date;
    const auto rows = quant::service::calculate_fx_forwards({.base_currency = pair->base,
                                                             .quote_currency = pair->quote,
                                                             .spot = spot.value,
                                                             .spot_date = spot_date,
                                                             .pip_factor = convention.pip_factor,
                                                             .base_curve = std::move(base.points),
                                                             .quote_curve = std::move(quote.points),
                                                             .interpolation = base.interpolation,
                                                             .tenors = std::move(dates.tenors)});
    if (!rows)
        throw refused(rows.error());

    auto& out = computed.ladder;
    out.pair_code = request.pair_code;
    out.as_of = request.as_of;
    out.spot = spot.value;
    out.spot_observed_at = spot.observed_at;
    out.spot_date = spot_date;
    out.spot_days = spot_days;
    out.pip_factor = convention.pip_factor;
    out.decimal_places = convention.decimal_places;
    out.base_curve = std::move(base.source);
    out.quote_curve = std::move(quote.source);
    out.rows.reserve(rows->size());
    for (const auto& r : *rows)
        out.rows.push_back({.tenor = r.tenor_id,
                            .near_date = r.near_date,
                            .value_date = r.value_date,
                            .base_discount = r.base_discount,
                            .quote_discount = r.quote_discount,
                            .forward_rate = r.forward_rate,
                            .forward_points = r.forward_points,
                            .swap_points = r.swap_points});
    return computed;
}

}

computed_fx_forward_set fx_forward_ladder_service::compute(context ctx,
                                                           const fx_forward_inputs& inputs) {
    return answer(ctx, inputs);
}

}
