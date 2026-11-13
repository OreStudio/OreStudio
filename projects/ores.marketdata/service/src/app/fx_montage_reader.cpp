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
#include "ores.marketdata.service/app/fx_montage_reader.hpp"

#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.api/datum/schema.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.service/app/instrument_naming.hpp"
#include "../curve_pillar_reader.hpp"
#include <algorithm>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <map>
#include <string>
#include <vector>

namespace ores::marketdata::service::app {

namespace {

namespace mm = ores::marketdata::messaging;
namespace dt = ores::marketdata::datum;
using namespace ores::logging;

auto& fx_montage_lg() {
    static auto instance = make_logger("ores.marketdata.service.fx_montage_reader");
    return instance;
}

constexpr std::int64_t montage_default_stale_seconds = 3600;
constexpr std::uint32_t montage_window_points = 2000;

/// The spots one pair's providers gave, and the numbers the window gives.
struct pair_draft final {
    std::string pair_name;
    std::string base_currency;
    std::string quote_currency;
    std::vector<mm::montage_spot> spots;
    std::optional<double> change_pct;
    std::optional<double> day_high;
    std::optional<double> day_low;
};

std::int64_t montage_age_seconds_of(std::chrono::system_clock::time_point as_of,
                                    std::chrono::system_clock::time_point now) {
    if (as_of == std::chrono::system_clock::time_point{})
        return 0;
    const auto age = std::chrono::duration_cast<std::chrono::seconds>(now - as_of).count();
    return age < 0 ? 0 : age;
}

double montage_median_of(std::vector<double> values) {
    std::sort(values.begin(), values.end());
    const auto n = values.size();
    if (n == 0)
        return 0.0;
    return n % 2 ? values[n / 2] : (values[n / 2 - 1] + values[n / 2]) / 2.0;
}

/**
 * @brief The move one series made inside the window.
 *
 * The first and last price at or after the window's start give the change; the
 * lowest and highest give the range. A series with no price in the window has
 * no move, and says so by leaving the three absent rather than zero.
 */
void apply_window(const std::vector<domain::market_observation>& observations,
                  std::chrono::system_clock::time_point change_since,
                  pair_draft& draft) {
    std::optional<double> first, last, low, high;
    for (const auto& o : observations) {
        if (o.observation_datetime < change_since)
            continue;
        double value = 0.0;
        try {
            value = observation_value(o);
        } catch (const std::exception&) {
            continue;
        }
        if (!first)
            first = value;
        last = value;
        low = low ? std::min(*low, value) : value;
        high = high ? std::max(*high, value) : value;
    }
    if (first && *first != 0.0) {
        draft.change_pct = (*last - *first) / *first * 100.0;
        draft.day_high = high;
        draft.day_low = low;
    }
}

}

mm::get_fx_montage_response get_fx_montage(const ores::database::context& ctx,
                                           const mm::get_fx_montage_request& request) {
    mm::get_fx_montage_response resp;
    resp.base_currency = request.base_currency;

    const auto now = std::chrono::system_clock::now();
    const auto as_of = request.as_of == std::chrono::system_clock::time_point{}
        ? now
        : request.as_of;
    // A trading day starts when the session says it does. Until the session is
    // served, the window starts at midnight UTC and the response states it.
    const auto change_since = std::chrono::system_clock::time_point{
        std::chrono::floor<std::chrono::days>(as_of)};
    resp.as_of = as_of;
    resp.change_since = change_since;

    repository::market_series_repository series_repo;
    repository::market_observation_repository observation_repo;

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

    std::map<std::string, pair_draft> pairs;
    for (const auto& series : series_repo.read_latest(ctx)) {
        if (!may_read(boost::uuids::to_string(series.party_id)))
            continue;
        auto parsed = dt::oresmd_uri_codec::read(series.oresmd_uri);
        if (!parsed || !parsed->is_series())
            continue;
        if (parsed->type() != dt::instrument_type::fx_spot)
            continue;
        const auto base = field_text(*parsed, dt::field::unit_ccy);
        const auto quote = field_text(*parsed, dt::field::ccy);
        if (base != request.base_currency && quote != request.base_currency)
            continue;
        if (!request.providers.empty() &&
            std::find(request.providers.begin(), request.providers.end(),
                      series.provider_code) == request.providers.end())
            continue;

        const auto observations = observation_repo.read_latest_for_series(ctx, series.id);
        if (observations.empty())
            continue;
        const auto& newest = *std::max_element(
            observations.begin(), observations.end(),
            [](const auto& a, const auto& b) {
                return a.observation_datetime < b.observation_datetime;
            });

        mm::montage_spot spot;
        spot.provider_code = series.provider_code;
        try {
            spot.value = observation_value(newest);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(fx_montage_lg(), warn)
                << "Skipping a quote that is not a number: " << newest.oresmd_uri
                << ": " << e.what();
            continue;
        }
        spot.as_of = newest.observation_datetime;
        spot.age_seconds = montage_age_seconds_of(spot.as_of, now);
        spot.stale = spot.age_seconds > montage_default_stale_seconds;
        spot.series_id = boost::uuids::to_string(series.id);

        const auto name = base + "/" + quote;
        auto [it, inserted] = pairs.try_emplace(name);
        auto& draft = it->second;
        if (inserted) {
            draft.pair_name = name;
            draft.base_currency = base;
            draft.quote_currency = quote;
        }
        draft.spots.push_back(spot);

        // The window is read from the store's own rows, so the change and the
        // range rest on the same facts the spot does.
        const auto window = observation_repo.read_latest_by_series_id(
            ctx, boost::uuids::to_string(series.id), 0, montage_window_points);
        apply_window(window, change_since, draft);
    }

    if (pairs.empty()) {
        resp.success = false;
        resp.message = "No provider quotes a cross of " + request.base_currency +
                       " at this as-of.";
        return resp;
    }

    for (auto& [name, draft] : pairs) {
        (void)name;
        mm::fx_montage_pair wire;
        wire.pair_name = draft.pair_name;
        wire.base_currency = draft.base_currency;
        wire.quote_currency = draft.quote_currency;
        wire.spot_by_provider = draft.spots;
        std::vector<double> values;
        for (const auto& s : draft.spots)
            values.push_back(s.value);
        wire.consensus = montage_median_of(values);
        wire.change_pct = draft.change_pct;
        wire.day_high = draft.day_high;
        wire.day_low = draft.day_low;
        resp.pairs.push_back(std::move(wire));
    }

    resp.success = true;
    return resp;
}

}
