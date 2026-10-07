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
#ifndef ORES_SYNTHETIC_API_FEEDS_VINTAGE_LOOKUP_HPP
#define ORES_SYNTHETIC_API_FEEDS_VINTAGE_LOOKUP_HPP

#include "ores.marketdata.client/market_data_client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <cstdint>
#include <expected>
#include <format>
#include <string>

namespace ores::synthetic::feed {

/**
 * @brief ISO date part of an observation timestamp: "2016-02-05" from a
 * timestamp at any time of day on that date. Observations are recorded at
 * midnight UTC for date-only vintages, but this tolerates otherwise.
 */
inline std::string date_part(std::chrono::system_clock::time_point tp) {
    const auto days = std::chrono::floor<std::chrono::days>(tp);
    return std::format("{:%F}", days);
}

/**
 * @brief The one vintage read: the (source, datum URI, date) row inside a
 * series, scanned one bounded page at a time.
 *
 * A series with a long tick history can produce a response larger than the
 * NATS maximum payload, which fails silently -- the handler completes but the
 * reply never arrives, so the caller sees a timeout. Observations come back
 * newest-first, so a recent-ish vintage converges in the first page or two.
 *
 * The series and datum URIs are resolved by the caller from its own config --
 * an FX key's own SPOT datum, or a curve's anchor pillar -- so this function
 * carries the paging rule alone.
 *
 * @param missing_message Caller-owned text for "no matching observation",
 * which names the coordinate the caller anchored on.
 * @param lookup_label Name the lookup failures quote; defaults to @p series_uri.
 * @param party_id Optional party scope for the series lookup.
 *
 * @return The row's value, or an actionable message.
 */
inline std::expected<double, std::string>
find_vintage_observation(ores::nats::service::nats_client& auth_nats,
                         const std::string& caller_bearer_token,
                         const std::string& series_uri,
                         const std::string& datum_uri,
                         const std::string& vintage_source,
                         const std::string& vintage_date,
                         const std::string& missing_message,
                         const std::string& lookup_label = {},
                         const std::string& party_id = {}) {
    const auto& label = lookup_label.empty() ? series_uri : lookup_label;

    auto delegated_nats = auth_nats.with_delegation(caller_bearer_token);
    ores::marketdata::client::market_data_client md_client(delegated_nats);

    auto series = md_client.find_series_by_uri(series_uri, party_id);
    if (!series)
        return std::unexpected("Failed to look up series for '" + label +
                               "': " + series.error());
    if (!series->has_value())
        return std::unexpected(missing_message);

    constexpr std::uint32_t page_size = 200;
    const auto series_id_str = boost::uuids::to_string((*series)->id);
    std::uint32_t offset = 0;
    for (;;) {
        auto observations = md_client.list_observations_page(series_id_str, offset, page_size);
        if (!observations)
            return std::unexpected("Failed to look up observations for '" + label +
                                   "': " + observations.error());
        for (const auto& obs : *observations) {
            if (obs.source == vintage_source && obs.oresmd_uri == datum_uri &&
                date_part(obs.observation_datetime) == vintage_date) {
                try {
                    return std::stod(obs.value);
                } catch (const std::exception& e) {
                    return std::unexpected("Vintage observation value '" + obs.value +
                                           "' is not a valid number: " + e.what());
                }
            }
        }
        if (observations->size() < page_size)
            break;
        offset += page_size;
    }
    return std::unexpected(missing_message);
}

}
#endif
