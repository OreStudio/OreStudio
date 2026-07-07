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
#ifndef ORES_MARKETDATA_CLIENT_MARKET_DATA_CLIENT_HPP
#define ORES_MARKETDATA_CLIENT_MARKET_DATA_CLIENT_HPP

#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.client/export.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <expected>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::client {

/**
 * @brief Typed, authenticated facade over the marketdata NATS protocol.
 *
 * Centralises request/response (de)serialisation and service authentication
 * so callers never hand-roll request_sync + rfl::json + auth headers. Every
 * call is issued through an authenticated nats_client, so the caller must
 * supply one backed by a valid token provider (e.g. a service-token provider
 * for service-to-service calls, or a user session for the GUI).
 *
 * Each method returns std::expected: the decoded response on success, or a
 * human-readable error string on transport, authorisation, or decode failure.
 *
 * @pre The @p nats client must be connected and must outlive this object.
 */
class ORES_MARKETDATA_CLIENT_EXPORT market_data_client {
public:
    explicit market_data_client(ores::nats::service::nats_client& nats);

    /**
     * @brief List market series, optionally filtered by series type.
     *
     * @param series_type ORE key type component (e.g. "FX"); empty = all types.
     */
    [[nodiscard]] std::expected<std::vector<domain::market_series>, std::string>
    list_series(const std::string& series_type = {});

    /**
     * @brief Persist (insert or update) market series.
     *
     * @return The number of series saved on success.
     */
    [[nodiscard]] std::expected<int, std::string>
    save_series(const std::vector<domain::market_series>& series);

    /**
     * @brief Persist market observations.
     *
     * @return The number of observations saved on success.
     */
    [[nodiscard]] std::expected<int, std::string>
    save_observations(const std::vector<domain::market_observation>& observations);

    /**
     * @brief Find an existing market series by its natural key.
     *
     * Fetches a generous page of series (limit 10000) and scans for an exact
     * (series_type, metric, qualifier) match; there is no dedicated
     * lookup-by-key request on the protocol yet.
     *
     * @return The matching series, std::nullopt if none found, or an error.
     */
    [[nodiscard]] std::expected<std::optional<domain::market_series>, std::string>
    find_series(const std::string& series_type,
               const std::string& metric,
               const std::string& qualifier);

    /**
     * @brief List observations for a series (limit 10000, first page).
     */
    [[nodiscard]] std::expected<std::vector<domain::market_observation>, std::string>
    list_observations(const std::string& series_id);

private:
    ores::nats::service::nats_client& nats_;
};

}

#endif
