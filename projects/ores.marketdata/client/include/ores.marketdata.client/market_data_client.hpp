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

#include "ores.marketdata.api/domain/feed_binding.hpp"
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
     * Sends marketdata.v1.market_series.list for a generous page. The canonical
     * request carries no series-type filter, so @p series_type is accepted for
     * call-site compatibility and ignored.
     *
     * @param series_type ORE key type component (e.g. "FX"); empty = all types.
     */
    [[nodiscard]] std::expected<std::vector<domain::market_series>, std::string>
    list_series(const std::string& series_type = {});

    /**
     * @brief Persist (insert or update) market series.
     *
     * Sends one marketdata.v1.market_series.put per item, stating the
     * precondition that lets the write replace whatever is current.
     *
     * @return The number of series saved on success.
     */
    [[nodiscard]] std::expected<int, std::string>
    save_series(const std::vector<domain::market_series>& series);

    /**
     * @brief Persist market observations.
     *
     * Sends one marketdata.v1.market_observations.put per item, stating the
     * precondition that lets the write replace whatever is current.
     *
     * @return The number of observations saved on success.
     */
    [[nodiscard]] std::expected<int, std::string>
    save_observations(const std::vector<domain::market_observation>& observations);

    /**
     * @brief Find an existing market series by its identity.
     *
     * The canonical key for this resource is the surrogate UUID, and the natural
     * key is now the series' oresmd identity, which the list request does not
     * filter on, so the identity is matched client-side against a generous page of
     * marketdata.v1.market_series.list (limit 10000).
     *
     * When @p party_id is non-empty the scan is restricted to that party's
     * series. Without it the first id-ordered match wins, which is arbitrary
     * when the same identity exists for several parties (e.g. FX spot series are
     * materialised per party).
     *
     * @return The matching series, std::nullopt if none found, or an error.
     */
    [[nodiscard]] std::expected<std::optional<domain::market_series>, std::string>
    find_series_by_uri(const std::string& oresmd_uri, const std::string& party_id = {});

    /**
     * @brief Find an existing market series by the registry's decomposition of its
     * key.
     *
     * Superseded by find_series_by_uri() for every caller but the IR curve feed's
     * vintage seeding, which reads a DQ-published series whose identity the cutover
     * cannot name yet; that caller goes when the synthetic config names the series
     * it seeds from.
     *
     * When @p party_id is non-empty the scan is restricted to that party's
     * series. Without it the first id-ordered match wins, which is arbitrary
     * when the same decomposition exists for several parties (e.g. FX spot
     * series are materialised per party).
     *
     * @return The matching series, std::nullopt if none found, or an error.
     */
    [[nodiscard]] std::expected<std::optional<domain::market_series>, std::string>
    find_series(const std::string& series_type,
                const std::string& metric,
                const std::string& qualifier,
                const std::string& party_id = {});

    /**
     * @brief List observations for a series (limit 10000, first page).
     *
     * Sends marketdata.v1.market_observations.list_by_series_id, which returns
     * observations newest first.
     *
     * Prefer list_observations_page() for callers that don't genuinely
     * need the whole series in one go -- a series with a long tick
     * history can produce a response larger than NATS's max payload,
     * which fails silently (the handler completes but the reply never
     * arrives, so the caller just sees a timeout).
     */
    [[nodiscard]] std::expected<std::vector<domain::market_observation>, std::string>
    list_observations(const std::string& series_id);

    /**
     * @brief List one bounded page of observations for a series, newest
     * first (matches the repository's own observation_datetime desc
     * ordering) -- for callers scanning for a specific observation (e.g.
     * a vintage date) who only need a handful of rows, not the whole
     * history.
     *
     * Sends marketdata.v1.market_observations.list_by_series_id.
     */
    [[nodiscard]] std::expected<std::vector<domain::market_observation>, std::string>
    list_observations_page(const std::string& series_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief List feed bindings (limit 10000, first page) — enough for a
     * caller checking whether a given (ore_key, source_name) is already bound.
     *
     * Sends marketdata.v1.feed_bindings.list.
     */
    [[nodiscard]] std::expected<std::vector<domain::feed_binding>, std::string>
    list_feed_bindings();

    /**
     * @brief Persist (insert or update) a feed binding.
     *
     * Sends marketdata.v1.feed_bindings.put, stating the precondition that lets
     * the write replace whatever is current.
     *
     * @pre @p binding.id must already be a non-nil UUID (the caller generates
     * it); tenant/party/audit fields are stamped server-side from the
     * authenticated session.
     */
    [[nodiscard]] std::expected<bool, std::string>
    save_feed_binding(const domain::feed_binding& binding);

private:
    ores::nats::service::nats_client& nats_;
};

}

#endif
