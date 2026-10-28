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
#ifndef ORES_MARKETDATA_OFFICIAL_DOMAIN_FRANKFURTER_HPP
#define ORES_MARKETDATA_OFFICIAL_DOMAIN_FRANKFURTER_HPP

#include "ores.http.client/net/transport.hpp"
#include "ores.marketdata.official/export.hpp"
#include <chrono>
#include <expected>
#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::official::domain {

/**
 * @brief One rate a publisher published: the units of @c quote that one @c base bought on
 * @c date.
 */
struct published_rate final {
    std::chrono::year_month_day date;
    std::string base;
    std::string quote;
    /// The rate as the shortest text that reads back to the published number.
    std::string rate;
};

/**
 * @brief The rates of a response of =/v2/providers/<key>/rates=. A row of a currency against
 * itself is left out.
 */
[[nodiscard]] ORES_MARKETDATA_OFFICIAL_EXPORT
    std::expected<std::vector<published_rate>, std::string>
    parse_rates(std::string_view json);

/**
 * @brief What a provider quotes: its pivot, and the currencies it quotes against it.
 */
struct provider_universe final {
    std::string pivot;
    std::vector<std::string> currencies;
};

/**
 * @brief The universe of the provider @p key, from a response of =/v2/providers=. The pivot is
 * left out of the currencies.
 */
[[nodiscard]] ORES_MARKETDATA_OFFICIAL_EXPORT std::expected<provider_universe, std::string>
parse_universe(std::string_view json, std::string_view key);

/**
 * @brief Reads one provider at a time through Frankfurter, never the blended rate.
 *
 * The blended rate is Frankfurter's own sum and hides which bank the number came from, so
 * every read names a provider.
 */
class ORES_MARKETDATA_OFFICIAL_EXPORT frankfurter_fetcher final {
public:
    frankfurter_fetcher(http::client::net::transport& transport, std::string base_url);

    /// What the provider @p key quotes.
    [[nodiscard]] std::expected<provider_universe, std::string> universe(std::string_view key);

    /**
     * @brief The rates the provider @p key published from @p from to @p to, both included,
     * against @p pivot.
     *
     * Frankfurter rebases every provider to the euro unless asked for another base, so the
     * request always names the provider's own pivot.
     */
    [[nodiscard]] std::expected<std::vector<published_rate>, std::string>
    rates(std::string_view key,
          std::string_view pivot,
          std::chrono::year_month_day from,
          std::chrono::year_month_day to);

private:
    std::expected<std::string, std::string> read(const std::string& url);

    http::client::net::transport& transport_;
    std::string base_url_;
};

}

#endif
