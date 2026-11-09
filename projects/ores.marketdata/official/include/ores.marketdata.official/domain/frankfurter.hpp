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
#include "ores.marketdata.official/domain/fetcher.hpp"
#include "ores.marketdata.official/export.hpp"
#include <chrono>
#include <cstdint>
#include <expected>
#include <functional>
#include <map>
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
 * every read names a provider. A provider's ids are the currencies it quotes against its
 * pivot, and a fixing is the rate of the pivot in that currency.
 */
class ORES_MARKETDATA_OFFICIAL_EXPORT frankfurter_fetcher final : public fetcher {
public:
    frankfurter_fetcher(http::client::net::transport& transport, std::string base_url);

    [[nodiscard]] std::string_view adapter_code() const override {
        return "frankfurter";
    }

    [[nodiscard]] std::string_view id_scheme() const override {
        return "iso4217";
    }

    [[nodiscard]] std::string_view field() const override {
        return "rate";
    }

    /// The currencies the provider @p key quotes, which also records its pivot.
    [[nodiscard]] std::expected<std::vector<std::string>, std::string>
    universe(std::string_view key) override;

    /**
     * @brief The rates the provider @p key published against its pivot.
     *
     * A rate against another base would be filed under the wrong item, since an item names
     * the pivot, so it is refused and counted. The universe of @p key must be read first.
     */
    [[nodiscard]] std::expected<std::vector<published_fixing>, std::string>
    fixings(std::string_view key,
            std::chrono::year_month_day from,
            std::chrono::year_month_day to) override;

    /// What the provider @p key quotes.
    [[nodiscard]] std::expected<provider_universe, std::string> provider(std::string_view key);

    /**
     * @brief The rates the provider @p key published from @p from to @p to, both included,
     * against @p pivot.
     *
     * Frankfurter rebases every provider to the euro unless asked for another base, so the
     * request always names the provider's own pivot. The answer is one list, unpaged: a
     * window of 400 days of the ECB came back as 8,190 rows in one response.
     */
    [[nodiscard]] std::expected<std::vector<published_rate>, std::string>
    rates(std::string_view key,
          std::string_view pivot,
          std::chrono::year_month_day from,
          std::chrono::year_month_day to);

    /// The rates refused because their base was not the provider's pivot.
    [[nodiscard]] std::uint64_t misbased() const {
        return misbased_;
    }

private:
    std::expected<std::string, std::string> read(const std::string& url);

    http::client::net::transport& transport_;
    std::string base_url_;
    std::map<std::string, std::string, std::less<>> pivots_;
    std::uint64_t misbased_ = 0;
};

}

#endif
