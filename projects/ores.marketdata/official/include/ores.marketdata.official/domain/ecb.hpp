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
#ifndef ORES_MARKETDATA_OFFICIAL_DOMAIN_ECB_HPP
#define ORES_MARKETDATA_OFFICIAL_DOMAIN_ECB_HPP

#include "ores.http.client/net/transport.hpp"
#include "ores.marketdata.official/domain/fetcher.hpp"
#include "ores.marketdata.official/export.hpp"
#include <chrono>
#include <expected>
#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::official::domain {

/**
 * @brief The fixings of the series @p key in a CSV answer of the ECB Data Portal.
 *
 * The answer of =format=csvdata&detail=dataonly= names its columns in its first line, and
 * each row is one observation. The value is the decimal of =OBS_VALUE=, which the ECB
 * publishes in percent. An empty answer holds no observation.
 */
[[nodiscard]] ORES_MARKETDATA_OFFICIAL_EXPORT
    std::expected<std::vector<published_fixing>, std::string>
    parse_ecb_csv(std::string_view csv, std::string_view key);

/**
 * @brief Reads a series of the ECB Data Portal.
 *
 * A source's key is the full series key, such as =EST.B.EU000A2X2A25.WT=: the dataflow, then
 * the key within it. The portal answers a query it has not cached with a 504 after thirty
 * seconds, and the same query again from its cache, so a read tries up to three times, each
 * with a limit of sixty seconds. A transport failure is tried again too, so a read that cannot
 * reach the portal gives up after about three minutes.
 */
class ORES_MARKETDATA_OFFICIAL_EXPORT ecb_fetcher final : public fetcher {
public:
    ecb_fetcher(http::client::net::transport& transport, std::string base_url);

    [[nodiscard]] std::string_view adapter_code() const override {
        return "ecb_portal";
    }

    [[nodiscard]] std::string_view id_scheme() const override {
        return "ecb_sdmx";
    }

    [[nodiscard]] std::string_view field() const override {
        return "OBS_VALUE";
    }

    /// The series key @p key, when it names a dataflow and a key within it.
    [[nodiscard]] std::expected<std::vector<std::string>, std::string>
    universe(std::string_view key) override;

    /// The fixings of the series @p key from @p from to @p to, both included.
    [[nodiscard]] std::expected<std::vector<published_fixing>, std::string>
    fixings(std::string_view key,
            std::chrono::year_month_day from,
            std::chrono::year_month_day to) override;

private:
    http::client::net::transport& transport_;
    std::string base_url_;
};

}

#endif
