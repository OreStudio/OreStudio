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
#ifndef ORES_MARKETDATA_OFFICIAL_DOMAIN_NYFED_HPP
#define ORES_MARKETDATA_OFFICIAL_DOMAIN_NYFED_HPP

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
 * @brief The fixings of a response of the New York Fed's =/api/rates/<group>/<rate>/search.json=.
 *
 * Each row of =refRates= is one rate type on one effective date. The id is the type, such as
 * =SOFR=, and the value is the decimal of =percentRate=.
 */
[[nodiscard]] ORES_MARKETDATA_OFFICIAL_EXPORT
    std::expected<std::vector<published_fixing>, std::string>
    parse_nyfed_rates(std::string_view json);

/**
 * @brief Reads the reference rates of the Federal Reserve Bank of New York from its markets
 * API.
 *
 * A source's key is a rate type, such as =SOFR=, and the type is the source's one id.
 */
class ORES_MARKETDATA_OFFICIAL_EXPORT nyfed_fetcher final : public fetcher {
public:
    nyfed_fetcher(http::client::net::transport& transport, std::string base_url);

    [[nodiscard]] std::string_view adapter_code() const override {
        return "nyfed";
    }

    [[nodiscard]] std::string_view id_scheme() const override {
        return "nyfed_rate_type";
    }

    [[nodiscard]] std::string_view field() const override {
        return "percentRate";
    }

    /// The rate type @p key, when the fetcher knows the path of its rate.
    [[nodiscard]] std::expected<std::vector<std::string>, std::string>
    universe(std::string_view key) override;

    /// The fixings of the rate type @p key from @p from to @p to, both included.
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
