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
#ifndef ORES_MARKETDATA_OFFICIAL_DOMAIN_BCB_HPP
#define ORES_MARKETDATA_OFFICIAL_DOMAIN_BCB_HPP

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
 * @brief The fixings in an answer of the Banco Central do Brasil's SGS API, read as the series
 * @p key.
 *
 * The answer is a JSON array of objects, each with a date in =data=, such as =03/11/2026= for
 * the 3rd of November, and a value in percent, as text, in =valor=. An entry with an empty value
 * is skipped. An answer of another shape is an error.
 */
[[nodiscard]] ORES_MARKETDATA_OFFICIAL_EXPORT
    std::expected<std::vector<published_fixing>, std::string>
    parse_bcb_series(std::string_view json, std::string_view key);

/**
 * @brief Reads a series of the Banco Central do Brasil's SGS API.
 *
 * A source's key is the series number, such as =4389= for the annualised CDI. The API answers a
 * window of a daily series of at most ten years, so a longer window is read in parts. It answers
 * both a window with no value and an unknown series with a 404, so after a 404 the fetcher asks
 * for the series' last value to tell them apart.
 */
class ORES_MARKETDATA_OFFICIAL_EXPORT bcb_fetcher final : public fetcher {
public:
    bcb_fetcher(http::client::net::transport& transport, std::string base_url);

    [[nodiscard]] std::string_view adapter_code() const override {
        return "bcb_sgs";
    }

    [[nodiscard]] std::string_view id_scheme() const override {
        return "bcb_sgs";
    }

    [[nodiscard]] std::string_view field() const override {
        return "valor";
    }

    /// The key @p key, when it is a series number of one to nine digits.
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
