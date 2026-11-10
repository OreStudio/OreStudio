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
#ifndef ORES_MARKETDATA_OFFICIAL_DOMAIN_NORGES_BANK_HPP
#define ORES_MARKETDATA_OFFICIAL_DOMAIN_NORGES_BANK_HPP

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
 * @brief The fixings in a CSV answer of Norges Bank's SDMX API, read as the series @p key.
 *
 * The answer separates its cells with a semicolon and names its columns in its first line. The
 * date is in =TIME_PERIOD= and the value, in percent, in =OBS_VALUE=. The query names one
 * series, so each row is an observation of @p key. A row with an empty value is skipped. An
 * answer without those columns is an error.
 */
[[nodiscard]] ORES_MARKETDATA_OFFICIAL_EXPORT
    std::expected<std::vector<published_fixing>, std::string>
    parse_norges_bank_csv(std::string_view csv, std::string_view key);

/**
 * @brief Reads a series of Norges Bank's SDMX API.
 *
 * A source's key is =<dataflow>.<series key>=, such as =SHORT_RATES.B.NOWA.ON.R= for NOWA. The
 * API answers both a window with no observation and an unknown series with a 404, so after a
 * 404 the fetcher asks for the series' latest observation to tell them apart.
 */
class ORES_MARKETDATA_OFFICIAL_EXPORT norges_bank_fetcher final : public fetcher {
public:
    norges_bank_fetcher(http::client::net::transport& transport, std::string base_url);

    [[nodiscard]] std::string_view adapter_code() const override {
        return "nb_sdmx";
    }

    [[nodiscard]] std::string_view id_scheme() const override {
        return "nb_sdmx";
    }

    [[nodiscard]] std::string_view field() const override {
        return "OBS_VALUE";
    }

    /// The key @p key, when it is a dataflow and a series key, joined by a dot, each part of
    /// capitals, digits and underscores.
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
