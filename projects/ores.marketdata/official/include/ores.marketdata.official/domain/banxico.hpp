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
#ifndef ORES_MARKETDATA_OFFICIAL_DOMAIN_BANXICO_HPP
#define ORES_MARKETDATA_OFFICIAL_DOMAIN_BANXICO_HPP

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
 * @brief The fixings in an answer of Banco de Mexico's SIE API, read as the series @p key.
 *
 * The answer holds one series in =bmx.series=, with its id in =idSerie=. Its =datos= is an
 * array of objects, each with a date in =fecha=, such as =06/11/2026= for the 6th of November,
 * and a value in percent, as text, in =dato=. A series with no =datos= holds no value in the
 * window. A value of =N/E= is skipped. An answer of another shape, or of another series, is an
 * error.
 */
[[nodiscard]] ORES_MARKETDATA_OFFICIAL_EXPORT
    std::expected<std::vector<published_fixing>, std::string>
    parse_banxico_series(std::string_view json, std::string_view key);

/**
 * @brief Reads a series of Banco de Mexico's SIE API.
 *
 * A source's key is the series id, such as =SF331451= for the TIIE de Fondeo. The API needs a
 * token, which a free registration gives. The fetcher sends it as the =token= query parameter,
 * which a logged address masks. Without a token, each read is an error that names the variable
 * to set. The API answers a window of any length in one read.
 */
class ORES_MARKETDATA_OFFICIAL_EXPORT banxico_fetcher final : public fetcher {
public:
    banxico_fetcher(http::client::net::transport& transport,
                    std::string base_url,
                    std::expected<std::string, std::string> token);

    [[nodiscard]] std::string_view adapter_code() const override {
        return "banxico_sie";
    }

    [[nodiscard]] std::string_view id_scheme() const override {
        return "banxico_sie";
    }

    [[nodiscard]] std::string_view field() const override {
        return "dato";
    }

    /// The key @p key, when it is a series id and the fetcher has a token.
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
    std::expected<std::string, std::string> token_;
};

}

#endif
