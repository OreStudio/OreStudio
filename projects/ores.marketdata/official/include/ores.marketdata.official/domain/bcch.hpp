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
#ifndef ORES_MARKETDATA_OFFICIAL_DOMAIN_BCCH_HPP
#define ORES_MARKETDATA_OFFICIAL_DOMAIN_BCCH_HPP

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
 * @brief The user and password of an account of the Banco Central de Chile's statistical
 * database, whose API access the owner activated.
 */
struct bcch_account final {
    std::string user;
    std::string password;
};

/**
 * @brief The fixings in an answer of the Banco Central de Chile's BDE web service, read as the
 * series @p key.
 *
 * The answer gives a =Codigo= of 0 for success, and another code with a =Descripcion= for an
 * error, such as -5 for a refused account or -50 for an unknown series. A success holds the
 * series in =Series=, with its id in =seriesId= and its observations in =Obs=. Each observation
 * has a date in =indexDateString=, such as =06-11-2026= for the 6th of November, a value in
 * percent as text in =value=, and a =statusCode=. Only an observation of status =OK= is a
 * value: a day without one, of status =ND= or =NA=, is skipped. The service writes its text in
 * Latin-1, so text that is not UTF-8 is accepted.
 */
[[nodiscard]] ORES_MARKETDATA_OFFICIAL_EXPORT
    std::expected<std::vector<published_fixing>, std::string>
    parse_bcch_series(std::string_view json, std::string_view key);

/**
 * @brief Reads a series of the Banco Central de Chile's BDE web service.
 *
 * A source's key is a series id, such as =F022.TIB.TIP.D001.NO.Z.D= for the average overnight
 * interbank rate, on which the Cámara index is built. The service needs the user and password
 * of an account, which go in the =user= and =pass= query parameters, which a logged address
 * masks. Without an account, each read is an error that names the variable to set. The service
 * answers a window of any length in one read.
 */
class ORES_MARKETDATA_OFFICIAL_EXPORT bcch_fetcher final : public fetcher {
public:
    bcch_fetcher(http::client::net::transport& transport,
                 std::string base_url,
                 std::expected<bcch_account, std::string> account);

    [[nodiscard]] std::string_view adapter_code() const override {
        return "bcch_bde";
    }

    [[nodiscard]] std::string_view id_scheme() const override {
        return "bcch_bde";
    }

    [[nodiscard]] std::string_view field() const override {
        return "value";
    }

    /// The key @p key, when it is a series id and the fetcher has an account.
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
    std::expected<bcch_account, std::string> account_;
};

}

#endif
