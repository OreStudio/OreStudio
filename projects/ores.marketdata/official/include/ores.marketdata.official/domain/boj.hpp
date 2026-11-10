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
#ifndef ORES_MARKETDATA_OFFICIAL_DOMAIN_BOJ_HPP
#define ORES_MARKETDATA_OFFICIAL_DOMAIN_BOJ_HPP

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
 * @brief The fixings of the series @p key in a JSON answer of the Bank of Japan's time-series
 * API.
 *
 * The key is =<database>.<series code>=, such as =FM01.STRDCLUCON=. The series' entry of
 * =RESULTSET= holds the dates in =VALUES.SURVEY_DATES=, as numbers such as =20261105=, and the
 * values, in percent, in =VALUES.VALUES= at the same positions. A null value is a day with no
 * publication and is skipped. An answer whose =STATUS= is not 200, which is paged, or whose
 * entries do not name the series, is an error, so a renamed series is not read as an empty one.
 */
[[nodiscard]] ORES_MARKETDATA_OFFICIAL_EXPORT
    std::expected<std::vector<published_fixing>, std::string>
    parse_boj_series(std::string_view json, std::string_view key);

/**
 * @brief Reads a series of the Bank of Japan's time-series API.
 *
 * A source's key is =<database>.<series code>=, such as =FM01.STRDCLUCON= for the
 * uncollateralised overnight call rate. The API takes its window in months, so the fetcher
 * asks for the months that hold the window and keeps the days inside it.
 */
class ORES_MARKETDATA_OFFICIAL_EXPORT boj_fetcher final : public fetcher {
public:
    boj_fetcher(http::client::net::transport& transport, std::string base_url);

    [[nodiscard]] std::string_view adapter_code() const override {
        return "boj_stat";
    }

    [[nodiscard]] std::string_view id_scheme() const override {
        return "boj_stat";
    }

    [[nodiscard]] std::string_view field() const override {
        return "VALUES";
    }

    /// The key @p key, when it is a database and a series code of capitals, digits and
    /// underscores, joined by a dot.
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
