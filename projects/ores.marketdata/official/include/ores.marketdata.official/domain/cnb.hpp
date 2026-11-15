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
#ifndef ORES_MARKETDATA_OFFICIAL_DOMAIN_CNB_HPP
#define ORES_MARKETDATA_OFFICIAL_DOMAIN_CNB_HPP

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
 * @brief The fixings in a yearly file of the Czech National Bank's CZEONIA rates, read as the
 * series @p key.
 *
 * The file is text, a header =Date|Volume (CZK mio)|Rate= and then one line a business day,
 * such as =02 Jan 2026|11671|3.07=, with the rate in percent. A year with no rate yet is an
 * empty file.
 */
[[nodiscard]] ORES_MARKETDATA_OFFICIAL_EXPORT
    std::expected<std::vector<published_fixing>, std::string>
    parse_cnb_czeonia(std::string_view text, std::string_view key);

/**
 * @brief Reads CZEONIA from the yearly files the Czech National Bank publishes.
 *
 * The only key is =CZEONIA=. The bank publishes one file a calendar year, from 2002, and
 * needs no key, so a window is read one year at a time.
 */
class ORES_MARKETDATA_OFFICIAL_EXPORT cnb_fetcher final : public fetcher {
public:
    cnb_fetcher(http::client::net::transport& transport, std::string base_url);

    [[nodiscard]] std::string_view adapter_code() const override {
        return "cnb_czeonia";
    }

    [[nodiscard]] std::string_view id_scheme() const override {
        return "cnb_czeonia";
    }

    [[nodiscard]] std::string_view field() const override {
        return "Rate";
    }

    /// The key @p key, when it is =CZEONIA=.
    [[nodiscard]] std::expected<std::vector<std::string>, std::string>
    universe(std::string_view key) override;

    /// The fixings of CZEONIA from @p from to @p to, both included.
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
