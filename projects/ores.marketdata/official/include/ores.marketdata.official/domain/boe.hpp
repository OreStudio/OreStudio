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
#ifndef ORES_MARKETDATA_OFFICIAL_DOMAIN_BOE_HPP
#define ORES_MARKETDATA_OFFICIAL_DOMAIN_BOE_HPP

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
 * @brief The fixings of the series @p code in a CSV answer of the Bank of England's
 * statistical database.
 *
 * The first line names =DATE= and the series code, and each row is a date such as
 * =29 Oct 2026= and the value, which the Bank publishes in percent. A header alone holds no
 * observation.
 */
[[nodiscard]] ORES_MARKETDATA_OFFICIAL_EXPORT
    std::expected<std::vector<published_fixing>, std::string>
    parse_boe_csv(std::string_view csv, std::string_view code);

/**
 * @brief Reads a series of the Bank of England's statistical database.
 *
 * A source's key is a series code, such as =IUDSOIA= for SONIA. The database answers an
 * unknown code with a redirect to its error page, which is an error.
 */
class ORES_MARKETDATA_OFFICIAL_EXPORT boe_fetcher final : public fetcher {
public:
    boe_fetcher(http::client::net::transport& transport, std::string base_url);

    [[nodiscard]] std::string_view adapter_code() const override {
        return "boe_iadb";
    }

    [[nodiscard]] std::string_view id_scheme() const override {
        return "boe_iadb";
    }

    [[nodiscard]] std::string_view field() const override {
        return "value";
    }

    /// The series code @p key, when it is letters and digits.
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
