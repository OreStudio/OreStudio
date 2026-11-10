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
#ifndef ORES_MARKETDATA_OFFICIAL_DOMAIN_RBA_HPP
#define ORES_MARKETDATA_OFFICIAL_DOMAIN_RBA_HPP

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
 * @brief The fixings of the series @p series in an RBA statistical table, read as @p key.
 *
 * The table is a CSV file. Rows of notes come first, and the row whose first cell is
 * =Series ID= names the series of each column. The rows after it each start with a date, such
 * as =04-Jan-2011=, and hold the values in percent. A row can stop before the last columns, so a
 * missing cell and an empty cell are both skipped. A table with no =Series ID= row, or without
 * the series, is an error.
 */
[[nodiscard]] ORES_MARKETDATA_OFFICIAL_EXPORT
    std::expected<std::vector<published_fixing>, std::string>
    parse_rba_table(std::string_view csv, std::string_view series, std::string_view key);

/**
 * @brief Reads a series of an RBA statistical table.
 *
 * A source's key is =<table>.<series>=, such as =f1.FIRMMCRID= for AONIA, the interbank
 * overnight cash rate of table F1. The RBA publishes each table as one file with its whole
 * history, so each read gets the file and keeps the days of the window.
 */
class ORES_MARKETDATA_OFFICIAL_EXPORT rba_fetcher final : public fetcher {
public:
    rba_fetcher(http::client::net::transport& transport, std::string base_url);

    [[nodiscard]] std::string_view adapter_code() const override {
        return "rba_table";
    }

    [[nodiscard]] std::string_view id_scheme() const override {
        return "rba_table";
    }

    [[nodiscard]] std::string_view field() const override {
        return "value";
    }

    /// The key @p key, when it is a table code of small letters, digits and dots, a dot, and a
    /// series of capitals and digits.
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
