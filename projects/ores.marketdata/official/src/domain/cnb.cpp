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
#include "ores.marketdata.official/domain/cnb.hpp"
#include "ores.marketdata.official/domain/published_text.hpp"
#include <algorithm>
#include <format>
#include <utility>

namespace ores::marketdata::official::domain {

namespace {

constexpr std::string_view czeonia_key = "CZEONIA";

}

std::expected<std::vector<published_fixing>, std::string> parse_cnb_czeonia(std::string_view text,
                                                                            std::string_view key) {
    const auto lines = lines_of(text);
    if (lines.empty())
        return std::vector<published_fixing>{};
    const auto header = cells_of(lines.front(), '|');
    const auto date_at = column_of(header, "Date");
    const auto rate_at = column_of(header, "Rate");
    if (!date_at || !rate_at)
        return std::unexpected(
            "the answer is not a yearly CZEONIA file: it names no Date or Rate column");
    const auto width = std::max(*date_at, *rate_at) + 1;
    std::vector<published_fixing> out;
    for (std::size_t i = 1; i < lines.size(); ++i) {
        const auto cells = cells_of(lines[i], '|');
        if (cells.size() < width)
            return std::unexpected(std::format(
                "row {} has {} cells, fewer than the {} its header needs", i, cells.size(), width));
        auto date = date_of_month_name(cells[*date_at], ' ');
        if (!date)
            return std::unexpected(date.error());
        auto value = decimal_of_percent(cells[*rate_at]);
        if (!value)
            return std::unexpected(value.error());
        out.push_back({*date, std::string(key), std::move(*value)});
    }
    return out;
}

cnb_fetcher::cnb_fetcher(http::client::net::transport& transport, std::string base_url)
    : transport_(transport)
    , base_url_(std::move(base_url)) {
    while (!base_url_.empty() && base_url_.back() == '/')
        base_url_.pop_back();
}

std::expected<std::vector<std::string>, std::string> cnb_fetcher::universe(std::string_view key) {
    if (key != czeonia_key)
        return std::unexpected("'" + std::string(key) +
                               "' is not a series of the Czech National Bank's yearly files: "
                               "the only one is CZEONIA");
    return std::vector<std::string>{std::string(key)};
}

std::expected<std::vector<published_fixing>, std::string> cnb_fetcher::fixings(
    std::string_view key, std::chrono::year_month_day from, std::chrono::year_month_day to) {
    if (auto known = universe(key); !known)
        return std::unexpected(known.error());
    std::vector<published_fixing> out;
    for (auto y = from.year(); y <= to.year(); ++y) {
        auto body = read_body(transport_,
                              std::format("{}/en/financial-markets/money-market/"
                                          "czeonia-reference-interest-rate/yearly.txt?"
                                          "calendarYear={}",
                                          base_url_,
                                          static_cast<int>(y)));
        if (!body)
            return std::unexpected(body.error());
        auto read = parse_cnb_czeonia(*body, key);
        if (!read)
            return std::unexpected(read.error());
        for (auto& f : *read)
            if (f.date >= from && f.date <= to)
                out.push_back(std::move(f));
    }
    return out;
}

}
