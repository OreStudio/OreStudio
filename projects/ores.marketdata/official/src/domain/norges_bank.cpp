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
#include "ores.marketdata.official/domain/norges_bank.hpp"
#include "ores.marketdata.official/domain/published_text.hpp"
#include <algorithm>
#include <format>
#include <utility>

namespace ores::marketdata::official::domain {

std::expected<std::vector<published_fixing>, std::string>
parse_norges_bank_csv(std::string_view csv, std::string_view key) {
    const auto lines = lines_of(csv);
    if (lines.empty())
        return std::vector<published_fixing>{};
    const auto header = cells_of(lines.front(), ';');
    const auto date_at = column_of(header, "TIME_PERIOD");
    const auto value_at = column_of(header, "OBS_VALUE");
    if (!date_at || !value_at)
        return std::unexpected(
            "the answer is not the CSV of a series: it names no TIME_PERIOD or OBS_VALUE column");
    const auto width = std::max(*date_at, *value_at) + 1;
    std::vector<published_fixing> out;
    for (std::size_t i = 1; i < lines.size(); ++i) {
        const auto cells = cells_of(lines[i], ';');
        if (cells.size() < width)
            return std::unexpected(std::format(
                "row {} has {} cells, fewer than the {} its header needs", i, cells.size(), width));
        if (cells[*value_at].empty())
            continue;
        auto date = date_of(cells[*date_at]);
        if (!date)
            return std::unexpected(date.error());
        auto value = decimal_of_percent(cells[*value_at]);
        if (!value)
            return std::unexpected(value.error());
        out.push_back({*date, std::string(key), std::move(*value)});
    }
    return out;
}

norges_bank_fetcher::norges_bank_fetcher(http::client::net::transport& transport,
                                         std::string base_url)
    : transport_(transport)
    , base_url_(std::move(base_url)) {
    while (!base_url_.empty() && base_url_.back() == '/')
        base_url_.pop_back();
}

std::expected<std::vector<std::string>, std::string>
norges_bank_fetcher::universe(std::string_view key) {
    const auto refused = [&] {
        return std::unexpected("'" + std::string(key) +
                               "' is not a series key: it needs a dataflow, a dot and a key, "
                               "each part of capitals, digits and underscores");
    };
    const auto parts = cells_of(key, '.');
    if (parts.size() < 2)
        return refused();
    for (const auto part : parts)
        if (part.empty() || !std::ranges::all_of(part, [](char c) {
                return (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_';
            }))
            return refused();
    return std::vector<std::string>{std::string(key)};
}

std::expected<std::vector<published_fixing>, std::string> norges_bank_fetcher::fixings(
    std::string_view key, std::chrono::year_month_day from, std::chrono::year_month_day to) {
    if (auto known = universe(key); !known)
        return std::unexpected(known.error());
    const auto dot = key.find('.');
    const auto series =
        std::format("{}/api/data/{}/{}", base_url_, key.substr(0, dot), key.substr(dot + 1));
    auto body = read_window(
        transport_,
        std::format(
            "{}?format=csv&startPeriod={}&endPeriod={}&locale=en", series, iso(from), iso(to)),
        std::format("{}?format=csv&lastNObservations=1&locale=en", series));
    if (!body)
        return std::unexpected(body.error());
    if (!*body)
        return std::vector<published_fixing>{};
    auto read = parse_norges_bank_csv(**body, key);
    if (!read)
        return std::unexpected(read.error());
    // The API honours the window; the filter keeps the fetcher's contract if it does not.
    std::erase_if(*read, [&](const published_fixing& f) { return f.date < from || f.date > to; });
    return read;
}

}
