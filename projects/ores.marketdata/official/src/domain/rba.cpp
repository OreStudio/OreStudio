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
#include "ores.marketdata.official/domain/rba.hpp"
#include "ores.marketdata.official/domain/published_text.hpp"
#include <algorithm>
#include <format>
#include <utility>

namespace ores::marketdata::official::domain {

std::expected<std::vector<published_fixing>, std::string>
parse_rba_table(std::string_view csv, std::string_view series, std::string_view key) {
    const auto lines = lines_of(csv);
    const auto header = std::ranges::find_if(
        lines, [](std::string_view line) { return line.starts_with("Series ID,"); });
    if (header == lines.end())
        return std::unexpected("the answer is not an RBA table: it has no Series ID row");
    const auto value_at = column_of(cells_of(*header, ','), series);
    if (!value_at)
        return std::unexpected("the table has no series '" + std::string(series) + "'");
    std::vector<published_fixing> out;
    for (auto line = std::next(header); line != lines.end(); ++line) {
        const auto cells = cells_of(*line, ',');
        auto date = date_of_month_name(cells.front(), '-');
        if (!date)
            return std::unexpected(date.error());
        if (cells.size() <= *value_at || cells[*value_at].empty())
            continue;
        auto value = decimal_of_percent(cells[*value_at]);
        if (!value)
            return std::unexpected(value.error());
        out.push_back({*date, std::string(key), std::move(*value)});
    }
    return out;
}

rba_fetcher::rba_fetcher(http::client::net::transport& transport, std::string base_url)
    : transport_(transport)
    , base_url_(std::move(base_url)) {
    while (!base_url_.empty() && base_url_.back() == '/')
        base_url_.pop_back();
}

std::expected<std::vector<std::string>, std::string> rba_fetcher::universe(std::string_view key) {
    const auto dot = key.rfind('.');
    const auto table = dot == std::string_view::npos ? std::string_view{} : key.substr(0, dot);
    const auto series = dot == std::string_view::npos ? std::string_view{} : key.substr(dot + 1);
    const auto is_table =
        !table.empty() && table.front() >= 'a' && table.front() <= 'z' &&
        std::ranges::all_of(table, [](char c) {
            return (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '.' || c == '-';
        });
    const auto is_series = !series.empty() && std::ranges::all_of(series, [](char c) {
        return (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9');
    });
    if (!is_table || !is_series)
        return std::unexpected(
            "'" + std::string(key) +
            "' is not a series key: it needs a table code of small letters, "
            "digits, dots and hyphens, a dot, and a series of capitals and digits");
    return std::vector<std::string>{std::string(key)};
}

std::expected<std::vector<published_fixing>, std::string> rba_fetcher::fixings(
    std::string_view key, std::chrono::year_month_day from, std::chrono::year_month_day to) {
    if (auto known = universe(key); !known)
        return std::unexpected(known.error());
    const auto dot = key.rfind('.');
    auto body = read_body(
        transport_,
        std::format("{}/statistics/tables/csv/{}-data.csv", base_url_, key.substr(0, dot)));
    if (!body)
        return std::unexpected(body.error());
    auto read = parse_rba_table(*body, key.substr(dot + 1), key);
    if (!read)
        return std::unexpected(read.error());
    std::erase_if(*read, [&](const published_fixing& f) { return f.date < from || f.date > to; });
    return read;
}

}
