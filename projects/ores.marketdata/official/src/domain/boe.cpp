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
#include "ores.marketdata.official/domain/boe.hpp"
#include "ores.marketdata.official/domain/published_text.hpp"
#include <algorithm>
#include <format>
#include <utility>

namespace ores::marketdata::official::domain {

namespace {

std::string_view trim(std::string_view s) {
    while (!s.empty() && (s.back() == '\r' || s.back() == ' '))
        s.remove_suffix(1);
    while (!s.empty() && s.front() == ' ')
        s.remove_prefix(1);
    return s;
}

/// A date as the database's query names it, such as =29/Oct/2026=.
std::string boe_query_date(std::chrono::year_month_day date) {
    return std::format("{:02}/{}/{}",
                       static_cast<unsigned>(date.day()),
                       month_name(date.month()),
                       static_cast<int>(date.year()));
}

}

std::expected<std::vector<published_fixing>, std::string> parse_boe_csv(std::string_view csv,
                                                                        std::string_view code) {
    constexpr std::string_view byte_order_mark = "\xEF\xBB\xBF";
    if (csv.starts_with(byte_order_mark))
        csv.remove_prefix(byte_order_mark.size());
    std::vector<std::string_view> lines;
    std::size_t start = 0;
    while (start < csv.size()) {
        const auto end = csv.find('\n', start);
        const auto line =
            trim(csv.substr(start, end == std::string_view::npos ? csv.npos : end - start));
        if (!line.empty())
            lines.push_back(line);
        if (end == std::string_view::npos)
            break;
        start = end + 1;
    }
    if (lines.empty())
        return std::unexpected("the answer is empty: it names no DATE column");
    if (lines.front() != "DATE," + std::string(code))
        return std::unexpected("the answer is not the CSV of the series '" + std::string(code) +
                               "': its header is '" + std::string(lines.front().substr(0, 80)) +
                               "'");
    std::vector<published_fixing> out;
    for (std::size_t i = 1; i < lines.size(); ++i) {
        const auto comma = lines[i].find(',');
        if (comma == std::string_view::npos)
            return std::unexpected("the row '" + std::string(lines[i]) + "' has no value");
        auto date = date_of_month_name(trim(lines[i].substr(0, comma)), ' ');
        if (!date)
            return std::unexpected(date.error());
        const auto text = trim(lines[i].substr(comma + 1));
        if (text.empty())
            continue;
        auto value = decimal_of_percent(text);
        if (!value)
            return std::unexpected(value.error());
        out.push_back({*date, std::string(code), std::move(*value)});
    }
    return out;
}

boe_fetcher::boe_fetcher(http::client::net::transport& transport, std::string base_url)
    : transport_(transport)
    , base_url_(std::move(base_url)) {
    while (!base_url_.empty() && base_url_.back() == '/')
        base_url_.pop_back();
}

std::expected<std::vector<std::string>, std::string> boe_fetcher::universe(std::string_view key) {
    if (key.empty() || !std::ranges::all_of(key, [](char c) {
            return (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9');
        }))
        return std::unexpected("'" + std::string(key) +
                               "' is not a series code: it holds only capitals and digits");
    return std::vector<std::string>{std::string(key)};
}

std::expected<std::vector<published_fixing>, std::string> boe_fetcher::fixings(
    std::string_view key, std::chrono::year_month_day from, std::chrono::year_month_day to) {
    if (auto known = universe(key); !known)
        return std::unexpected(known.error());
    auto body = read_body(
        transport_,
        std::format("{}/boeapps/database/_iadb-fromshowcolumns.asp?csv.x=yes&Datefrom={}&Dateto={}"
                    "&SeriesCodes={}&CSVF=TN&UsingCodes=Y&VPD=Y&VFD=N",
                    base_url_,
                    boe_query_date(from),
                    boe_query_date(to),
                    key));
    if (!body)
        return std::unexpected(body.error());
    auto read = parse_boe_csv(*body, key);
    if (!read)
        return std::unexpected(read.error());
    // The database honours the window; the filter keeps the fetcher's contract if it does not.
    std::erase_if(*read, [&](const published_fixing& f) { return f.date < from || f.date > to; });
    return read;
}

}
