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
#include "ores.marketdata.official/domain/ecb.hpp"
#include "ores.marketdata.official/domain/published_text.hpp"
#include <algorithm>
#include <format>
#include <optional>
#include <utility>

namespace ores::marketdata::official::domain {

namespace {

constexpr int attempts = 3;

/// Longer than the thirty seconds after which the portal's gateway answers 504.
const http::client::net::request_limits limits{std::chrono::seconds(60)};

// The answer of detail=dataonly holds codes, dates and numbers, never quoted text, so a comma
// always separates two cells.
std::vector<std::string_view> split(std::string_view line, char separator) {
    std::vector<std::string_view> out;
    std::size_t start = 0;
    for (auto end = line.find(separator); end != std::string_view::npos;
         end = line.find(separator, start)) {
        out.push_back(line.substr(start, end - start));
        start = end + 1;
    }
    out.push_back(line.substr(start));
    return out;
}

std::vector<std::string_view> lines_of(std::string_view text) {
    std::vector<std::string_view> out;
    for (auto line : split(text, '\n')) {
        if (!line.empty() && line.back() == '\r')
            line.remove_suffix(1);
        if (!line.empty())
            out.push_back(line);
    }
    return out;
}

std::optional<std::size_t> column(const std::vector<std::string_view>& header,
                                  std::string_view name) {
    const auto found = std::ranges::find(header, name);
    if (found == header.end())
        return std::nullopt;
    return static_cast<std::size_t>(found - header.begin());
}

}

std::expected<std::vector<published_fixing>, std::string> parse_ecb_csv(std::string_view csv,
                                                                        std::string_view key) {
    const auto lines = lines_of(csv);
    if (lines.empty())
        return std::vector<published_fixing>{};
    const auto header = split(lines.front(), ',');
    const auto key_at = column(header, "KEY");
    const auto date_at = column(header, "TIME_PERIOD");
    const auto value_at = column(header, "OBS_VALUE");
    if (!key_at || !date_at || !value_at)
        return std::unexpected("the answer is not the CSV of a series: it names no KEY, "
                               "TIME_PERIOD or OBS_VALUE column");
    const auto width = std::max({*key_at, *date_at, *value_at}) + 1;
    std::vector<published_fixing> out;
    for (std::size_t i = 1; i < lines.size(); ++i) {
        const auto cells = split(lines[i], ',');
        if (cells.size() < width)
            return std::unexpected(std::format(
                "row {} has {} cells, fewer than the {} its header needs", i, cells.size(), width));
        if (cells[*key_at] != key || cells[*value_at].empty())
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

ecb_fetcher::ecb_fetcher(http::client::net::transport& transport, std::string base_url)
    : transport_(transport)
    , base_url_(std::move(base_url)) {
    while (!base_url_.empty() && base_url_.back() == '/')
        base_url_.pop_back();
}

std::expected<std::vector<std::string>, std::string> ecb_fetcher::universe(std::string_view key) {
    const auto dot = key.find('.');
    if (dot == std::string_view::npos || dot == 0 || dot + 1 == key.size())
        return std::unexpected("'" + std::string(key) +
                               "' is not a series key: it needs a dataflow, a dot and a key");
    return std::vector<std::string>{std::string(key)};
}

std::expected<std::vector<published_fixing>, std::string> ecb_fetcher::fixings(
    std::string_view key, std::chrono::year_month_day from, std::chrono::year_month_day to) {
    if (auto known = universe(key); !known)
        return std::unexpected(known.error());
    const auto dot = key.find('.');
    const auto url = std::format(
        "{}/service/data/{}/{}?startPeriod={}&endPeriod={}&format=csvdata&detail=dataonly",
        base_url_,
        key.substr(0, dot),
        key.substr(dot + 1),
        iso(from),
        iso(to));
    std::string last_error;
    for (int attempt = 0; attempt < attempts; ++attempt) {
        auto answer = transport_.get(url, limits);
        if (!answer) {
            last_error = answer.error().message;
            continue;
        }
        if (answer->status >= 500) {
            last_error = std::format("{} answered {}", url, answer->status);
            continue;
        }
        // A window with no observation is a 200 with an empty body; a 404 names a series that
        // does not exist, so it is an error.
        if (answer->status != 200)
            return std::unexpected(std::format("{} answered {}", url, answer->status));
        auto read = parse_ecb_csv(answer->body, key);
        if (!read)
            return std::unexpected(read.error());
        // The portal honours the window; the filter keeps the fetcher's contract if it does not.
        std::erase_if(*read,
                      [&](const published_fixing& f) { return f.date < from || f.date > to; });
        return read;
    }
    return std::unexpected(std::format("{} (after {} attempts)", last_error, attempts));
}

}
