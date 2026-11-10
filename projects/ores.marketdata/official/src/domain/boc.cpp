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
#include "ores.marketdata.official/domain/boc.hpp"
#include "ores.marketdata.official/domain/published_text.hpp"
#include <boost/json/parse.hpp>
#include <boost/json/value.hpp>
#include <algorithm>
#include <format>
#include <utility>

namespace ores::marketdata::official::domain {

std::expected<std::vector<published_fixing>, std::string>
parse_valet_observations(std::string_view json, std::string_view series) {
    boost::system::error_code ec;
    const auto doc = boost::json::parse(json, ec);
    if (ec)
        return std::unexpected("the observations are not JSON: " + ec.message());
    const auto* top = doc.if_object();
    const auto* rows = top == nullptr ? nullptr : top->if_contains("observations");
    if (rows == nullptr || !rows->is_array())
        return std::unexpected("the answer holds no list of observations");
    std::vector<published_fixing> out;
    bool names_the_series = false;
    for (const auto& item : rows->as_array()) {
        const auto* row = item.if_object();
        if (row == nullptr)
            return std::unexpected("an observation is not an object");
        const auto* date = row->if_contains("d");
        if (date == nullptr || !date->is_string())
            return std::unexpected("an observation lacks its date");
        const auto* cell = row->if_contains(series);
        if (cell == nullptr)
            continue;
        names_the_series = true;
        const auto* value = cell->is_object() ? cell->as_object().if_contains("v") : nullptr;
        if (value == nullptr || !value->is_string())
            return std::unexpected("the observation of " + std::string(date->as_string()) +
                                   " holds no value as text");
        if (value->as_string().empty())
            continue;
        auto parsed = date_of(date->as_string());
        if (!parsed)
            return std::unexpected(parsed.error());
        auto decimal = decimal_of_percent(value->as_string());
        if (!decimal)
            return std::unexpected(decimal.error());
        out.push_back({*parsed, std::string(series), std::move(*decimal)});
    }
    if (!rows->as_array().empty() && !names_the_series)
        return std::unexpected("no observation names the series '" + std::string(series) + "'");
    return out;
}

boc_fetcher::boc_fetcher(http::client::net::transport& transport, std::string base_url)
    : transport_(transport)
    , base_url_(std::move(base_url)) {
    while (!base_url_.empty() && base_url_.back() == '/')
        base_url_.pop_back();
}

std::expected<std::vector<std::string>, std::string> boc_fetcher::universe(std::string_view key) {
    if (key.empty() || !std::ranges::all_of(key, [](char c) {
            return (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '.' || c == '_';
        }))
        return std::unexpected("'" + std::string(key) +
                               "' is not a series name: it holds only capitals, digits, dots "
                               "and underscores");
    return std::vector<std::string>{std::string(key)};
}

std::expected<std::vector<published_fixing>, std::string> boc_fetcher::fixings(
    std::string_view key, std::chrono::year_month_day from, std::chrono::year_month_day to) {
    if (auto known = universe(key); !known)
        return std::unexpected(known.error());
    auto body = read_body(transport_,
                          std::format("{}/valet/observations/{}/json?start_date={}&end_date={}",
                                      base_url_,
                                      key,
                                      iso(from),
                                      iso(to)));
    if (!body)
        return std::unexpected(body.error());
    auto read = parse_valet_observations(*body, key);
    if (!read)
        return std::unexpected(read.error());
    // The API honours the window; the filter keeps the fetcher's contract if it does not.
    std::erase_if(*read, [&](const published_fixing& f) { return f.date < from || f.date > to; });
    return read;
}

}
