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
#include "ores.marketdata.official/domain/riksbank.hpp"
#include "ores.marketdata.official/domain/published_text.hpp"
#include <boost/json/parse.hpp>
#include <boost/json/value.hpp>
#include <algorithm>
#include <format>
#include <utility>

namespace ores::marketdata::official::domain {

std::expected<std::vector<published_fixing>, std::string> parse_swestr_rates(std::string_view json,
                                                                             std::string_view key) {
    boost::system::error_code ec;
    const auto doc = boost::json::parse(json, ec);
    if (ec)
        return std::unexpected("the rates are not JSON: " + ec.message());
    if (!doc.is_array())
        return std::unexpected("the answer is not a list of rates");
    std::vector<published_fixing> out;
    for (const auto& item : doc.as_array()) {
        const auto* row = item.if_object();
        if (row == nullptr)
            return std::unexpected("a rate is not an object");
        const auto* date = row->if_contains("date");
        const auto* rate = row->if_contains("rate");
        if (date == nullptr || !date->is_string() || rate == nullptr)
            return std::unexpected("a rate lacks its date or its rate");
        if (rate->is_null())
            continue;
        if (!rate->is_number())
            return std::unexpected("the rate of " + std::string(date->as_string()) +
                                   " is not a number");
        auto parsed = date_of(date->as_string());
        if (!parsed)
            return std::unexpected(parsed.error());
        auto decimal = decimal_of_percent(text_of(rate->to_number<double>()));
        if (!decimal)
            return std::unexpected(decimal.error());
        out.push_back({*parsed, std::string(key), std::move(*decimal)});
    }
    return out;
}

riksbank_fetcher::riksbank_fetcher(http::client::net::transport& transport, std::string base_url)
    : transport_(transport)
    , base_url_(std::move(base_url)) {
    while (!base_url_.empty() && base_url_.back() == '/')
        base_url_.pop_back();
}

std::expected<std::vector<std::string>, std::string>
riksbank_fetcher::universe(std::string_view key) {
    if (key.empty() || !std::ranges::all_of(key, [](char c) {
            return (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_';
        }))
        return std::unexpected("'" + std::string(key) +
                               "' is not a series name: it holds only capitals, digits and "
                               "underscores");
    return std::vector<std::string>{std::string(key)};
}

std::expected<std::vector<published_fixing>, std::string> riksbank_fetcher::fixings(
    std::string_view key, std::chrono::year_month_day from, std::chrono::year_month_day to) {
    if (auto known = universe(key); !known)
        return std::unexpected(known.error());
    auto body = read_window(
        transport_,
        std::format(
            "{}/swestr/v1/all/{}?fromDate={}&toDate={}", base_url_, key, iso(from), iso(to)),
        std::format("{}/swestr/v1/latest/{}", base_url_, key));
    if (!body)
        return std::unexpected(body.error());
    if (!*body)
        return std::vector<published_fixing>{};
    auto read = parse_swestr_rates(**body, key);
    if (!read)
        return std::unexpected(read.error());
    // The API honours the window; the filter keeps the fetcher's contract if it does not.
    std::erase_if(*read, [&](const published_fixing& f) { return f.date < from || f.date > to; });
    return read;
}

}
