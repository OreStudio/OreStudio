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
#include "ores.marketdata.official/domain/bcb.hpp"
#include "ores.marketdata.official/domain/published_text.hpp"
#include <boost/json/parse.hpp>
#include <boost/json/value.hpp>
#include <algorithm>
#include <format>
#include <iterator>
#include <utility>

namespace ores::marketdata::official::domain {

namespace {

/// The longest window the fetcher asks for, inside the API's ten years for a daily series.
constexpr std::chrono::days bcb_max_window{5 * 365};

std::string bcb_query_date(std::chrono::year_month_day date) {
    return std::format("{:02}/{:02}/{:04}",
                       static_cast<unsigned>(date.day()),
                       static_cast<unsigned>(date.month()),
                       static_cast<int>(date.year()));
}

}

std::expected<std::vector<published_fixing>, std::string> parse_bcb_series(std::string_view json,
                                                                           std::string_view key) {
    boost::system::error_code ec;
    const auto doc = boost::json::parse(json, ec);
    if (ec)
        return std::unexpected("the series is not JSON: " + ec.message());
    const auto* entries = doc.if_array();
    if (entries == nullptr)
        return std::unexpected("the answer is not an array of values");
    std::vector<published_fixing> out;
    for (const auto& item : *entries) {
        const auto* entry = item.if_object();
        const auto* date = entry == nullptr ? nullptr : entry->if_contains("data");
        const auto* value = entry == nullptr ? nullptr : entry->if_contains("valor");
        if (date == nullptr || !date->is_string() || value == nullptr || !value->is_string())
            return std::unexpected("an entry of the answer has no text data and valor");
        if (value->as_string().empty())
            continue;
        auto parsed = date_of_day_first(date->as_string(), '/');
        if (!parsed)
            return std::unexpected(parsed.error());
        auto decimal = decimal_of_percent(value->as_string());
        if (!decimal)
            return std::unexpected(decimal.error());
        out.push_back({*parsed, std::string(key), std::move(*decimal)});
    }
    return out;
}

bcb_fetcher::bcb_fetcher(http::client::net::transport& transport, std::string base_url)
    : transport_(transport)
    , base_url_(std::move(base_url)) {
    while (!base_url_.empty() && base_url_.back() == '/')
        base_url_.pop_back();
}

std::expected<std::vector<std::string>, std::string> bcb_fetcher::universe(std::string_view key) {
    if (key.empty() || key.size() > 9 ||
        !std::ranges::all_of(key, [](char c) { return c >= '0' && c <= '9'; }))
        return std::unexpected("'" + std::string(key) +
                               "' is not a series key: it needs a series number of one to nine "
                               "digits");
    return std::vector<std::string>{std::string(key)};
}

std::expected<std::vector<published_fixing>, std::string> bcb_fetcher::fixings(
    std::string_view key, std::chrono::year_month_day from, std::chrono::year_month_day to) {
    if (auto known = universe(key); !known)
        return std::unexpected(known.error());
    const auto series = std::format("{}/dados/serie/bcdata.sgs.{}/dados", base_url_, key);
    std::vector<published_fixing> out;
    for (auto start = std::chrono::sys_days{from}; start <= std::chrono::sys_days{to};
         start += bcb_max_window + std::chrono::days{1}) {
        const auto end = std::min(start + bcb_max_window, std::chrono::sys_days{to});
        auto body = read_window(transport_,
                                std::format("{}?formato=json&dataInicial={}&dataFinal={}",
                                            series,
                                            bcb_query_date(std::chrono::year_month_day{start}),
                                            bcb_query_date(std::chrono::year_month_day{end})),
                                std::format("{}/ultimos/1?formato=json", series));
        if (!body)
            return std::unexpected(body.error());
        if (!*body)
            continue;
        auto read = parse_bcb_series(**body, key);
        if (!read)
            return std::unexpected(read.error());
        std::ranges::move(*read, std::back_inserter(out));
    }
    std::erase_if(out, [&](const published_fixing& f) { return f.date < from || f.date > to; });
    return out;
}

}
