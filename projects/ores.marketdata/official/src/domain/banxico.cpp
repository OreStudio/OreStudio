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
#include "ores.marketdata.official/domain/banxico.hpp"
#include "ores.marketdata.official/domain/published_text.hpp"
#include <boost/json/parse.hpp>
#include <boost/json/value.hpp>
#include <algorithm>
#include <format>
#include <utility>

namespace ores::marketdata::official::domain {

std::expected<std::vector<published_fixing>, std::string>
parse_banxico_series(std::string_view json, std::string_view key) {
    boost::system::error_code ec;
    const auto doc = boost::json::parse(json, ec);
    if (ec)
        return std::unexpected("the series is not JSON: " + ec.message());
    const auto* root = doc.if_object();
    const auto* bmx = root == nullptr ? nullptr : root->if_contains("bmx");
    const auto* series =
        bmx == nullptr || !bmx->is_object() ? nullptr : bmx->as_object().if_contains("series");
    if (series == nullptr || !series->is_array() || series->as_array().size() != 1 ||
        !series->as_array()[0].is_object())
        return std::unexpected("the answer holds no single series in bmx.series");
    const auto& one = series->as_array()[0].as_object();
    const auto* id = one.if_contains("idSerie");
    if (id == nullptr || !id->is_string() || id->as_string() != key)
        return std::unexpected("the answer is not of the series " + std::string(key));
    std::vector<published_fixing> out;
    const auto* data = one.if_contains("datos");
    if (data == nullptr)
        return out;
    if (!data->is_array())
        return std::unexpected("the datos of the series is not an array");
    for (const auto& item : data->as_array()) {
        const auto* entry = item.if_object();
        const auto* date = entry == nullptr ? nullptr : entry->if_contains("fecha");
        const auto* value = entry == nullptr ? nullptr : entry->if_contains("dato");
        if (date == nullptr || !date->is_string() || value == nullptr || !value->is_string())
            return std::unexpected("an entry of the series has no text fecha and dato");
        if (value->as_string() == "N/E")
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

banxico_fetcher::banxico_fetcher(http::client::net::transport& transport,
                                 std::string base_url,
                                 std::expected<std::string, std::string> token)
    : transport_(transport)
    , base_url_(std::move(base_url))
    , token_(std::move(token)) {
    while (!base_url_.empty() && base_url_.back() == '/')
        base_url_.pop_back();
}

std::expected<std::vector<std::string>, std::string>
banxico_fetcher::universe(std::string_view key) {
    const auto letters = std::ranges::find_if(key, [](char c) { return c < 'A' || c > 'Z'; });
    const auto prefix = static_cast<std::size_t>(letters - key.begin());
    if (prefix == 0 || prefix > 4 || key.size() - prefix == 0 || key.size() > 16 ||
        !std::all_of(letters, key.end(), [](char c) { return c >= '0' && c <= '9'; }))
        return std::unexpected("'" + std::string(key) +
                               "' is not a series id: it needs one to four capital letters "
                               "and then digits, such as SF331451");
    if (!token_)
        return std::unexpected("Banco de Mexico needs a token: " + token_.error());
    return std::vector<std::string>{std::string(key)};
}

std::expected<std::vector<published_fixing>, std::string> banxico_fetcher::fixings(
    std::string_view key, std::chrono::year_month_day from, std::chrono::year_month_day to) {
    if (auto known = universe(key); !known)
        return std::unexpected(known.error());
    auto body = read_body(transport_,
                          std::format("{}/SieAPIRest/service/v1/series/{}/datos/{}/{}?token={}",
                                      base_url_,
                                      key,
                                      iso(from),
                                      iso(to),
                                      *token_));
    if (!body)
        return std::unexpected(body.error());
    auto read = parse_banxico_series(*body, key);
    if (!read)
        return std::unexpected(read.error());
    std::erase_if(*read, [&](const published_fixing& f) { return f.date < from || f.date > to; });
    return read;
}

}
