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
#include "ores.marketdata.official/domain/nyfed.hpp"
#include "ores.marketdata.official/domain/published_text.hpp"
#include <boost/json/parse.hpp>
#include <boost/json/value.hpp>
#include <format>
#include <map>
#include <utility>

namespace ores::marketdata::official::domain {

namespace {

/// The path of each rate type under =/api/rates=.
const std::map<std::string, std::string, std::less<>> rate_paths{
    {"SOFR", "secured/sofr"},
};

}

std::expected<std::vector<published_fixing>, std::string> parse_nyfed_rates(std::string_view json) {
    boost::system::error_code ec;
    const auto doc = boost::json::parse(json, ec);
    if (ec)
        return std::unexpected("the rates are not JSON: " + ec.message());
    const auto* top = doc.if_object();
    const auto* rows = top == nullptr ? nullptr : top->if_contains("refRates");
    if (rows == nullptr || !rows->is_array())
        return std::unexpected("the answer holds no list of refRates");
    std::vector<published_fixing> out;
    for (const auto& item : rows->as_array()) {
        const auto* row = item.if_object();
        if (row == nullptr)
            return std::unexpected("a rate is not an object");
        const auto* date = row->if_contains("effectiveDate");
        const auto* type = row->if_contains("type");
        const auto* rate = row->if_contains("percentRate");
        if (date == nullptr || !date->is_string() || type == nullptr || !type->is_string() ||
            rate == nullptr || !rate->is_number())
            return std::unexpected("a rate lacks its effectiveDate, type or percentRate");
        auto parsed = date_of(date->as_string());
        if (!parsed)
            return std::unexpected(parsed.error());
        auto value = decimal_of_percent(text_of(rate->to_number<double>()));
        if (!value)
            return std::unexpected(value.error());
        out.push_back({*parsed, std::string(type->as_string()), std::move(*value)});
    }
    return out;
}

nyfed_fetcher::nyfed_fetcher(http::client::net::transport& transport, std::string base_url)
    : transport_(transport)
    , base_url_(std::move(base_url)) {
    while (!base_url_.empty() && base_url_.back() == '/')
        base_url_.pop_back();
}

std::expected<std::vector<std::string>, std::string> nyfed_fetcher::universe(std::string_view key) {
    if (!rate_paths.contains(key))
        return std::unexpected("the New York Fed fetcher knows no rate '" + std::string(key) + "'");
    return std::vector<std::string>{std::string(key)};
}

std::expected<std::vector<published_fixing>, std::string> nyfed_fetcher::fixings(
    std::string_view key, std::chrono::year_month_day from, std::chrono::year_month_day to) {
    const auto path = rate_paths.find(key);
    if (path == rate_paths.end())
        return std::unexpected("the New York Fed fetcher knows no rate '" + std::string(key) + "'");
    auto body = read_body(transport_,
                          std::format("{}/api/rates/{}/search.json?startDate={}&endDate={}",
                                      base_url_,
                                      path->second,
                                      iso(from),
                                      iso(to)));
    if (!body)
        return std::unexpected(body.error());
    auto rates = parse_nyfed_rates(*body);
    if (!rates)
        return std::unexpected(rates.error());
    std::erase_if(*rates, [&](const published_fixing& f) {
        return f.external_id != key || f.date < from || f.date > to;
    });
    return rates;
}

}
