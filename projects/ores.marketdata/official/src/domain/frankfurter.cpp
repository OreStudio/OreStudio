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
#include "ores.marketdata.official/domain/frankfurter.hpp"
#include <boost/json/parse.hpp>
#include <boost/json/value.hpp>
#include <algorithm>
#include <charconv>
#include <format>
#include <utility>

namespace ores::marketdata::official::domain {

namespace {

std::string lower(std::string_view s) {
    std::string out(s);
    std::ranges::transform(out, out.begin(), [](unsigned char c) {
        return static_cast<char>(c >= 'A' && c <= 'Z' ? c - 'A' + 'a' : c);
    });
    return out;
}

std::string text_of(double value) {
    char buffer[32];
    const auto [end, ec] = std::to_chars(std::begin(buffer), std::end(buffer), value);
    return ec == std::errc{} ? std::string(buffer, end) : std::format("{}", value);
}

std::expected<std::chrono::year_month_day, std::string> date_of(std::string_view text) {
    int y = 0;
    unsigned m = 0;
    unsigned d = 0;
    if (text.size() != 10 || text[4] != '-' || text[7] != '-' ||
        std::from_chars(text.data(), text.data() + 4, y).ec != std::errc{} ||
        std::from_chars(text.data() + 5, text.data() + 7, m).ec != std::errc{} ||
        std::from_chars(text.data() + 8, text.data() + 10, d).ec != std::errc{})
        return std::unexpected("'" + std::string(text) + "' is not a date");
    const std::chrono::year_month_day date{
        std::chrono::year{y}, std::chrono::month{m}, std::chrono::day{d}};
    if (!date.ok())
        return std::unexpected("'" + std::string(text) + "' is not a date");
    return date;
}

std::string iso(std::chrono::year_month_day date) {
    return std::format("{:%F}", std::chrono::sys_days{date});
}

}

std::expected<std::vector<published_rate>, std::string> parse_rates(std::string_view json) {
    boost::system::error_code ec;
    const auto doc = boost::json::parse(json, ec);
    if (ec)
        return std::unexpected("the rates are not JSON: " + ec.message());
    if (!doc.is_array())
        return std::unexpected("the rates are not a list");
    std::vector<published_rate> out;
    for (const auto& item : doc.as_array()) {
        const auto* row = item.if_object();
        if (row == nullptr)
            return std::unexpected("a rate is not an object");
        const auto* date = row->if_contains("date");
        const auto* base = row->if_contains("base");
        const auto* quote = row->if_contains("quote");
        const auto* rate = row->if_contains("rate");
        if (date == nullptr || !date->is_string() || base == nullptr || !base->is_string() ||
            quote == nullptr || !quote->is_string() || rate == nullptr || !rate->is_number())
            return std::unexpected("a rate lacks its date, base, quote or rate");
        if (base->as_string() == quote->as_string())
            continue;
        auto parsed = date_of(date->as_string());
        if (!parsed)
            return std::unexpected(parsed.error());
        out.push_back({*parsed,
                       std::string(base->as_string()),
                       std::string(quote->as_string()),
                       text_of(rate->to_number<double>())});
    }
    return out;
}

std::expected<std::vector<std::string>, std::string> parse_currencies(std::string_view json,
                                                                      std::string_view key) {
    boost::system::error_code ec;
    const auto doc = boost::json::parse(json, ec);
    if (ec)
        return std::unexpected("the providers are not JSON: " + ec.message());
    if (!doc.is_array())
        return std::unexpected("the providers are not a list");
    const auto wanted = lower(key);
    for (const auto& item : doc.as_array()) {
        const auto* row = item.if_object();
        if (row == nullptr)
            continue;
        const auto* k = row->if_contains("key");
        if (k == nullptr || !k->is_string() || lower(k->as_string()) != wanted)
            continue;
        const auto* pivot = row->if_contains("pivot_currency");
        const auto* currencies = row->if_contains("currencies");
        if (currencies == nullptr || !currencies->is_array())
            return std::unexpected("the provider '" + std::string(key) + "' lists no currencies");
        std::vector<std::string> out;
        for (const auto& c : currencies->as_array())
            if (c.is_string() &&
                (pivot == nullptr || !pivot->is_string() || c.as_string() != pivot->as_string()))
                out.emplace_back(c.as_string());
        return out;
    }
    return std::unexpected("Frankfurter lists no provider '" + std::string(key) + "'");
}

frankfurter_fetcher::frankfurter_fetcher(http::client::net::transport& transport,
                                         std::string base_url)
    : transport_(transport)
    , base_url_(std::move(base_url)) {
    while (!base_url_.empty() && base_url_.back() == '/')
        base_url_.pop_back();
}

std::expected<std::string, std::string> frankfurter_fetcher::read(const std::string& url) {
    auto answer = transport_.get(url);
    if (!answer)
        return std::unexpected(answer.error().message);
    if (answer->status != 200)
        return std::unexpected(std::format("{} answered {}", url, answer->status));
    return std::move(answer->body);
}

std::expected<std::vector<std::string>, std::string>
frankfurter_fetcher::currencies(std::string_view key) {
    auto body = read(base_url_ + "/v2/providers");
    if (!body)
        return std::unexpected(body.error());
    return parse_currencies(*body, key);
}

std::expected<std::vector<published_rate>, std::string> frankfurter_fetcher::rates(
    std::string_view key, std::chrono::year_month_day from, std::chrono::year_month_day to) {
    auto body = read(std::format(
        "{}/v2/providers/{}/rates?from={}&to={}", base_url_, lower(key), iso(from), iso(to)));
    if (!body)
        return std::unexpected(body.error());
    return parse_rates(*body);
}

}
