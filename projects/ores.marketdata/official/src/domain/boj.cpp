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
#include "ores.marketdata.official/domain/boj.hpp"
#include "ores.marketdata.official/domain/published_text.hpp"
#include <boost/json/parse.hpp>
#include <boost/json/value.hpp>
#include <algorithm>
#include <cstdint>
#include <format>
#include <utility>

namespace ores::marketdata::official::domain {

namespace {

bool is_code(std::string_view text) {
    return !text.empty() && std::ranges::all_of(text, [](char c) {
        return (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_';
    });
}

std::expected<std::chrono::year_month_day, std::string> date_of_number(std::int64_t number) {
    const std::chrono::year_month_day date{
        std::chrono::year{static_cast<int>(number / 10000)},
        std::chrono::month{static_cast<unsigned>(number / 100 % 100)},
        std::chrono::day{static_cast<unsigned>(number % 100)}};
    if (number < 10000101 || number > 99991231 || !date.ok())
        return std::unexpected(std::format("'{}' is not a date", number));
    return date;
}

std::string month_of(std::chrono::year_month_day date) {
    return std::format(
        "{:04}{:02}", static_cast<int>(date.year()), static_cast<unsigned>(date.month()));
}

}

std::expected<std::vector<published_fixing>, std::string> parse_boj_series(std::string_view json,
                                                                           std::string_view key) {
    const auto dot = key.find('.');
    const auto code = dot == std::string_view::npos ? key : key.substr(dot + 1);
    boost::system::error_code ec;
    const auto doc = boost::json::parse(json, ec);
    if (ec)
        return std::unexpected("the series is not JSON: " + ec.message());
    const auto* top = doc.if_object();
    if (top == nullptr)
        return std::unexpected("the answer is not an object");
    const auto* status = top->if_contains("STATUS");
    if (status == nullptr || !status->is_int64() || status->as_int64() != 200) {
        const auto* message = top->if_contains("MESSAGE");
        return std::unexpected("the answer is not a success: " +
                               (message != nullptr && message->is_string() ?
                                    std::string(message->as_string()) :
                                    std::string("it gives no STATUS 200")));
    }
    const auto* next = top->if_contains("NEXTPOSITION");
    if (next != nullptr && !next->is_null())
        return std::unexpected("the answer is paged, and a series is read in one answer");
    const auto* results = top->if_contains("RESULTSET");
    if (results == nullptr || !results->is_array())
        return std::unexpected("the answer holds no RESULTSET");
    const boost::json::object* series = nullptr;
    for (const auto& item : results->as_array()) {
        const auto* entry = item.if_object();
        const auto* name = entry == nullptr ? nullptr : entry->if_contains("SERIES_CODE");
        if (name != nullptr && name->is_string() && name->as_string() == code)
            series = entry;
    }
    if (series == nullptr)
        return std::unexpected("no entry of the answer names the series '" + std::string(code) +
                               "'");
    const auto* values = series->if_contains("VALUES");
    const auto* dates = values != nullptr && values->is_object() ?
                            values->as_object().if_contains("SURVEY_DATES") :
                            nullptr;
    const auto* numbers = values != nullptr && values->is_object() ?
                              values->as_object().if_contains("VALUES") :
                              nullptr;
    if (dates == nullptr || !dates->is_array() || numbers == nullptr || !numbers->is_array())
        return std::unexpected("the series holds no SURVEY_DATES and VALUES");
    if (dates->as_array().size() != numbers->as_array().size())
        return std::unexpected(std::format("the series has {} dates and {} values",
                                           dates->as_array().size(),
                                           numbers->as_array().size()));
    std::vector<published_fixing> out;
    for (std::size_t i = 0; i < dates->as_array().size(); ++i) {
        const auto& date = dates->as_array()[i];
        const auto& number = numbers->as_array()[i];
        if (!date.is_int64())
            return std::unexpected("a date of the series is not a number");
        if (number.is_null())
            continue;
        if (!number.is_number())
            return std::unexpected(std::format("the value of {} is not a number", date.as_int64()));
        auto parsed = date_of_number(date.as_int64());
        if (!parsed)
            return std::unexpected(parsed.error());
        auto decimal = decimal_of_percent(text_of(number.to_number<double>()));
        if (!decimal)
            return std::unexpected(decimal.error());
        out.push_back({*parsed, std::string(key), std::move(*decimal)});
    }
    return out;
}

boj_fetcher::boj_fetcher(http::client::net::transport& transport, std::string base_url)
    : transport_(transport)
    , base_url_(std::move(base_url)) {
    while (!base_url_.empty() && base_url_.back() == '/')
        base_url_.pop_back();
}

std::expected<std::vector<std::string>, std::string> boj_fetcher::universe(std::string_view key) {
    const auto dot = key.find('.');
    if (dot == std::string_view::npos || !is_code(key.substr(0, dot)) ||
        !is_code(key.substr(dot + 1)))
        return std::unexpected("'" + std::string(key) +
                               "' is not a series key: it needs a database, a dot and a series "
                               "code, of capitals, digits and underscores");
    return std::vector<std::string>{std::string(key)};
}

std::expected<std::vector<published_fixing>, std::string> boj_fetcher::fixings(
    std::string_view key, std::chrono::year_month_day from, std::chrono::year_month_day to) {
    if (auto known = universe(key); !known)
        return std::unexpected(known.error());
    const auto dot = key.find('.');
    auto body =
        read_body(transport_,
                  std::format("{}/api/v1/getDataCode?format=json&lang=en&db={}&code={}&startDate={}"
                              "&endDate={}",
                              base_url_,
                              key.substr(0, dot),
                              key.substr(dot + 1),
                              month_of(from),
                              month_of(to)));
    if (!body)
        return std::unexpected(body.error());
    auto read = parse_boj_series(*body, key);
    if (!read)
        return std::unexpected(read.error());
    std::erase_if(*read, [&](const published_fixing& f) { return f.date < from || f.date > to; });
    return read;
}

}
