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
#include "ores.marketdata.official/domain/published_text.hpp"
#include <algorithm>
#include <array>
#include <charconv>
#include <format>
#include <iterator>
#include <utility>

namespace ores::marketdata::official::domain {

namespace {

// Publishers write their months in English whatever the locale, so the table is fixed.
constexpr std::array<std::string_view, 12> month_names{
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

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

std::expected<std::chrono::year_month_day, std::string> date_of_month_name(std::string_view text,
                                                                           char separator) {
    const auto refused = [&] {
        return std::unexpected(std::format(
            "'{}' is not a date of the form 29{}Oct{}2026", text, separator, separator));
    };
    unsigned d = 0;
    int y = 0;
    if (text.size() != 11 || text[2] != separator || text[6] != separator ||
        std::from_chars(text.data(), text.data() + 2, d).ec != std::errc{} ||
        std::from_chars(text.data() + 7, text.data() + 11, y).ec != std::errc{})
        return refused();
    const auto m = std::ranges::find(month_names, text.substr(3, 3));
    if (m == month_names.end())
        return refused();
    const std::chrono::year_month_day date{
        std::chrono::year{y},
        std::chrono::month{static_cast<unsigned>(m - month_names.begin()) + 1},
        std::chrono::day{d}};
    if (!date.ok())
        return refused();
    return date;
}

std::string_view month_name(std::chrono::month month) {
    return month_names[static_cast<unsigned>(month) - 1];
}

std::expected<std::string, std::string> decimal_of_percent(std::string_view text) {
    const auto refused = [&] {
        return std::unexpected("'" + std::string(text) + "' is not a number");
    };
    std::string_view rest = text;
    std::string sign;
    if (!rest.empty() && (rest.front() == '-' || rest.front() == '+')) {
        if (rest.front() == '-')
            sign = "-";
        rest.remove_prefix(1);
    }
    int exponent = 0;
    if (const auto e = rest.find_first_of("eE"); e != std::string_view::npos) {
        auto power = rest.substr(e + 1);
        if (!power.empty() && power.front() == '+')
            power.remove_prefix(1);
        if (power.empty() ||
            std::from_chars(power.data(), power.data() + power.size(), exponent).ptr !=
                power.data() + power.size())
            return refused();
        rest = rest.substr(0, e);
    }
    const auto point = rest.find('.');
    const auto whole = rest.substr(0, point);
    const auto fraction =
        point == std::string_view::npos ? std::string_view{} : rest.substr(point + 1);
    std::string digits = std::string(whole) + std::string(fraction);
    if (digits.empty() || !std::ranges::all_of(digits, [](char c) { return c >= '0' && c <= '9'; }))
        return refused();

    // The position of the point within the digits, after a division by a hundred.
    const auto at = static_cast<long>(whole.size()) + exponent - 2;
    std::string integral;
    std::string decimals;
    if (at <= 0) {
        integral = "0";
        decimals = std::string(static_cast<std::size_t>(-at), '0') + digits;
    } else if (at >= static_cast<long>(digits.size())) {
        integral = digits + std::string(static_cast<std::size_t>(at) - digits.size(), '0');
    } else {
        integral = digits.substr(0, static_cast<std::size_t>(at));
        decimals = digits.substr(static_cast<std::size_t>(at));
    }
    while (integral.size() > 1 && integral.front() == '0')
        integral.erase(0, 1);
    while (!decimals.empty() && decimals.back() == '0')
        decimals.pop_back();
    if (integral == "0" && decimals.empty())
        sign.clear();
    return sign + integral + (decimals.empty() ? "" : "." + decimals);
}

std::string iso(std::chrono::year_month_day date) {
    return std::format("{:%F}", std::chrono::sys_days{date});
}

std::expected<std::string, std::string> read_body(http::client::net::transport& transport,
                                                  const std::string& url) {
    auto answer = transport.get(url);
    if (!answer)
        return std::unexpected(answer.error().message);
    if (answer->status != 200)
        return std::unexpected(std::format("{} answered {}", url, answer->status));
    return std::move(answer->body);
}

std::expected<std::optional<std::string>, std::string>
read_window(http::client::net::transport& transport,
            const std::string& window_url,
            const std::string& latest_url) {
    auto answer = transport.get(window_url);
    if (!answer)
        return std::unexpected(answer.error().message);
    if (answer->status == 200)
        return std::optional<std::string>(std::move(answer->body));
    if (answer->status != 404)
        return std::unexpected(std::format("{} answered {}", window_url, answer->status));
    auto latest = transport.get(latest_url);
    if (!latest)
        return std::unexpected(latest.error().message);
    if (latest->status != 200)
        return std::unexpected(
            std::format("{} answered 404 and {} answered {}: the series is unknown",
                        window_url,
                        latest_url,
                        latest->status));
    return std::optional<std::string>{};
}

std::vector<std::string_view> cells_of(std::string_view line, char separator) {
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
    if (text.starts_with("\xEF\xBB\xBF"))
        text.remove_prefix(3);
    std::vector<std::string_view> out;
    for (auto line : cells_of(text, '\n')) {
        if (!line.empty() && line.back() == '\r')
            line.remove_suffix(1);
        if (!line.empty())
            out.push_back(line);
    }
    return out;
}

std::optional<std::size_t> column_of(const std::vector<std::string_view>& header,
                                     std::string_view name) {
    const auto found = std::ranges::find(header, name);
    if (found == header.end())
        return std::nullopt;
    return static_cast<std::size_t>(found - header.begin());
}

}
