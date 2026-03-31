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
#include "ores.ore/market/market_data_parser.hpp"

#include <chrono>
#include <istream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>

namespace ores::ore::market {

namespace {

/**
 * @brief Parses a date string in either YYYYMMDD or YYYY-MM-DD format.
 *
 * @param s The date string to parse.
 * @return The corresponding year_month_day.
 * @throws std::invalid_argument if the format is unrecognised or the date
 *         fields cannot be converted.
 */
std::chrono::year_month_day parse_date(std::string_view s) {
    if (s.size() == 8) {
        // YYYYMMDD
        const int y = std::stoi(std::string(s.substr(0, 4)));
        const unsigned m = static_cast<unsigned>(std::stoi(std::string(s.substr(4, 2))));
        const unsigned d = static_cast<unsigned>(std::stoi(std::string(s.substr(6, 2))));
        const std::chrono::year_month_day ymd{
            std::chrono::year{y},
            std::chrono::month{m},
            std::chrono::day{d}};
        if (!ymd.ok())
            throw std::invalid_argument("invalid date: " + std::string(s));
        return ymd;
    } else if (s.size() == 10 && s[4] == '-' && s[7] == '-') {
        // YYYY-MM-DD
        const int y = std::stoi(std::string(s.substr(0, 4)));
        const unsigned m = static_cast<unsigned>(std::stoi(std::string(s.substr(5, 2))));
        const unsigned d = static_cast<unsigned>(std::stoi(std::string(s.substr(8, 2))));
        const std::chrono::year_month_day ymd{
            std::chrono::year{y},
            std::chrono::month{m},
            std::chrono::day{d}};
        if (!ymd.ok())
            throw std::invalid_argument("invalid date: " + std::string(s));
        return ymd;
    }
    throw std::invalid_argument("unrecognised date format: " + std::string(s));
}

/**
 * @brief Tokenizes a data line into exactly three fields.
 *
 * Splits on whitespace.  The value field consumes the remainder of the line
 * after the first two tokens, stripped of leading whitespace.
 *
 * @return {date_str, key_str, value_str} or throws on fewer than 3 tokens.
 */
struct line_tokens {
    std::string_view date;
    std::string_view key;
    std::string_view value;
};

line_tokens tokenize(std::string_view line, int line_no) {
    // Skip leading whitespace
    auto skip_ws = [](std::string_view sv) -> std::string_view {
        const auto p = sv.find_first_not_of(" \t\r");
        return p == std::string_view::npos ? std::string_view{} : sv.substr(p);
    };
    auto next_token = [](std::string_view sv) -> std::pair<std::string_view, std::string_view> {
        const auto end = sv.find_first_of(" \t\r");
        if (end == std::string_view::npos)
            return {sv, {}};
        return {sv.substr(0, end), sv.substr(end)};
    };

    line = skip_ws(line);
    auto [date, rest1] = next_token(line);
    if (date.empty() || rest1.empty()) {
        throw std::invalid_argument(
            "line " + std::to_string(line_no) + ": fewer than 3 tokens: " +
            std::string(line));
    }

    rest1 = skip_ws(rest1);
    auto [key, rest2] = next_token(rest1);
    if (key.empty() || rest2.empty()) {
        throw std::invalid_argument(
            "line " + std::to_string(line_no) + ": fewer than 3 tokens: " +
            std::string(line));
    }

    const auto value = skip_ws(rest2);
    if (value.empty()) {
        throw std::invalid_argument(
            "line " + std::to_string(line_no) + ": fewer than 3 tokens: " +
            std::string(line));
    }

    return {date, key, value};
}

} // namespace

std::vector<market_datum> parse_market_data(std::istream& in) {
    std::vector<market_datum> result;
    std::string line;
    int line_no = 0;

    while (std::getline(in, line)) {
        ++line_no;
        std::string_view sv{line};

        // Strip trailing CR (Windows line endings)
        if (!sv.empty() && sv.back() == '\r')
            sv.remove_suffix(1);

        // Skip blank lines
        const auto first = sv.find_first_not_of(" \t\r");
        if (first == std::string_view::npos)
            continue;

        // Skip comment lines
        if (sv[first] == '#')
            continue;

        try {
            const auto [date_str, key_str, val_str] = tokenize(sv, line_no);
            market_datum datum;
            datum.date  = parse_date(date_str);
            datum.key   = std::string(key_str);
            datum.value = std::string(val_str);
            result.push_back(std::move(datum));
        } catch (const std::invalid_argument& ex) {
            throw std::invalid_argument(
                "line " + std::to_string(line_no) + ": " + ex.what());
        }
    }

    return result;
}

std::vector<fixing> parse_fixings(std::istream& in) {
    std::vector<fixing> result;
    std::string line;
    int line_no = 0;

    while (std::getline(in, line)) {
        ++line_no;
        std::string_view sv{line};

        if (!sv.empty() && sv.back() == '\r')
            sv.remove_suffix(1);

        const auto first = sv.find_first_not_of(" \t\r");
        if (first == std::string_view::npos)
            continue;

        if (sv[first] == '#')
            continue;

        try {
            const auto [date_str, idx_str, val_str] = tokenize(sv, line_no);
            fixing f;
            f.date       = parse_date(date_str);
            f.index_name = std::string(idx_str);
            f.value      = std::string(val_str);
            result.push_back(std::move(f));
        } catch (const std::invalid_argument& ex) {
            throw std::invalid_argument(
                "line " + std::to_string(line_no) + ": " + ex.what());
        }
    }

    return result;
}

} // namespace ores::ore::market
