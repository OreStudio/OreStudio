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
#include "ores.ore.core/market/market_data_parser.hpp"
#include "ores.ore.core/market/series_key_registry.hpp"
#include "ores.platform/time/time_utils.hpp"
#include <istream>
#include <map>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::market {

namespace {

/**
 * @brief Drops all but the last occurrence of each (date, key) pair,
 *        preserving the file-order position of the surviving occurrence,
 *        and records each dropped occurrence in *report per on_duplicate.
 *
 * @tparam T     Element type (market_datum or fixing).
 * @tparam KeyOf Callable: const T& -> the second key field (raw key string
 *               or index_name).
 */
template <typename T, typename KeyOf>
std::vector<T> dedupe_by_date_and_key(std::vector<T> items,
                                      const std::vector<int>& line_numbers,
                                      duplicate_policy on_duplicate,
                                      parse_report* report,
                                      KeyOf key_of) {
    // Map (date, key) -> index of its LAST occurrence in `items`.
    std::map<std::pair<std::chrono::sys_days, std::string>, std::size_t> last_index;
    for (std::size_t i = 0; i < items.size(); ++i) {
        const auto composite = std::make_pair(std::chrono::sys_days{items[i].date}, key_of(items[i]));
        last_index[composite] = i;
    }

    std::vector<T> result;
    result.reserve(items.size());
    for (std::size_t i = 0; i < items.size(); ++i) {
        const auto composite = std::make_pair(std::chrono::sys_days{items[i].date}, key_of(items[i]));
        const auto winner = last_index.at(composite);
        if (winner != i) {
            // This occurrence is superseded by a later one at index `winner`.
            if (report) {
                auto& bucket =
                    on_duplicate == duplicate_policy::warn ? report->warnings : report->errors;
                bucket.push_back(
                    {line_numbers[i],
                     "duplicate key '" + key_of(items[i]) + "' — superseded by line " +
                         std::to_string(line_numbers[winner])});
            }
            continue;
        }
        result.push_back(std::move(items[i]));
    }
    return result;
}

/**
 * @brief Tokenizes a data line into exactly three fields.
 *
 * Auto-detects the delimiter: if the line contains two or more commas the
 * fields are split on commas (some ORE example files use CSV format);
 * otherwise fields are split on whitespace.  The value field consumes the
 * remainder of the line after the first two tokens.
 *
 * @return {date_str, key_str, value_str} or throws on fewer than 3 tokens.
 */
struct line_tokens {
    std::string_view date;
    std::string_view key;
    std::string_view value;
};

line_tokens tokenize(std::string_view line) {
    auto skip_ws = [](std::string_view sv) -> std::string_view {
        const auto p = sv.find_first_not_of(" \t\r");
        return p == std::string_view::npos ? std::string_view{} : sv.substr(p);
    };

    line = skip_ws(line);

    // Detect comma-delimited format: if there are at least two commas, split on commas.
    const auto c1 = line.find(',');
    if (c1 != std::string_view::npos) {
        const auto c2 = line.find(',', c1 + 1);
        if (c2 != std::string_view::npos) {
            const auto date = line.substr(0, c1);
            const auto key = line.substr(c1 + 1, c2 - c1 - 1);
            const auto value = line.substr(c2 + 1);
            if (date.empty() || key.empty() || value.empty())
                throw std::invalid_argument("fewer than 3 tokens: " + std::string(line));
            return {date, key, value};
        }
    }

    // Whitespace-delimited format.
    auto next_token = [](std::string_view sv) -> std::pair<std::string_view, std::string_view> {
        const auto end = sv.find_first_of(" \t\r");
        if (end == std::string_view::npos)
            return {sv, {}};
        return {sv.substr(0, end), sv.substr(end)};
    };

    auto [date, rest1] = next_token(line);
    if (date.empty() || rest1.empty())
        throw std::invalid_argument("fewer than 3 tokens: " + std::string(line));

    rest1 = skip_ws(rest1);
    auto [key, rest2] = next_token(rest1);
    if (key.empty() || rest2.empty())
        throw std::invalid_argument("fewer than 3 tokens: " + std::string(line));

    const auto value = skip_ws(rest2);
    if (value.empty())
        throw std::invalid_argument("fewer than 3 tokens: " + std::string(line));

    return {date, key, value};
}

} // namespace

std::vector<market_datum>
parse_market_data(std::istream& in, duplicate_policy on_duplicate, parse_report* report) {
    std::vector<market_datum> result;
    std::vector<int> line_numbers;
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
            const auto [date_str, key_str, val_str] = tokenize(sv);
            market_datum datum;
            datum.date = ores::platform::time::time_utils::parse_date(date_str);
            datum.key = std::string(key_str);
            datum.value = std::string(val_str);
            const auto dk = decompose_key(datum.key);
            datum.series_type = dk.series_type;
            datum.metric = dk.metric;
            datum.qualifier = dk.qualifier;
            datum.point_id = dk.point_id;
            result.push_back(std::move(datum));
            line_numbers.push_back(line_no);
        } catch (const std::invalid_argument& ex) {
            throw std::invalid_argument("line " + std::to_string(line_no) + ": " + ex.what());
        }
    }

    return dedupe_by_date_and_key(
        std::move(result), line_numbers, on_duplicate, report,
        [](const market_datum& d) { return d.key; });
}

std::vector<fixing>
parse_fixings(std::istream& in, duplicate_policy on_duplicate, parse_report* report) {
    std::vector<fixing> result;
    std::vector<int> line_numbers;
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
            const auto [date_str, idx_str, val_str] = tokenize(sv);
            fixing f;
            f.date = ores::platform::time::time_utils::parse_date(date_str);
            f.index_name = std::string(idx_str);
            f.qualifier = f.index_name;
            f.value = std::string(val_str);
            result.push_back(std::move(f));
            line_numbers.push_back(line_no);
        } catch (const std::invalid_argument& ex) {
            throw std::invalid_argument("line " + std::to_string(line_no) + ": " + ex.what());
        }
    }

    return dedupe_by_date_and_key(
        std::move(result), line_numbers, on_duplicate, report,
        [](const fixing& f) { return f.index_name; });
}

} // namespace ores::ore::market
