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
#ifndef ORES_TESTING_SQL_VALUES_ROWS_HPP
#define ORES_TESTING_SQL_VALUES_ROWS_HPP

#include <cctype>
#include <cstddef>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::testing {

/**
 * @brief The column texts of every row in the `values` list of a populate
 *        script.
 *
 * A test that restates a seed would pass after the seed changed, which is the
 * drift reading the file catches. The script is the same one the database
 * loader runs, so a row that parses here is a row that lands in the table.
 *
 * Quotes are resolved and `--` comments are skipped, both between rows and
 * inside one. A `null` keyword stays as the text `null`, because only the
 * caller knows which of its columns admit it. Column positions are the
 * caller's business: a caller that knows its own script's column list indexes
 * this result by position.
 */
inline std::vector<std::vector<std::string>> sql_values_rows(const std::string& sql,
                                                             const std::string& path) {
    constexpr std::string_view values_keyword = "\nvalues";
    const auto values = sql.find(values_keyword);
    if (values == std::string::npos)
        throw std::invalid_argument("no `values` list in " + path);

    // A `--` inside a quoted description is text, not a comment, so this is
    // only ever called with the scanner outside a string.
    const auto skip_comment = [&sql](std::size_t i) {
        const auto nl = sql.find('\n', i);
        return nl == std::string::npos ? sql.size() : nl + 1;
    };
    const auto trim = [](std::string s) {
        const auto first = s.find_first_not_of(" \t\r\n");
        if (first == std::string::npos)
            return std::string{};
        return s.substr(first, s.find_last_not_of(" \t\r\n") - first + 1);
    };

    std::vector<std::vector<std::string>> rows;
    std::size_t i = values + values_keyword.size();
    while (i < sql.size()) {
        if (sql[i] == '-' && i + 1 < sql.size() && sql[i + 1] == '-') {
            i = skip_comment(i);
            continue;
        }
        if (std::isspace(static_cast<unsigned char>(sql[i])) || sql[i] == ',') {
            ++i;
            continue;
        }
        if (sql[i] != '(') {
            // The statement ended before this character; anything else here
            // is the tail of the script.
            break;
        }

        std::vector<std::string> columns;
        std::string column;
        bool quoted = false;
        std::size_t depth = 0;
        while (i < sql.size()) {
            const char c = sql[i];
            if (quoted) {
                if (c == '\'') {
                    if (i + 1 < sql.size() && sql[i + 1] == '\'') {
                        column += '\'';
                        i += 2;
                        continue;
                    }
                    quoted = false;
                    ++i;
                    continue;
                }
                column += c;
                ++i;
                continue;
            }
            if (c == '\'') {
                quoted = true;
                ++i;
                continue;
            }
            if (c == '-' && i + 1 < sql.size() && sql[i + 1] == '-') {
                i = skip_comment(i);
                continue;
            }
            if (c == '(') {
                ++depth;
                column += c;
                ++i;
                continue;
            }
            if (c == ')') {
                --depth;
                if (depth == 0) {
                    ++i;
                    break;
                }
                column += c;
                ++i;
                continue;
            }
            if (c == ',' && depth == 1) {
                columns.push_back(trim(column));
                column.clear();
                ++i;
                continue;
            }
            column += c;
            ++i;
        }
        columns.push_back(trim(column));

        // The opening parenthesis of the row was scanned as part of the first
        // column; it belongs to the row, not to the value.
        if (!columns.empty() && !columns.front().empty() && columns.front().front() == '(')
            columns.front().erase(0, 1);

        rows.push_back(std::move(columns));
    }

    if (rows.empty())
        throw std::invalid_argument("no value rows found in " + path);
    return rows;
}

}

#endif
