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
#ifndef ORES_TESTING_SERIES_KEY_SHAPE_SEED_HPP
#define ORES_TESTING_SERIES_KEY_SHAPE_SEED_HPP

#include "ores.ore.api/domain/series_key_shape.hpp"
#include "ores.ore.core/market/series_key_registry.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include <cctype>
#include <cstddef>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::testing {

/**
 * @brief The series key shape rows as the populate script writes them, read
 *        from the script itself rather than restated in C++.
 *
 * A test that restates the seed would pass after the seed changed, which is
 * the drift this reads the file to catch. The script is the same one the
 * database loader runs, so a row that parses here is a row that lands in the
 * table.
 *
 * Parsed once per process and shared; the file does not change while a test
 * binary runs.
 */
inline const std::vector<ores::ore::domain::series_key_shape>& seed_shapes() {
    static const auto shapes = [] {
        const auto path = ores::testing::project_root::resolve(
            "projects/ores.sql/populate/ore/ore_series_key_shapes_populate.sql");
        const auto sql = ores::platform::filesystem::file::read_content(path);

        // Column positions of one `values` row, as the script writes them.
        constexpr std::size_t series_type_column = 1;
        constexpr std::size_t qualifier_depth_column = 3;
        constexpr std::size_t has_point_dimension_column = 4;
        constexpr std::size_t default_point_column = 5;
        constexpr std::size_t min_columns = 6;

        constexpr std::string_view values_keyword = "\nvalues";
        const auto values = sql.find(values_keyword);
        if (values == std::string::npos)
            throw std::invalid_argument("no `values` list in " + path.string());

        // A `--` inside a quoted description is text, not a comment, so this
        // is only ever called with the scanner outside a string.
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

        std::vector<ores::ore::domain::series_key_shape> result;
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

            if (columns.size() < min_columns)
                throw std::invalid_argument("row with " + std::to_string(columns.size()) +
                                            " columns in " + path.string() +
                                            ", expected at least " + std::to_string(min_columns));

            ores::ore::domain::series_key_shape shape;
            shape.series_type = columns[series_type_column];
            shape.qualifier_depth = std::stoi(columns[qualifier_depth_column]);
            shape.has_point_dimension = columns[has_point_dimension_column] == "true";
            shape.default_point = columns[default_point_column];
            result.push_back(std::move(shape));
        }

        if (result.empty())
            throw std::invalid_argument("no shape rows found in " + path.string());
        return result;
    }();
    return shapes;
}

/**
 * @brief The key grammar a test gets when it has no database to read one from.
 */
inline const ores::ore::market::series_key_registry& seed_registry() {
    static const ores::ore::market::series_key_registry registry{seed_shapes()};
    return registry;
}

}

#endif
