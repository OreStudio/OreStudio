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
#ifndef ORES_TESTING_SERIES_CLASSIFICATION_RULE_SEED_HPP
#define ORES_TESTING_SERIES_CLASSIFICATION_RULE_SEED_HPP

#include "ores.marketdata.api/domain/series_classification_rule.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/sql_values_rows.hpp"
#include <cstddef>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::testing {

/**
 * @brief The series classification rule rows as the populate script writes
 *        them, read from the script itself rather than restated in C++.
 *
 * A test that restates the seed would pass after the seed changed, which is
 * the drift this reads the file to catch. The script is the same one the
 * database loader runs, so a row that parses here is a row that lands in the
 * table.
 *
 * Parsed once per process and shared; the file does not change while a test
 * binary runs.
 */
inline const std::vector<ores::marketdata::domain::series_classification_rule>&
seed_classification_rules() {
    static const auto rules = [] {
        const auto path = ores::testing::project_root::resolve(
            "projects/ores.sql/populate/marketdata/"
            "marketdata_series_classification_rules_populate.sql");
        const auto sql = ores::platform::filesystem::file::read_content(path);

        // Column positions of one `values` row, as the script writes them.
        constexpr std::size_t series_type_column = 1;
        constexpr std::size_t metric_column = 2;
        constexpr std::size_t asset_class_source_column = 4;
        constexpr std::size_t asset_class_code_column = 5;
        constexpr std::size_t series_subclass_code_column = 6;
        constexpr std::size_t description_column = 7;
        constexpr std::size_t min_columns = 8;

        std::vector<ores::marketdata::domain::series_classification_rule> result;
        for (const auto& columns : sql_values_rows(sql, path.string())) {
            if (columns.size() < min_columns)
                throw std::invalid_argument("row with " + std::to_string(columns.size()) +
                                            " columns in " + path.string() +
                                            ", expected at least " + std::to_string(min_columns));

            ores::marketdata::domain::series_classification_rule rule;
            rule.series_type = columns[series_type_column];
            rule.metric = columns[metric_column];
            rule.asset_class_source = columns[asset_class_source_column];
            // Null is the honest "no class here" for a correlation rule, and
            // the script writes the keyword rather than an empty string.
            if (columns[asset_class_code_column] != "null")
                rule.asset_class_code = columns[asset_class_code_column];
            rule.series_subclass_code = columns[series_subclass_code_column];
            rule.description = columns[description_column];
            result.push_back(std::move(rule));
        }
        return result;
    }();
    return rules;
}

}

#endif
