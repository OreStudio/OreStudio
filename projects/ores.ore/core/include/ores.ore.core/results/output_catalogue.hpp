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
#ifndef ORES_ORE_CORE_RESULTS_OUTPUT_CATALOGUE_HPP
#define ORES_ORE_CORE_RESULTS_OUTPUT_CATALOGUE_HPP

#include "ores.ore.core/export.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::results {

/**
 * @brief The kind of value a column states.
 *
 * The kinds are the ones a reader can rely on without reading the file: a
 * decimal is a figure, a date names the day it belongs to, and a text column is
 * carried as the engine wrote it.
 */
enum class output_column_kind { text, integer, decimal, date, time };

/**
 * @brief One column an output family states.
 */
struct output_column {
    /**
     * @brief The heading the engine writes.
     */
    std::string name;
    /**
     * @brief The kind of value the samples show in the column.
     */
    output_column_kind kind = output_column_kind::text;
};

/**
 * @brief The job a column does in a family's row.
 *
 * A key identifies the row, a qualifier states the context of a figure without
 * being one, and a measure is a figure. Every column states exactly one, which
 * is what lets one reader serve families whose shapes differ.
 */
enum class output_column_role { key, qualifier, measure, none };

/**
 * @brief One family of output ORE writes.
 *
 * A family is the set of files that state one shape: =npv.csv=, or the
 * =exposure_trade_*= files one per trade. The columns are a union over the
 * header variants the samples show, because a family's shape follows the
 * configuration that asked for it; =header_variants= says how many variants
 * there are, so a reader knows whether the union is one shape or several.
 */
struct output_family {
    /**
     * @brief How the family is named, and how a reader asks for it.
     */
    std::string code;
    /**
     * @brief The file names it covers: a name, or a pattern with one '*'.
     *
     * The samples name a family's files after the trade or the curve they
     * belong to, so a family states the part of the name it fixes, as
     * =exposure_trade_*.csv=.
     */
    std::string file_pattern;
    /**
     * @brief The ORE parameter that names the file, empty when the engine names it.
     */
    std::string file_parameter;
    /**
     * @brief The analytic codes that write the family.
     */
    std::vector<std::string> analytic_codes;
    /**
     * @brief The columns that identify a row rather than measure it.
     */
    std::vector<std::string> key_columns;
    /**
     * @brief The columns every header variant of the family states.
     */
    std::vector<std::string> required_columns;
    /**
     * @brief The columns that state the context of a figure without being one.
     */
    std::vector<std::string> qualifier_columns;
    /**
     * @brief The columns that are figures, one metric for each.
     */
    std::vector<std::string> measure_columns;
    /**
     * @brief Whether the family gains measure columns from the configuration.
     *
     * A configuration-driven family adds a column for every curve, factor or
     * quantile the run asked for, so its measure set is a sample of a larger
     * space and a reader must accept one it has not seen.
     */
    bool measures_open = false;
    /**
     * @brief How many header variants the samples show for this family.
     */
    int header_variants = 0;
    /**
     * @brief A checked-in sample file that exercises the family.
     */
    std::string sample_file;
    /**
     * @brief Every column the family states, in the order the samples state them.
     */
    std::vector<output_column> columns;
};

/**
 * @brief Every output family ORE writes, as the checked-in samples state them.
 *
 * The table is generated from the samples by
 * =projects/ores.ore/scripts/generate_output_catalogue.py=, so it cannot
 * disagree with them. It is parsed once, on first use.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT const std::vector<output_family>& output_families();

/**
 * @brief The family with a code, or null when no family states it.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT const output_family* family_named(std::string_view code);

/**
 * @brief The family a file name belongs to, or null when no family covers it.
 *
 * @param file_name the file's name, without a directory.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT const output_family* family_of_file(std::string_view file_name);

/**
 * @brief The column a family states under a heading, or null when it states none.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT const output_column* column_named(const output_family& family,
                                                                     std::string_view name);

/**
 * @brief The job a column does in a family, or none when the family does not
 * state the column.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT output_column_role role_of(const output_family& family,
                                                              std::string_view name);

/**
 * @brief The name a column kind is written by in the catalogue.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT std::string_view to_string(output_column_kind kind);

}

#endif
