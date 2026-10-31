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
#ifndef ORES_ORE_CORE_RESULTS_OUTPUT_CSV_HPP
#define ORES_ORE_CORE_RESULTS_OUTPUT_CSV_HPP

#include <string>
#include <string_view>
#include <vector>

/**
 * @file output_csv.hpp
 * @brief What every reader of an engine output file shares.
 *
 * The engine writes comma-separated text with a leading hash on the heading and
 * a null string for a figure it has no value for. The fields are declared once,
 * in a named namespace, so a reader of metrics and a reader of cashflows read a
 * file the same way and neither holds a private copy that can drift.
 */
namespace ores::ore::results::output_csv {

/**
 * @brief Whether a field states the absence of a value.
 *
 * The engine writes a figure it has no value for as its null string, and a key
 * it has no value for as an empty field. Both mean the same thing to a reader:
 * nothing was stated.
 */
[[nodiscard]] bool is_null(std::string_view text);

/**
 * @brief The fields of one line, with the quotes removed.
 *
 * A field may be quoted and may hold commas inside the quotes, because a label
 * such as a curve name is not a number and the engine quotes what it must.
 */
[[nodiscard]] std::vector<std::string> fields_of(std::string_view line);

/**
 * @brief Reads a figure, exactly.
 *
 * A figure the engine states in full precision is read in full precision, and a
 * trailing or leading space makes it unreadable rather than silently accepted.
 */
[[nodiscard]] bool figure_of(std::string_view text, double& figure);

/**
 * @brief The names as one sentence, for a refusal that names what it found.
 */
[[nodiscard]] std::string joined(const std::vector<std::string>& names);

}

#endif
