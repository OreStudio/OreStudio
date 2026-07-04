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
#ifndef ORES_ORE_CORE_MARKET_MARKET_DATA_PARSER_HPP
#define ORES_ORE_CORE_MARKET_MARKET_DATA_PARSER_HPP

#include "ores.ore.core/export.hpp"
#include "ores.ore.core/market/fixing.hpp"
#include "ores.ore.core/market/market_datum.hpp"
#include <iosfwd>
#include <string>
#include <vector>

namespace ores::ore::market {

/**
 * @brief How the parser treats a repeated (date, key) pair within one file.
 *
 * ORE market data files can — and, in real ORE example data, do — repeat
 * the same key on the same date with a different value later in the file.
 * Either way the later occurrence wins (matches how a sequential loader
 * that stores into a map would naturally behave); the policy only controls
 * which report bucket the repeat is recorded in.
 */
enum class duplicate_policy {
    /** Later occurrence wins; each repeat is recorded as a warning. */
    warn,
    /** Later occurrence wins; each repeat is recorded as an error. */
    error,
};

/**
 * @brief One parser-reported issue (a duplicate key, currently the only kind).
 */
struct parse_issue {
    /** 1-based line number of the losing (earlier) occurrence. */
    int line_no;
    /** Human-readable description, e.g. the key and the winning line number. */
    std::string message;
};

/**
 * @brief Non-fatal issues collected while parsing, bucketed by severity.
 *
 * Distinct from the std::invalid_argument thrown for malformed lines, which
 * always aborts the parse — this report is for well-formed lines that are
 * nonetheless suspicious (currently: duplicate keys), which the caller may
 * want to surface (e.g. in a shell command's response) without failing the
 * whole import.
 */
struct parse_report {
    std::vector<parse_issue> warnings;
    std::vector<parse_issue> errors;
};

/**
 * @brief Parses an ORE market data quote file (market*.txt).
 *
 * Skips blank lines and comment lines (starting with #).
 * Accepts both YYYYMMDD and YYYY-MM-DD date formats, auto-detected per line.
 * A (date, key) pair repeated later in the file overwrites the earlier one
 * in the returned vector, at the position of its last occurrence; see
 * duplicate_policy for how the repeat is reported.
 *
 * @param in Input stream positioned at the start of the file.
 * @param on_duplicate How to bucket a repeated (date, key) pair in *report.
 * @param report If non-null, appended with one issue per duplicate found.
 * @return Parsed, de-duplicated market data entries in file order.
 * @throws std::invalid_argument if a non-blank, non-comment line has fewer
 *         than three whitespace-delimited tokens or an unrecognised date
 *         format.  The message includes the 1-based line number and the
 *         offending content.
 */
ORES_ORE_CORE_EXPORT std::vector<market_datum>
parse_market_data(std::istream& in,
                  duplicate_policy on_duplicate = duplicate_policy::warn,
                  parse_report* report = nullptr);

/**
 * @brief Parses an ORE fixings file (fixings*.txt).
 *
 * Skips blank lines and comment lines (starting with #).
 * Accepts both YYYYMMDD and YYYY-MM-DD date formats, auto-detected per line.
 * A (date, index_name) pair repeated later in the file overwrites the
 * earlier one in the returned vector, at the position of its last
 * occurrence; see duplicate_policy for how the repeat is reported.
 *
 * @param in Input stream positioned at the start of the file.
 * @param on_duplicate How to bucket a repeated (date, index_name) pair in *report.
 * @param report If non-null, appended with one issue per duplicate found.
 * @return Parsed, de-duplicated fixings in file order.
 * @throws std::invalid_argument if a non-blank, non-comment line has fewer
 *         than three whitespace-delimited tokens or an unrecognised date
 *         format.  The message includes the 1-based line number and the
 *         offending content.
 */
ORES_ORE_CORE_EXPORT std::vector<fixing>
parse_fixings(std::istream& in,
             duplicate_policy on_duplicate = duplicate_policy::warn,
             parse_report* report = nullptr);

} // namespace ores::ore::market

#endif
