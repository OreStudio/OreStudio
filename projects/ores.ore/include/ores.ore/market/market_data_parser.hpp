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
#ifndef ORES_ORE_MARKET_MARKET_DATA_PARSER_HPP
#define ORES_ORE_MARKET_MARKET_DATA_PARSER_HPP

#include <iosfwd>
#include <vector>
#include "ores.ore/market/fixing.hpp"
#include "ores.ore/market/market_datum.hpp"

namespace ores::ore::market {

/**
 * @brief Parses an ORE market data quote file (market*.txt).
 *
 * Skips blank lines and comment lines (starting with #).
 * Accepts both YYYYMMDD and YYYY-MM-DD date formats, auto-detected per line.
 *
 * @param in Input stream positioned at the start of the file.
 * @return All parsed market data entries in file order.
 * @throws std::invalid_argument if a non-blank, non-comment line has fewer
 *         than three whitespace-delimited tokens or an unrecognised date
 *         format.  The message includes the 1-based line number and the
 *         offending content.
 */
std::vector<market_datum> parse_market_data(std::istream& in);

/**
 * @brief Parses an ORE fixings file (fixings*.txt).
 *
 * Skips blank lines and comment lines (starting with #).
 * Accepts both YYYYMMDD and YYYY-MM-DD date formats, auto-detected per line.
 *
 * @param in Input stream positioned at the start of the file.
 * @return All parsed fixings in file order.
 * @throws std::invalid_argument if a non-blank, non-comment line has fewer
 *         than three whitespace-delimited tokens or an unrecognised date
 *         format.  The message includes the 1-based line number and the
 *         offending content.
 */
std::vector<fixing> parse_fixings(std::istream& in);

} // namespace ores::ore::market

#endif
