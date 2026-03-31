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
#ifndef ORES_ORE_MARKET_MARKET_DATA_SERIALIZER_HPP
#define ORES_ORE_MARKET_MARKET_DATA_SERIALIZER_HPP

#include <iosfwd>
#include <vector>
#include "ores.ore/market/fixing.hpp"
#include "ores.ore/market/market_datum.hpp"

namespace ores::ore::market {

/**
 * @brief Serializes market data quotes to ORE text format.
 *
 * Output format: one entry per line — DATE<TAB>KEY<TAB>VALUE
 * Dates are written in YYYYMMDD format.
 * Values are written exactly as stored (no reformatting).
 * Comment lines are not emitted.
 *
 * @param out Output stream.
 * @param data The market data entries to write.
 */
void serialize_market_data(std::ostream& out,
                           const std::vector<market_datum>& data);

/**
 * @brief Serializes fixings to ORE text format.
 *
 * Output format: one entry per line — DATE<TAB>INDEX_NAME<TAB>VALUE
 * Dates are written in YYYY-MM-DD format.
 * Values are written exactly as stored (no reformatting).
 * Comment lines are not emitted.
 *
 * @param out Output stream.
 * @param fixings The fixings to write.
 */
void serialize_fixings(std::ostream& out,
                       const std::vector<fixing>& fixings);

} // namespace ores::ore::market

#endif
