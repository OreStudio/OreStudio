/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.risk/csv/currency_mapper.hpp"

namespace ores::risk::csv {

using domain::currency;
using namespace ores::utility::log;

std::vector<currency>
currency_mapper::map(const std::vector<currency>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Started mapping. Total: " << v.size();
    
    // For CSV, we simply return the currencies as-is since they're already
    // in the right format for CSV output. This follows the same pattern
    // as the ORE XML mapper but with minimal transformation.
    BOOST_LOG_SEV(lg(), debug) << "Finished mapping.";
    return v;
}

}