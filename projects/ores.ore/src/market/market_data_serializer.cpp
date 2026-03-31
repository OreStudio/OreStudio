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
#include "ores.ore/market/market_data_serializer.hpp"

#include <ostream>
#include <vector>
#include "ores.platform/time/time_utils.hpp"

namespace ores::ore::market {

void serialize_market_data(std::ostream& out,
                           const std::vector<market_datum>& data) {
    for (const auto& d : data) {
        out << ores::platform::time::time_utils::format_date_compact(d.date)
            << '\t' << d.key << '\t' << d.value << '\n';
    }
}

void serialize_fixings(std::ostream& out,
                       const std::vector<fixing>& fixings) {
    for (const auto& f : fixings) {
        out << ores::platform::time::time_utils::format_date_iso(f.date)
            << '\t' << f.index_name << '\t' << f.value << '\n';
    }
}

} // namespace ores::ore::market
