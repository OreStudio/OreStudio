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
#include "ores.ore/market/series_key_registry.hpp"

namespace ores::ore::market {

void serialize_market_data(std::ostream& out,
                           const std::vector<market_datum>& data) {
    for (const auto& d : data) {
        // Reconstruct the key from decomposed components to validate that
        // decomposition is lossless.  Falls back to the verbatim key for
        // entries where decomposition was not attempted (series_type empty).
        const std::string emitted_key = d.series_type.empty()
            ? d.key
            : reconstruct_key({d.series_type, d.metric, d.qualifier, d.point_id});
        out << ores::platform::time::time_utils::format_date_compact(d.date)
            << '\t' << emitted_key << '\t' << d.value << '\n';
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
