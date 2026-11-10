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
#include "ores.analytics.quant/domain/tenor_resolution.hpp"
#include <array>
#include <stdexcept>

namespace ores::analytics::quant::domain {

std::string_view to_string(resolution_status status) {
    switch (status) {
        case resolution_status::ok:
            return "ok";
        case resolution_status::tenor_not_in_convention:
            return "tenor_not_in_convention";
        case resolution_status::calendar_index_out_of_range:
            return "calendar_index_out_of_range";
        case resolution_status::unsupported_anchor:
            return "unsupported_anchor";
        case resolution_status::missing_spot_date:
            return "missing_spot_date";
        case resolution_status::period_tenor_without_period:
            return "period_tenor_without_period";
        case resolution_status::special_tenor_without_offset:
            return "special_tenor_without_offset";
        case resolution_status::inconsistent_schedule_declaration:
            return "inconsistent_schedule_declaration";
        case resolution_status::schedule_without_dates:
            return "schedule_without_dates";
        case resolution_status::schedule_dates_not_ordered:
            return "schedule_dates_not_ordered";
        case resolution_status::schedule_exhausted:
            return "schedule_exhausted";
    }
    return "unknown";
}

} // namespace ores::analytics::quant::domain
