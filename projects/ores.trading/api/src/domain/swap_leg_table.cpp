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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_table.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.trading.api/domain/swap_leg_table.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <fort.hpp>

namespace ores::trading::domain {


std::string convert_to_table(const std::vector<swap_leg>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header << "ID" << "Instrument" << "Leg" << "Type" << "Ccy" << "Notional"
          << "Fixed Rate" << "Index" << "Recorded At" << fort::endr;

    for ([[maybe_unused]] const auto& sl : v) {
        table << sl.identity.id << sl.identity.instrument_id << sl.identity.leg_number
              << sl.leg_type_code << sl.currency << sl.notional << sl.fixed_rate
              << sl.floating_index_code << sl.audit.recorded_at << fort::endr;
    }
    return table.to_string();
}

}
