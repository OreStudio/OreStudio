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
#include "ores.trading.api/domain/equity_option_instrument_table.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <fort.hpp>

namespace ores::trading::domain {


std::string convert_to_table(const std::vector<equity_option_instrument>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header << "ID" << "Type" << "Underlying" << "Ccy" << "Notional" << "Option Type"
          << "Strike" << "Expiry Date" << "Exercise Type" << "Recorded At" << fort::endr;

    for (const auto& eoi : v) {
        table << eoi.identity.instrument_id << eoi.identity.trade_type_code << eoi.underlying_name
              << eoi.currency << eoi.notional << eoi.option_type << eoi.strike << eoi.expiry_date
              << eoi.exercise_type << eoi.audit.recorded_at << fort::endr;
    }
    return table.to_string();
}

}
