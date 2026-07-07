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
#include "ores.refdata.api/domain/currency_pair_convention_table.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <fort.hpp>
#include <sstream>

namespace ores::refdata::domain {

namespace {
template <typename T>
std::string opt_str(const std::optional<T>& o) {
    if (!o)
        return {};
    std::ostringstream s;
    if constexpr (std::is_same_v<T, bool>)
        s << std::boolalpha;
    s << *o;
    return s.str();
}
}

std::string convert_to_table(const std::vector<currency_pair_convention>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header << "Pair" << "Pip Factor" << "Tick Size" << "Decimal Places"
          << "Advance Calendar" << "Business Day Convention" << "Spot Relative" << "End Of Month"
          << "Modified By" << "Version" << fort::endr;

    for (const auto& cpv : v) {
        table << cpv.pair_code << cpv.pip_factor << cpv.tick_size << cpv.decimal_places
              << opt_str(cpv.advance_calendar) << opt_str(cpv.business_day_convention)
              << opt_str(cpv.spot_relative) << opt_str(cpv.end_of_month) << cpv.modified_by
              << cpv.version << fort::endr;
    }
    return table.to_string();
}

}
