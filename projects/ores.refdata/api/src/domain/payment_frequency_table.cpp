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
#include "ores.refdata.api/domain/payment_frequency_table.hpp"
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

std::string convert_to_table(const std::vector<payment_frequency>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header << "Code" << "Name" << "Description" << "Period Unit"
          << "Period Multiplier" << "Display Order" << "Modified By" << "Version" << fort::endr;

    for (const auto& pf : v) {
        table << pf.code << pf.name << pf.description << pf.period_unit
              << opt_str(pf.period_multiplier) << pf.display_order << pf.modified_by << pf.version
              << fort::endr;
    }
    return table.to_string();
}

}
