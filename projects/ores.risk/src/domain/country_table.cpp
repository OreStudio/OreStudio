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
#include "ores.risk/domain/country_table.hpp"

#include <sstream>
#include <fort.hpp>

namespace ores::risk::domain {

std::string convert_to_table(const country& c) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header << "Alpha-2" << "Alpha-3" << "Numeric" << "Version"
          << "Name" << "Official Name" << "Recorded By"
          << "Recorded At" << fort::endr;

    table << c.alpha2_code << c.alpha3_code << c.numeric_code << c.version
          << c.name << c.official_name << c.recorded_by
          << c.recorded_at << fort::endr;

    std::ostringstream ss;
    ss << std::endl << table.to_string() << std::endl;
    return ss.str();
}

std::string convert_to_table(const std::vector<country>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header << "Alpha-2" << "Alpha-3" << "Numeric" << "Version"
          << "Name" << "Official Name" << "Recorded By"
          << "Recorded At" << fort::endr;

    for (const auto& c : v) {
        table << c.alpha2_code << c.alpha3_code << c.numeric_code << c.version
              << c.name << c.official_name << c.recorded_by
              << c.recorded_at << fort::endr;
    }

    std::ostringstream ss;
    ss << std::endl << table.to_string() << std::endl;
    return ss.str();
}

}
