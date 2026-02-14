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
#include "ores.refdata/domain/business_centre_table.hpp"

#include <fort.hpp>

namespace ores::refdata::domain {

namespace {

fort::char_table make_business_centre_table() {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);
    table << fort::header << "Code" << "Version" << "Source" << "Description"
          << "Coding Scheme" << "Country" << "Change Reason" << "Modified By"
          << "Recorded At" << fort::endr;
    return table;
}

void add_business_centre_row(fort::char_table& table, const business_centre& c) {
    table << c.code << c.version << c.source << c.description
          << c.coding_scheme_code << c.country_alpha2_code
          << c.change_reason_code << c.modified_by
          << c.recorded_at << fort::endr;
}

std::string format_table(fort::char_table& table) {
    return table.to_string();
}

}

std::string convert_to_table(const business_centre& c) {
    auto table = make_business_centre_table();
    add_business_centre_row(table, c);
    return format_table(table);
}

std::string convert_to_table(const std::vector<business_centre>& v) {
    auto table = make_business_centre_table();
    for (const auto& c : v) {
        add_business_centre_row(table, c);
    }
    return format_table(table);
}

}
