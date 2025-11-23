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
#include "ores.risk/domain/currency_version_table.hpp"

#include <fort.hpp>

namespace ores::risk::domain {

std::string convert_to_table(const std::vector<currency_version>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header << "Version" << "ISO Code" << "Name" << "Modified By"
          << "Modified At" << "Change Summary" << fort::endr;

    for (const auto& cv : v) {
        table << cv.version_number << cv.data.iso_code << cv.data.name
              << cv.modified_by << cv.modified_at << cv.change_summary
              << fort::endr;
    }
    return table.to_string();
}

}
