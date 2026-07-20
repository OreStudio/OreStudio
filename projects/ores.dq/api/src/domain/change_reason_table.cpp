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
#include "ores.dq.api/domain/change_reason_table.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <fort.hpp>

namespace ores::dq::domain {


std::string convert_to_table(const std::vector<change_reason>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header << "Code" << "Description" << "Category" << "Applies To New"
          << "Applies To Amend" << "Applies To Delete" << "Requires Commentary" << "Display Order"
          << "Modified By" << fort::endr;

    for (const auto& r : v) {
        table << r.code << r.description << r.category_code << (r.applies_to_new ? "true" : "false")
              << (r.applies_to_amend ? "true" : "false") << (r.applies_to_delete ? "true" : "false")
              << (r.requires_commentary ? "true" : "false") << r.display_order << r.modified_by
              << fort::endr;
    }
    return table.to_string();
}

}
