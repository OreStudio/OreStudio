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
#include "ores.connections/domain/folder_table_io.hpp"

#include <ostream>
#include <fort.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::connections::domain {

std::ostream& operator<<(std::ostream& s, const std::vector<folder>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header << "ID" << "Name" << "Parent ID" << "Description" << fort::endr;

    for (const auto& f : v) {
        const auto parent_str = f.parent_id
            ? boost::uuids::to_string(*f.parent_id)
            : "(root)";

        table << boost::uuids::to_string(f.id)
              << f.name
              << parent_str
              << f.description
              << fort::endr;
    }

    s << table.to_string();
    return s;
}

}
