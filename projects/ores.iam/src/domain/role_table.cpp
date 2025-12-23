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
#include "ores.iam/domain/role_table.hpp"

#include <sstream>
#include <fort.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/algorithm/string/join.hpp>

namespace ores::iam::domain {

std::string convert_to_table(const std::vector<role>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header << "ID (UUID)" << "Name" << "Description"
          << "Permissions" << "Recorded By" << "Version" << fort::endr;

    for (const auto& r : v) {
        std::string permissions_str;
        if (r.permission_codes.size() <= 3) {
            permissions_str = boost::algorithm::join(r.permission_codes, ", ");
        } else {
            std::vector<std::string> first_three(
                r.permission_codes.begin(),
                r.permission_codes.begin() + 3);
            std::ostringstream oss;
            oss << boost::algorithm::join(first_three, ", ")
                << " (+" << (r.permission_codes.size() - 3) << " more)";
            permissions_str = oss.str();
        }

        table << boost::uuids::to_string(r.id) << r.name
              << r.description << permissions_str
              << r.recorded_by << r.version << fort::endr;
    }
    return table.to_string();
}

}
