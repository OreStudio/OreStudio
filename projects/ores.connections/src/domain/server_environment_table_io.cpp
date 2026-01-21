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
#include "ores.connections/domain/server_environment_table_io.hpp"

#include <ostream>
#include <fort.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::connections::domain {

std::ostream& operator<<(std::ostream& s, const std::vector<server_environment>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header
          << "ID" << "Name" << "Host" << "Port" << "Username" << "Password" << "Description"
          << fort::endr;

    for (const auto& env : v) {
        const auto has_password = !env.encrypted_password.empty() ? "Y" : "N";

        table << boost::uuids::to_string(env.id)
              << env.name
              << env.host
              << env.port
              << env.username
              << has_password
              << env.description
              << fort::endr;
    }

    s << table.to_string();
    return s;
}

}
