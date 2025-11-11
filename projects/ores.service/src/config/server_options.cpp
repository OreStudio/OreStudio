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
#include <ostream>
#include "ores.service/config/server_options.hpp"

namespace ores::service::config {

std::ostream& operator<<(std::ostream& s, const server_options& v) {
    s << " { "
      << "\"__type__\": " << "\"ores::service::config::server_options\"" << ", "
      << "\"port\": " << v.port << ", "
      << "\"max_connections\": " << v.max_connections << ", "
      << "\"certificate_file\": " << "\"" << v.certificate_file << "\"" << ", "
      << "\"private_key_file\": " << "\"" << v.private_key_file << "\"" << ", "
      << "\"server_identifier\": " << "\"" << v.server_identifier << "\""
      << " }";
    return s;
}

}
