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
#ifndef ORES_CONNECTIONS_DOMAIN_SERVER_ENVIRONMENT_JSON_IO_HPP
#define ORES_CONNECTIONS_DOMAIN_SERVER_ENVIRONMENT_JSON_IO_HPP

#include <iosfwd>
#include "ores.connections/domain/server_environment.hpp"

namespace ores::connections::domain {

/**
 * @brief Dumps the server_environment object to a stream in JSON format.
 *
 * Note: The encrypted_password field is included in the output. Callers should
 * be aware that while the password is encrypted, the ciphertext will be visible
 * in the JSON output.
 */
std::ostream& operator<<(std::ostream& s, const server_environment& v);

}

#endif
