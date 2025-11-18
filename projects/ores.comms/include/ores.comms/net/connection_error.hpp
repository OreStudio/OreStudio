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
#ifndef ORES_COMMS_NET_CONNECTION_ERROR_HPP
#define ORES_COMMS_NET_CONNECTION_ERROR_HPP

#include <stdexcept>
#include <string>

namespace ores::comms {

/**
 * @brief Exception thrown when client connection or handshake fails.
 *
 * This exception provides detailed error messages for connection failures,
 * protocol mismatches, and handshake errors.
 */
class connection_error : public std::runtime_error {
public:
    explicit connection_error(const std::string& message)
        : std::runtime_error(message) {}

    explicit connection_error(const char* message)
        : std::runtime_error(message) {}
};

}

#endif
