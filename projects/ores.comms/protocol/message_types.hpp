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
#ifndef ORES_COMMS_PROTOCOL_MESSAGE_TYPES_HPP
#define ORES_COMMS_PROTOCOL_MESSAGE_TYPES_HPP

#include <cstdint>
#include <algorithm>

namespace ores::comms::protocol {

constexpr std::uint32_t PROTOCOL_MAGIC = 0x4F524553; // "ORES" in ASCII

constexpr std::uint16_t PROTOCOL_VERSION_MAJOR = 1;
constexpr std::uint16_t PROTOCOL_VERSION_MINOR = 0;

enum class message_type {
    // Core protocol messages (0x0000 - 0x0FFF)
    handshake_request = 0x0001,
    handshake_response = 0x0002,
    handshake_ack = 0x0003,
    error_response = 0x0004,

    // Risk subsystem messages (0x1000 - 0x1FFF)
    get_currencies_request = 0x1001,
    get_currencies_response = 0x1002,

    // Accounts subsystem messages (0x2000 - 0x2FFF)
    create_account_request = 0x2001,
    create_account_response = 0x2002,
    list_accounts_request = 0x2003,
    list_accounts_response = 0x2004,
    login_request = 0x2005,
    login_response = 0x2006,
    unlock_account_request = 0x2007,
    unlock_account_response = 0x2008,

    last_value
};

enum class error_code {
    none = 0x0000,
    version_mismatch = 0x0001,
    crc_validation_failed = 0x0002,
    invalid_message_type = 0x0003,
    handshake_timeout = 0x0004,
    handshake_failed = 0x0005,
    payload_too_large = 0x0006,
    network_error = 0x0007,
    last_value
};

}

#endif
