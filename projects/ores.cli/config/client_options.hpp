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
#ifndef ORES_CLI_CONFIG_CLIENT_OPTIONS_HPP
#define ORES_CLI_CONFIG_CLIENT_OPTIONS_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <iosfwd>
#include <string>

namespace ores::cli::config {

/**
 * @brief Options for the client command.
 */
struct client_options final {
    /**
     * @brief Host to connect to.
     */
    std::string host = "localhost";

    /**
     * @brief Port to connect to.
     */
    uint16_t port = 55555;

    /**
     * @brief Client identifier to send in handshake.
     */
    std::string client_identifier = "ores-cli-client";

    /**
     * @brief Whether to verify server certificate.
     */
    bool verify_certificate = false;
};

std::ostream& operator<<(std::ostream& s, const client_options& v);

}

#endif
