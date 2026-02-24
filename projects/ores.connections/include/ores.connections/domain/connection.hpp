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
#ifndef ORES_CONNECTIONS_DOMAIN_CONNECTION_HPP
#define ORES_CONNECTIONS_DOMAIN_CONNECTION_HPP

#include <string>
#include <optional>
#include <boost/uuid/uuid.hpp>

namespace ores::connections::domain {

/**
 * @brief Represents a saved connection with credentials.
 *
 * A connection stores the credentials needed to authenticate to a server.
 * When linked to an environment (via environment_id), the host and port are
 * resolved live from the environment. When standalone, host and port are
 * stored directly.
 *
 * The encrypted_password field stores the AES-encrypted password. Use the
 * connection_manager service to encrypt/decrypt passwords.
 */
struct connection final {
    /**
     * @brief Unique identifier for this connection.
     */
    boost::uuids::uuid id;

    /**
     * @brief Folder this connection belongs to.
     *
     * If empty, the connection is at the root level.
     */
    std::optional<boost::uuids::uuid> folder_id;

    /**
     * @brief Environment this connection is linked to.
     *
     * When set, host and port are resolved from the environment rather
     * than from the host/port fields on this connection.
     */
    std::optional<boost::uuids::uuid> environment_id;

    /**
     * @brief Display name for this connection.
     */
    std::string name;

    /**
     * @brief Host name or IP address of the server.
     *
     * Only used when environment_id is not set.
     */
    std::optional<std::string> host;

    /**
     * @brief Port number the server is listening on.
     *
     * Only used when environment_id is not set.
     */
    std::optional<int> port;

    /**
     * @brief Username for authentication.
     */
    std::string username;

    /**
     * @brief Encrypted password for authentication.
     *
     * This field stores the AES-encrypted password. Use the connection_manager
     * service to encrypt passwords before storing and decrypt when connecting.
     */
    std::string encrypted_password;

    /**
     * @brief Optional description or notes about this connection.
     */
    std::string description;
};

}

#endif
