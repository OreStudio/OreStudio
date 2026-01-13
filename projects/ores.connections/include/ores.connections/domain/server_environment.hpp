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
#ifndef ORES_CONNECTIONS_DOMAIN_SERVER_ENVIRONMENT_HPP
#define ORES_CONNECTIONS_DOMAIN_SERVER_ENVIRONMENT_HPP

#include <string>
#include <optional>
#include <boost/uuid/uuid.hpp>

namespace ores::connections::domain {

/**
 * @brief Represents a server connection/environment configuration.
 *
 * A server environment stores all the details needed to connect to a remote
 * server instance. Environments are organized into folders and can be tagged
 * for flexible categorization.
 *
 * The password field stores the encrypted form of the password. Use the
 * connection_manager service to encrypt/decrypt passwords.
 */
struct server_environment final {
    /**
     * @brief Unique identifier for this environment.
     */
    boost::uuids::uuid id;

    /**
     * @brief Folder this environment belongs to.
     *
     * If empty, the environment is at the root level.
     */
    std::optional<boost::uuids::uuid> folder_id;

    /**
     * @brief Display name for this environment.
     */
    std::string name;

    /**
     * @brief Host name or IP address of the server.
     */
    std::string host;

    /**
     * @brief Port number the server is listening on.
     */
    int port = 0;

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
     * @brief Optional description or notes about this environment.
     */
    std::string description;
};

}

#endif
