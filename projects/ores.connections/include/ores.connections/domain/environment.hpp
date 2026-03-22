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
#ifndef ORES_CONNECTIONS_DOMAIN_ENVIRONMENT_HPP
#define ORES_CONNECTIONS_DOMAIN_ENVIRONMENT_HPP

#include <string>
#include <optional>
#include <boost/uuid/uuid.hpp>

namespace ores::connections::domain {

/**
 * @brief Represents a pure server environment (host + port only, no credentials).
 *
 * An environment captures where to connect — host and port — without any
 * credential information. Connections may be linked to an environment so that
 * host/port are resolved live rather than stored redundantly.
 *
 * Environments are organized into folders and can be tagged for flexible
 * categorization.
 */
struct environment final {
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
     * @brief Port number the NATS server is listening on.
     */
    int port{0};

    /**
     * @brief Port number the HTTP server is listening on.
     *
     * Used by the Qt client to construct the HTTP base URL for package
     * upload and download. Defaults to 8080.
     */
    int http_port{8080};

    /**
     * @brief Optional description or notes about this environment.
     */
    std::string description;

    /**
     * @brief NATS subject namespace for this environment.
     *
     * When non-empty, prepended to every outbound NATS subject to isolate
     * this environment from others sharing the same broker.
     * Format: "ores.{tier}.{instance}", e.g. "ores.dev.local1".
     */
    std::string subject_prefix;
};

}

#endif
