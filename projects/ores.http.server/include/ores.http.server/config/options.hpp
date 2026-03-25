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
#ifndef ORES_HTTP_SERVER_CONFIG_OPTIONS_HPP
#define ORES_HTTP_SERVER_CONFIG_OPTIONS_HPP

#include <iosfwd>
#include <string>
#include <optional>
#include "ores.logging/logging_options.hpp"
#include "ores.database/domain/database_options.hpp"
#include "ores.http.api/net/http_server_options.hpp"
#include "ores.nats/config/nats_options.hpp"

namespace ores::http_server::config {

/**
 * @brief All of the configuration options required by the HTTP server.
 */
struct options final {
    /**
     * @brief Configuration options related to logging, if any.
     */
    std::optional<ores::logging::logging_options> logging;

    /**
     * @brief Configuration related to HTTP server operations.
     */
    http::net::http_server_options server;

    /**
     * @brief Configuration related to database operations.
     */
    ores::database::database_options database;

    /**
     * @brief Configuration related to NATS messaging.
     */
    ores::nats::config::nats_options nats;

    /**
     * @brief Root directory for compute grid file storage (packages, inputs, outputs).
     */
    std::string compute_storage_dir{"/var/ores/http-server/compute"};

    /**
     * @brief Override for the HTTP base URL advertised via NATS service discovery.
     *
     * If empty, defaults to "http://localhost:{server.port}".
     */
    std::string http_base_url;
};

std::ostream& operator<<(std::ostream& s, const options& v);

}

#endif
