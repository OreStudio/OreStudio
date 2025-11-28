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
 * FOR A PARTICULAR PURPOSE. Seethe GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General PublicLicense along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_SERVICE_SERVICE_HPP
#define ORES_SERVICE_SERVICE_HPP

/**
 * @brief Main server application for ORE Studio.
 *
 * Multi-client server application hosting the ORE Studio backend services.
 * Key features:
 *
 * - Server hosting: Runs the ORE Studio comms server with SSL/TLS
 * - Subsystem integration: Registers message handlers for accounts, risk, and
 *   other subsystems
 * - Database access: Provides database context to all subsystems
 * - Configuration: Boost program_options parser for server, database, and
 *   logging configuration
 * - Concurrent connections: Supports multiple simultaneous client connections
 * - Asynchronous I/O: Built on Boost.Asio coroutines for scalable async operations
 *
 * The component is organized into namespaces: config (option parsing and
 * configuration), and app (application hosting and execution).
 */
namespace ores::service {}

#endif
