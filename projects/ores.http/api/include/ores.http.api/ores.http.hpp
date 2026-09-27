/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_HTTP_HPP
#define ORES_HTTP_HPP

/**
 * @brief HTTP gateway module for ORE Studio.
 *
 * The gateway terminates HTTP, authenticates the caller, and forwards each
 * request to the owning service over NATS, so it depends on the services'
 * wire protocols and not on their libraries. It is a composite of six parts.
 *
 * The module is organized into namespaces: domain (the request, response and
 * route types), net (the Boost.Beast server, the session and the router),
 * messaging (the service-discovery protocol), openapi (the endpoint
 * registry), server (the process entry point, its configuration and its
 * handlers), and routes (the generated route units for the assets, iam and
 * refdata domains).
 */
namespace ores::http {}

#endif
