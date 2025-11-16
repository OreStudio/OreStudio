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
#ifndef ORES_COMMS_ORES_COMMS_HPP
#define ORES_COMMS_ORES_COMMS_HPP

/**
 * @brief Communications library providing client-server connectivity for ORE Studio.
 *
 * Implements a custom binary protocol over SSL/TLS for secure client-server
 * communication. Key features:
 *
 * - SSL/TLS security: All connections secured with OpenSSL
 * - Binary protocol: Frame-based message exchange with headers and payloads
 * - Server: Asynchronous multi-client server with session management
 * - Client: Thread-safe client with both sync and async APIs
 * - Handshake: Connection establishment with server/client identification
 * - Message dispatching: Pluggable message handlers by subsystem range
 * - Connection pooling: Maximum connection limits and active connection tracking
 * - Error handling: Comprehensive error codes and std::expected-based APIs
 *
 * The protocol namespace defines message frames, types, handlers, and the
 * message dispatcher that routes messages to registered subsystem handlers.
 */
namespace ores::comms {}

#endif
