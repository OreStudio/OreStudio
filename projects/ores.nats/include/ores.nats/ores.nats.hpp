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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_NATS_HPP
#define ORES_NATS_HPP

/**
 * @brief NATS transport layer: the external message bus and the
 * cross-process connectivity every service uses.
 *
 * Provides RAII wrappers around the cnats C library, so no component
 * depends on cnats directly. It carries the three messaging patterns the
 * tree uses: synchronous request/reply, queue-group subscribe for
 * load-balanced handlers, and durable JetStream streams. Every message
 * body is serialised by the wire codec in the format the process resolved
 * at startup.
 *
 * Subject names follow @c {domain}.v1.{entity}.{operation}, with an
 * optional prefix for multi-instance deployments. The component that owns
 * a subject declares its constant in a model, and this library applies the
 * prefix.
 *
 * Sub-namespaces:
 * - @b config: the connection options, read from the command line and the
 *   shared @c ORES_NATS_ environment domain.
 * - @b domain: the message, header and correlation types, the wire codec
 *   and the compression helpers. No cnats type appears in them.
 * - @b service: the client, the buffered subscription, the JetStream
 *   administration, the JWKS key fetch, the timeouts and the retry policy.
 *
 * Transport security: TLS (@c tls+tcp://) with an optional CA and client
 * certificate, and a JWT RS256 bearer token in the @c Authorization header
 * for client identity.
 *
 * Contrast with ores.eventing, which is in-process pub/sub with no network.
 */
namespace ores::nats {}

#endif
