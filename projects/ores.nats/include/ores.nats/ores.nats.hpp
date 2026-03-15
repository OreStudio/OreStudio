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
 * @brief NATS transport layer — external message bus, cross-process connectivity.
 *
 * Provides RAII wrappers around the cnats C library and higher-level
 * facilities for domain services, the Qt client, and JetStream consumers.
 * All components that communicate over NATS depend on this library rather
 * than on cnats directly.
 *
 * Subject naming conventions:
 * - Request/reply:         @c ores.<domain>.<operation>
 *   (e.g. @c ores.iam.login)
 * - Pub/sub events:        @c <tenant_id>.ores.<domain>.<event_name>
 *   (e.g. @c 499d2aaa.ores.refdata.currency_changed)
 * - JetStream streams:     @c ores.<domain>.js.<stream>
 *   (e.g. @c ores.trading.js.trades)
 *
 * Sub-namespaces (planned):
 * - @b connection: RAII wrappers for natsConnection and natsOptions; TLS
 *   configuration; connection lifecycle management.
 * - @b service: base class for domain microservices; binds a NATS subject
 *   and dispatches incoming requests to handler coroutines.
 * - @b client: Qt-side session; sends requests and receives replies via
 *   unique per-request _INBOX subjects (fully concurrent, no serialisation).
 * - @b messaging: typed request/reply message helpers built on top of the
 *   cnats publish/subscribe primitives.
 * - @b jetstream: JetStream consumer support; durable subscriptions for
 *   chart history replay and work-queue delivery to grid nodes.
 *
 * Transport security: TLS (@c tls+tcp://) for encryption and server
 * authentication; JWT RS256 tokens in message headers for client identity.
 *
 * Contrast with ores.eventing (in-process pub/sub only, no network) and
 * ores.mq (durable PostgreSQL-backed queues, persistent across restarts).
 */
namespace ores::nats {}

#endif
