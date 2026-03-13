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
#ifndef ORES_MQ_HPP
#define ORES_MQ_HPP

/**
 * @brief Durable PostgreSQL-backed message queues — persistent, transactional.
 *
 * Provides reliable, durable message queuing built on custom PostgreSQL MQ
 * tables. Messages survive process restarts and are delivered exactly once
 * under transactional guarantees. Suitable for work that must not be lost
 * (e.g. audit trails, deferred jobs, cross-service task dispatch).
 *
 * - @b domain sub-namespace: domain types for queue definitions, messages
 *   and statistics (queue_definition, mq_message, queue_stats, etc.).
 *
 * - @b repository sub-namespace: data access for MQ tables
 *   (queue_repository, message_repository, queue_stats_repository).
 *
 * - @b service sub-namespace: high-level mq_service facade that composes
 *   the repositories, and queue_listener for NOTIFY-driven event dispatch.
 *
 * - @b messaging sub-namespace: binary protocol support for the MQ subsystem
 *   (0xB000-0xBFFF). Provides get_queues, get_queue_stats, send/read/ack
 *   request/response messages so that Qt UI components can query queue state
 *   on demand. The registrar registers the mq_message_handler with the comms
 *   server.
 *
 * Contrast with ores.nats (external NATS bus, cross-process, no persistence
 * guarantee) and ores.eventing (in-process pub/sub only, no network).
 */
namespace ores::mq {}

#endif
