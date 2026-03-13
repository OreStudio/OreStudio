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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_EVENTING_ORES_EVENTING_HPP
#define ORES_EVENTING_ORES_EVENTING_HPP

/**
 * @brief In-process event bus — decoupled communication within a single process.
 *
 * Provides type-safe publish/subscribe between components running in the same
 * process (e.g. handlers inside ores.comms.service, or between service layers).
 * Does not cross process boundaries and has no network dependency.
 *
 * - @b domain sub-namespace: event_traits<T> — compile-time mapping from a
 *   C++ event type to its string name (e.g. "ores.refdata.currency_changed").
 *   Also defines entity_change_event, the common payload for all domain
 *   change notifications.
 *
 * - @b service sub-namespace: event_bus — thread-safe in-process pub/sub.
 *   postgres_event_source bridges PostgreSQL LISTEN/NOTIFY into the event bus
 *   so that database-side changes (triggers, pg_notify) raise in-process
 *   events without polling.
 *
 * The event_traits string names defined here are reused as NATS subject
 * suffixes by ores.nats when publishing events to external subscribers.
 *
 * Contrast with ores.nats (external NATS bus, cross-process) and
 * ores.mq (durable PostgreSQL-backed queues, persistent across restarts).
 */
namespace ores::eventing {}

#endif
