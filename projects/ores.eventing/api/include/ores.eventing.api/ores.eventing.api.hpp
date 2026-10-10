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
#ifndef ORES_EVENTING_API_ORES_EVENTING_API_HPP
#define ORES_EVENTING_API_ORES_EVENTING_API_HPP

/**
 * @brief In-process event bus — decoupled communication within a single process.
 *
 * Provides type-safe publish/subscribe between components running in the same
 * process (e.g. handlers inside ores.comms.service, or between service layers).
 * Does not cross process boundaries and has no network dependency.
 *
 * - @b domain sub-namespace: entity_event_traits<T> — compile-time mapping from
 *   an entity's change event to the NATS subject prefix it is published on
 *   (e.g. "refdata.v1.countries_events"). event_traits<T> names an in-process
 *   domain event that is not an entity change (e.g. "ores.iam.role_assigned").
 *   Also defines entity_change_event, the common payload for all domain
 *   change notifications.
 *
 * - @b service sub-namespace: event_bus — thread-safe in-process pub/sub.
 *   postgres_event_source bridges PostgreSQL LISTEN/NOTIFY into the event bus
 *   so that database-side changes (triggers, pg_notify) raise in-process
 *   events without polling.
 *
 * An entity's change event is published to NATS on the subject its
 * entity_event_traits state, the prefix and the action. An in-process domain
 * event is not published to NATS.
 *
 * Contrast with ores.nats (external NATS bus, cross-process) and
 * ores.mq (durable PostgreSQL-backed queues, persistent across restarts).
 */
namespace ores::eventing {}

#endif
