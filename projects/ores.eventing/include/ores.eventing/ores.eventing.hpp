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
 * @brief Event-driven architecture infrastructure for decoupled component communication.
 *
 * Provides a publish/subscribe event bus for decoupled communication between
 * components without direct dependencies. Key features:
 *
 * - Event Bus: In-process publish/subscribe with type-safe callbacks
 * - Event Traits: Type-to-name mapping for protocol integration
 * - Subscriptions: RAII-based subscription management with auto-unsubscribe
 * - PostgreSQL Events: LISTEN/NOTIFY bridge for database change notifications
 * - Entity Change Events: Domain-agnostic change notification with timestamps
 * - Thread Safety: All event bus operations are thread-safe
 *
 * The domain namespace defines event types, traits, and entity change events.
 * The service namespace provides the event_bus implementation and PostgreSQL
 * integration via postgres_listener_service and postgres_event_source.
 */
namespace ores::eventing {}

#endif
