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
#ifndef ORES_TELEMETRY_CORE_HPP
#define ORES_TELEMETRY_CORE_HPP

/**
 * @brief Telemetry and observability infrastructure: the logging, tracing and
 * record types the other parts and every other component build on.
 *
 * The core module owns the component's outermost namespace, @c ores::telemetry,
 * because the types the other parts exchange live directly in it rather than in
 * a namespace of their own.
 *
 * Its sub-namespaces:
 * - @b domain: the log record, its resource and attributes, the trace and span
 *   identifiers, and the OpenTelemetry semantic conventions.
 * - @b log: the Boost.Log integration, the lifecycle manager and the sinks that
 *   convert a log record for storage or for forwarding.
 * - @b exporting: the exporter configuration and options.
 * - @b generators: the trace and span identifier generators.
 * - @b messaging: the log, NATS sample and service sample protocol messages.
 */
namespace ores::telemetry {}

#endif
