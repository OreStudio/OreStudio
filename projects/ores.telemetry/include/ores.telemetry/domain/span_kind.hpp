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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_TELEMETRY_DOMAIN_SPAN_KIND_HPP
#define ORES_TELEMETRY_DOMAIN_SPAN_KIND_HPP

#include <cstdint>

namespace ores::telemetry::domain {

/**
 * @brief The type of span, following OpenTelemetry conventions.
 *
 * SpanKind describes the relationship between the span, its parents, and
 * its children in a trace. It helps visualization tools and analysis
 * systems understand the structure of the distributed trace.
 */
enum class span_kind : std::uint8_t {
    /**
     * @brief Default value. Indicates an internal operation within an
     * application, not representing a remote call.
     */
    internal = 0,

    /**
     * @brief Indicates that the span covers the server-side handling of a
     * synchronous RPC or other remote request.
     */
    server = 1,

    /**
     * @brief Indicates that the span describes a request to some remote
     * service. This is often the client-side of an RPC call.
     */
    client = 2,

    /**
     * @brief Indicates that the span describes a producer sending a message
     * to a message broker. Unlike client and server, there is often no direct
     * relationship between producer and consumer spans.
     */
    producer = 3,

    /**
     * @brief Indicates that the span describes a consumer receiving a message
     * from a message broker.
     */
    consumer = 4
};

}

#endif
