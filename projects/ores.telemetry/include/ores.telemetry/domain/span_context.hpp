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
#ifndef ORES_TELEMETRY_DOMAIN_SPAN_CONTEXT_HPP
#define ORES_TELEMETRY_DOMAIN_SPAN_CONTEXT_HPP

#include <cstdint>
#include "ores.telemetry/domain/trace_id.hpp"
#include "ores.telemetry/domain/span_id.hpp"

namespace ores::telemetry::domain {

/**
 * @brief Immutable context for trace propagation across boundaries.
 *
 * A span_context contains the identifying information for a span and is
 * designed to be propagated across process, service, and network boundaries.
 * This is the core data structure used for distributed tracing context
 * propagation, compatible with W3C Trace Context.
 */
struct span_context final {
    /**
     * @brief The trace identifier. All spans in a trace share this ID.
     */
    trace_id trace;

    /**
     * @brief The span identifier. Unique within a trace.
     */
    span_id span;

    /**
     * @brief W3C trace flags (8-bit). Bit 0 indicates if the trace is sampled.
     *
     * 0x01 = sampled (the trace should be recorded)
     * 0x00 = not sampled
     */
    std::uint8_t trace_flags = 0x01;

    /**
     * @brief Checks if the context is valid (both trace_id and span_id valid).
     */
    bool is_valid() const;

    /**
     * @brief Checks if this trace is marked as sampled.
     */
    bool is_sampled() const;

    /**
     * @brief Default comparison operator.
     */
    auto operator<=>(const span_context&) const = default;
};

}

#endif
