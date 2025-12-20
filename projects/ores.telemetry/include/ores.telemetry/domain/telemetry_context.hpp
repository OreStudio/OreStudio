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
#ifndef ORES_TELEMETRY_DOMAIN_TELEMETRY_CONTEXT_HPP
#define ORES_TELEMETRY_DOMAIN_TELEMETRY_CONTEXT_HPP

#include <memory>
#include <string_view>
#include "ores.telemetry/domain/span_context.hpp"
#include "ores.telemetry/domain/span_kind.hpp"
#include "ores.telemetry/domain/span.hpp"
#include "ores.telemetry/domain/resource.hpp"

namespace ores::telemetry::domain {

/**
 * @brief Immutable context passed through the call chain for telemetry.
 *
 * The telemetry_context is designed to be passed as an explicit parameter
 * to all functions that participate in distributed tracing. This approach
 * works well with coroutines and makes data flow explicit.
 *
 * Usage:
 * @code
 * awaitable<void> handle_request(const request& req,
 *                                const telemetry_context& ctx) {
 *     auto [child_ctx, child_span] = ctx.start_span("process_request");
 *     // ... do work, pass child_ctx to nested calls ...
 *     child_span.end_time = std::chrono::system_clock::now();
 *     // ... record child_span to storage ...
 * }
 * @endcode
 */
class telemetry_context final {
public:
    /**
     * @brief Constructs a telemetry context.
     *
     * @param ctx The span context identifying the current span.
     * @param res The resource describing the entity producing telemetry.
     */
    explicit telemetry_context(span_context ctx,
                               std::shared_ptr<resource> res);

    /**
     * @brief Gets the current span context.
     */
    const span_context& context() const;

    /**
     * @brief Gets the trace_id from the current context.
     */
    const trace_id& get_trace_id() const;

    /**
     * @brief Gets the span_id from the current context.
     */
    const span_id& get_span_id() const;

    /**
     * @brief Gets the resource associated with this context.
     */
    const resource& get_resource() const;

    /**
     * @brief Gets the shared pointer to the resource.
     */
    std::shared_ptr<resource> resource_ptr() const;

    /**
     * @brief Creates a new child span within the same trace.
     *
     * The returned span has this context's span_id as its parent.
     *
     * @param name The name of the new span.
     * @param kind The kind of span (default: internal).
     * @return A pair of the new child context and the span to be recorded.
     */
    std::pair<telemetry_context, span> start_span(
        std::string_view name,
        span_kind kind = span_kind::internal) const;

    /**
     * @brief Creates a new trace linked to the current span.
     *
     * This is used when starting independent work that should be correlated
     * with the current trace but not as a child span. For example, when a
     * grid engine picks up work triggered by a client request.
     *
     * @param name The name of the new span.
     * @param kind The kind of span (default: internal).
     * @return A pair of the new context (with new trace_id) and the span.
     */
    std::pair<telemetry_context, span> start_linked_trace(
        std::string_view name,
        span_kind kind = span_kind::internal) const;

    /**
     * @brief Checks if this context is valid.
     */
    bool is_valid() const;

private:
    span_context ctx_;
    std::shared_ptr<resource> resource_;
};

}

#endif
