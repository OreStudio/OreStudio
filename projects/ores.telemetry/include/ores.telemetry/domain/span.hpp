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
#ifndef ORES_TELEMETRY_DOMAIN_SPAN_HPP
#define ORES_TELEMETRY_DOMAIN_SPAN_HPP

#include <chrono>
#include <optional>
#include <string>
#include <vector>
#include "ores.telemetry/domain/span_context.hpp"
#include "ores.telemetry/domain/span_kind.hpp"
#include "ores.telemetry/domain/span_status.hpp"
#include "ores.telemetry/domain/span_link.hpp"
#include "ores.telemetry/domain/attribute_value.hpp"

namespace ores::telemetry::domain {

/**
 * @brief Represents a single operation within a trace.
 *
 * A span is the primary building block of a distributed trace. It represents
 * a single operation and contains timing information, a name, attributes,
 * and relationships to other spans. Spans form a tree structure through
 * parent-child relationships, and can also form a hypergraph through links.
 *
 * This structure is designed to be compatible with OpenTelemetry's span model.
 */
struct span final {
    /**
     * @brief The span's identity (trace_id + span_id + flags).
     */
    span_context context;

    /**
     * @brief The parent span's ID, if this is not a root span.
     */
    std::optional<span_id> parent_span_id;

    /**
     * @brief Links to other spans forming hypergraph relationships.
     */
    std::vector<span_link> links;

    /**
     * @brief A human-readable name describing the operation.
     *
     * Examples: "HTTP GET /users", "database.query", "calculate_pv01"
     */
    std::string name;

    /**
     * @brief The kind of span (internal, server, client, producer, consumer).
     */
    span_kind kind = span_kind::internal;

    /**
     * @brief When the span started.
     */
    std::chrono::system_clock::time_point start_time;

    /**
     * @brief When the span ended. Empty if the span is still in progress.
     */
    std::optional<std::chrono::system_clock::time_point> end_time;

    /**
     * @brief The status code of the operation.
     */
    span_status_code status_code = span_status_code::unset;

    /**
     * @brief A message describing the status, typically set when status is error.
     */
    std::string status_message;

    /**
     * @brief Key-value pairs providing additional context about the operation.
     *
     * OpenTelemetry semantic conventions define standard attribute names.
     */
    attributes attrs;

    /**
     * @brief Checks if this span represents a session (has session.id attribute).
     */
    bool is_session() const;

    /**
     * @brief Checks if this is a root span (no parent).
     */
    bool is_root() const;

    /**
     * @brief Calculates the duration of the span if it has ended.
     */
    std::optional<std::chrono::nanoseconds> duration() const;
};

}

#endif
