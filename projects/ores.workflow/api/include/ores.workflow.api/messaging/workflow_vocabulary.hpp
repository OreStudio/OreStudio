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
#ifndef ORES_WORKFLOW_API_MESSAGING_WORKFLOW_VOCABULARY_HPP
#define ORES_WORKFLOW_API_MESSAGING_WORKFLOW_VOCABULARY_HPP

#include <cstdint>
#include <rfl.hpp>
#include <stdexcept>
#include <string>
#include <string_view>

/**
 * @file
 * @brief The words of the workflow conversation that a model cannot state.
 *
 * The messages themselves are generated from ores.workflow.workflow_messages.
 * What stays here is what an operation model has no syntax for: the two
 * enumerations with their wire spellings and reflectors, and the names of the
 * NATS headers a step command carries. The enumerations are registered in
 * projects/modeling/cpp_custom_types.org, so the generated protocol includes
 * this header and the TypeScript twin carries each one as a string.
 */

namespace ores::workflow::messaging {

/**
 * @brief Severity of a step log entry.
 *
 * Serialises as a lowercase string ("info", "warn", "error") so that
 * step_log_json columns in the DB are human-readable and queryable via
 * JSON containment (@>).
 */
enum class step_log_level : std::uint8_t { info = 0, warn = 1, error = 2 };

[[nodiscard]] inline std::string_view to_string(step_log_level v) {
    switch (v) {
        case step_log_level::info:
            return "info";
        case step_log_level::warn:
            return "warn";
        case step_log_level::error:
            return "error";
    }
    throw std::invalid_argument("Out-of-range step_log_level");
}

[[nodiscard]] inline step_log_level step_log_level_from_string(std::string_view sv) {
    if (sv == "info")
        return step_log_level::info;
    if (sv == "warn")
        return step_log_level::warn;
    if (sv == "error")
        return step_log_level::error;
    throw std::invalid_argument("Invalid step_log_level: '" + std::string(sv) + "'");
}

/**
 * @brief Terminal outcome of a workflow step.
 *
 * Replaces the previous bool success field to express three distinct states:
 *   completed               — succeeded with no issues
 *   completed_with_warnings — ran to completion but some items failed
 *   failed                  — fatal; saga compensation will be triggered
 *
 * Serialises as a lowercase string for the same DB queryability reasons
 * as step_log_level.
 */
enum class step_outcome : std::uint8_t { completed = 0, completed_with_warnings = 1, failed = 2 };

[[nodiscard]] inline std::string_view to_string(step_outcome v) {
    switch (v) {
        case step_outcome::completed:
            return "completed";
        case step_outcome::completed_with_warnings:
            return "completed_with_warnings";
        case step_outcome::failed:
            return "failed";
    }
    throw std::invalid_argument("Out-of-range step_outcome");
}

[[nodiscard]] inline step_outcome step_outcome_from_string(std::string_view sv) {
    if (sv == "completed")
        return step_outcome::completed;
    if (sv == "completed_with_warnings")
        return step_outcome::completed_with_warnings;
    if (sv == "failed")
        return step_outcome::failed;
    throw std::invalid_argument("Invalid step_outcome: '" + std::string(sv) + "'");
}

/**
 * @brief NATS header carrying the workflow step id on a step command.
 *
 * The engine sets it when it dispatches a step command. A domain service reads
 * it to recognise a workflow command and to key its idempotency check, and
 * echoes it in the completion event.
 */
inline constexpr std::string_view step_id_header = "X-Workflow-Step-Id";

/**
 * @brief NATS header carrying the parent workflow instance id.
 */
inline constexpr std::string_view instance_id_header = "X-Workflow-Instance-Id";

/**
 * @brief NATS header carrying the tenant that owns the workflow instance.
 *
 * Set by the engine on every step command, so the receiving service can build
 * the tenant-scoped database context the step runs under.
 */
inline constexpr std::string_view tenant_id_header = "X-Tenant-Id";

}

namespace rfl {

template <>
struct Reflector<ores::workflow::messaging::step_log_level> {
    using ReflType = std::string;

    static ores::workflow::messaging::step_log_level to(const ReflType& s) {
        return ores::workflow::messaging::step_log_level_from_string(s);
    }

    static ReflType from(const ores::workflow::messaging::step_log_level& v) {
        return std::string(ores::workflow::messaging::to_string(v));
    }
};

template <>
struct Reflector<ores::workflow::messaging::step_outcome> {
    using ReflType = std::string;

    static ores::workflow::messaging::step_outcome to(const ReflType& s) {
        return ores::workflow::messaging::step_outcome_from_string(s);
    }

    static ReflType from(const ores::workflow::messaging::step_outcome& v) {
        return std::string(ores::workflow::messaging::to_string(v));
    }
};

}

#endif
