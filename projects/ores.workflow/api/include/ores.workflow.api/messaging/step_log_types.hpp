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
#ifndef ORES_WORKFLOW_API_MESSAGING_STEP_LOG_TYPES_HPP
#define ORES_WORKFLOW_API_MESSAGING_STEP_LOG_TYPES_HPP

#include <cstdint>
#include <stdexcept>
#include <string>
#include <string_view>

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
 * @brief A single structured log entry emitted by a step handler.
 *
 * Entries are stored as a JSON array in workflow_step.step_log_json and
 * surfaced in the workflow instance detail dialog.  The context field
 * carries item-level identifiers (trade ID, filename, etc.) so the user
 * can locate the source of each message.
 */
struct step_log_entry {
    step_log_level level = step_log_level::info;
    std::string message;
    std::string context;
};

} // namespace ores::workflow::messaging

#endif
