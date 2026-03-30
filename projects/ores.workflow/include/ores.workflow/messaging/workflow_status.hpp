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
#ifndef ORES_WORKFLOW_MESSAGING_WORKFLOW_STATUS_HPP
#define ORES_WORKFLOW_MESSAGING_WORKFLOW_STATUS_HPP

#include <cstdint>
#include <ostream>
#include <string_view>

namespace ores::workflow::domain {

/**
 * @brief Lifecycle status values for workflow instances and steps.
 *
 * Represents the current execution state of a workflow instance or step.
 * Terminal states are completed, failed, and compensated.
 */
enum class workflow_status : std::uint8_t {
    // Created but not yet started.
    pending = 0,
    // Currently executing.
    in_progress = 1,
    // Finished successfully (terminal).
    completed = 2,
    // Finished with an error (terminal).
    failed = 3,
    // Rolling back completed steps.
    compensating = 4,
    // Rollback finished (terminal).
    compensated = 5
};

/**
 * @brief Convert workflow_status to string representation.
 *
 * @param v The enum value to convert.
 * @return String view of the enum name, or empty for unknown values.
 */
[[nodiscard]] constexpr std::string_view to_string(workflow_status v) noexcept {
    switch (v) {
    case workflow_status::pending: return "pending";
    case workflow_status::in_progress: return "in_progress";
    case workflow_status::completed: return "completed";
    case workflow_status::failed: return "failed";
    case workflow_status::compensating: return "compensating";
    case workflow_status::compensated: return "compensated";
    default: return {};
    }
}

/**
 * @brief Stream output operator for workflow_status.
 *
 * Outputs the enum name followed by the hex value in parentheses.
 * Example: "get_currencies_request (0x1001)"
 * If the value is unknown, outputs "[unknown]" prefix.
 */
inline std::ostream& operator<<(std::ostream& os, workflow_status v) {
    const auto name = to_string(v);
    if (name.empty()) {
        os << "[unknown]";
    } else {
        os << name;
    }
    return os << " (0x" << std::hex
              << static_cast<std::uint8_t>(v)
              << std::dec << ")";
}

}

#endif
