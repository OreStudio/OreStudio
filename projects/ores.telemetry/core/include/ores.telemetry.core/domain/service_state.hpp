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
#ifndef ORES_TELEMETRY_CORE_DOMAIN_SERVICE_STATE_HPP
#define ORES_TELEMETRY_CORE_DOMAIN_SERVICE_STATE_HPP

#include <string_view>

namespace ores::telemetry::domain {

/**
 * @brief The state of one expected service instance on the services roster.
 *
 * The state is read from the instance's heartbeats alone, so it says whether
 * the instance reported and when, not why it stopped.
 */
enum class service_state {
    /**
     * @brief The instance reported within the running window.
     */
    running = 0,

    /**
     * @brief The instance reported before, but not within the running window.
     */
    stopped = 1,

    /**
     * @brief No instance has reported for this expected slot.
     */
    missing = 2
};

/**
 * @brief Converts a service_state to its string representation.
 */
[[nodiscard]] constexpr std::string_view to_string(service_state s) {
    switch (s) {
        case service_state::running:
            return "running";
        case service_state::stopped:
            return "stopped";
        case service_state::missing:
            return "missing";
    }
    return "unknown";
}

}

#endif
