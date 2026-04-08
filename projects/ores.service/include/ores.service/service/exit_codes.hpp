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
#ifndef ORES_SERVICE_SERVICE_EXIT_CODES_HPP
#define ORES_SERVICE_SERVICE_EXIT_CODES_HPP

#include <string_view>

namespace ores::service::service {

/**
 * @brief Well-known process exit codes for ORE Studio services.
 *
 * Services must exit with these values so the controller can diagnose
 * failures without parsing log files. Add new codes here as new failure
 * categories are discovered; never reuse an existing value.
 */
enum class exit_code : int {
    ok                     = 0,  ///< Clean shutdown (normal or graceful signal)
    general_error          = 1,  ///< Unclassified runtime error
    config_error           = 2,  ///< Missing / invalid configuration
    db_connection_failed   = 3,  ///< Could not acquire a PostgreSQL connection
    nats_connection_failed = 4,  ///< Could not connect to NATS
    startup_timeout        = 5,  ///< Timed out waiting for a dependency
    auth_error             = 6,  ///< Authentication or authorisation failure
};

/**
 * @brief Returns a short human-readable name for the exit code.
 *
 * Intended for use in log messages and service_event records so operators
 * can read the reason without looking up integer constants.
 */
inline constexpr std::string_view exit_code_name(exit_code code) noexcept {
    switch (code) {
    case exit_code::ok:                     return "ok";
    case exit_code::general_error:          return "general_error";
    case exit_code::config_error:           return "config_error";
    case exit_code::db_connection_failed:   return "db_connection_failed";
    case exit_code::nats_connection_failed: return "nats_connection_failed";
    case exit_code::startup_timeout:        return "startup_timeout";
    case exit_code::auth_error:             return "auth_error";
    default:                                return "unknown";
    }
}

/**
 * @brief Converts a raw int exit code to the enum.
 *
 * Values not in the enum map to @c exit_code::general_error so the
 * controller always gets a valid enum regardless of what the child process
 * returned.
 */
inline constexpr exit_code to_exit_code(int raw) noexcept {
    switch (raw) {
    case 0: return exit_code::ok;
    case 1: return exit_code::general_error;
    case 2: return exit_code::config_error;
    case 3: return exit_code::db_connection_failed;
    case 4: return exit_code::nats_connection_failed;
    case 5: return exit_code::startup_timeout;
    case 6: return exit_code::auth_error;
    default: return exit_code::general_error;
    }
}

}

#endif
