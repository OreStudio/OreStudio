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
#ifndef ORES_TELEMETRY_LOG_DATABASE_SINK_UTILS_HPP
#define ORES_TELEMETRY_LOG_DATABASE_SINK_UTILS_HPP

#include <memory>
#include <functional>
#include "ores.telemetry/log/database_sink_backend.hpp"

namespace ores::telemetry::log {

/**
 * @brief Creates a generic database log handler that simply forwards entries.
 *
 * This utility function creates a handler function that can be used with the
 * database sink backend. The actual storage mechanism is left to the caller
 * to implement, which avoids circular dependencies with the repository.
 *
 * @param handler A function that takes a telemetry_log_entry and stores it appropriately.
 * @return A handler function that can be used with the database sink backend.
 */
inline database_log_handler make_forwarding_handler(
    std::function<void(const domain::telemetry_log_entry&)> handler) {
    return [handler](const domain::telemetry_log_entry& entry) {
        try {
            handler(entry);
        } catch (const std::exception& ex) {
            // In a real implementation, we might want to log this error somewhere
            // but we shouldn't throw from a logging sink as it could cause issues
            // Just silently fail to avoid disrupting the application
        }
    };
}

}

#endif