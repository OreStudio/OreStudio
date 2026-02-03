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
#ifndef ORES_TELEMETRY_DATABASE_LOG_DATABASE_SINK_UTILS_HPP
#define ORES_TELEMETRY_DATABASE_LOG_DATABASE_SINK_UTILS_HPP

#include <memory>
#include <functional>
#include "ores.telemetry/log/database_sink_backend.hpp"
#include "ores.telemetry.database/repository/telemetry_repository.hpp"

namespace ores::telemetry::database::log {

/**
 * @brief Creates a database log handler that stores entries to the telemetry repository.
 *
 * This utility function creates a handler function that can be used with the
 * database sink backend to store log entries directly to the database.
 *
 * @param repo Shared pointer to the telemetry repository.
 * @return A handler function that stores log entries to the database.
 */
inline log::database_log_handler make_database_handler(
    std::shared_ptr<repository::telemetry_repository> repo) {
    return [repo](const domain::telemetry_log_entry& entry) {
        try {
            repo->create(entry);
        } catch (const std::exception& ex) {
            // In a real implementation, we might want to log this error somewhere
            // but we shouldn't throw from a logging sink as it could cause issues
            // Just silently fail to avoid disrupting the application
        }
    };
}

/**
 * @brief Creates a database log handler that stores entries to the telemetry repository with error callback.
 *
 * This utility function creates a handler function that can be used with the
 * database sink backend to store log entries directly to the database, with
 * an optional error callback for handling exceptions.
 *
 * @param repo Shared pointer to the telemetry repository.
 * @param error_callback Optional callback function to handle exceptions during database storage.
 * @return A handler function that stores log entries to the database.
 */
inline log::database_log_handler make_database_handler_with_error_callback(
    std::shared_ptr<repository::telemetry_repository> repo,
    std::function<void(const std::exception&)> error_callback = {}) {
    return [repo, error_callback](const domain::telemetry_log_entry& entry) {
        try {
            repo->create(entry);
        } catch (const std::exception& ex) {
            if (error_callback) {
                error_callback(ex);
            }
        }
    };
}

}

#endif