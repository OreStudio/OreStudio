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
#ifndef ORES_QT_EXCEPTION_HELPER_HPP
#define ORES_QT_EXCEPTION_HELPER_HPP

#include <QString>
#include <exception>
#include <boost/exception/diagnostic_information.hpp>
#include "ores.logging/boost_severity.hpp"

namespace ores::qt {

/**
 * @brief Helper class for handling exceptions in async fetch operations.
 *
 * Provides a standardized way to handle exceptions thrown from QFuture
 * operations, extracting detailed diagnostic information and emitting
 * user-friendly error messages.
 */
class exception_helper final {
public:
    /**
     * @brief Handles a fetch exception by logging and emitting an error signal.
     *
     * Extracts detailed diagnostic information from the exception using
     * boost::diagnostic_information, logs the error, and calls the emit
     * callback with a user-friendly message and technical details.
     *
     * @tparam Logger The logger type (must support BOOST_LOG_SEV).
     * @tparam EmitFunc Callable that takes (const QString& message, const QString& details).
     * @param e The exception that was caught.
     * @param entity_name The name of the entity being fetched (e.g., "origin dimensions").
     * @param logger Reference to the logger instance.
     * @param emit_error Callback to emit the error signal.
     */
    template<typename Logger, typename EmitFunc>
    static void handle_fetch_exception(
        const std::exception& e,
        const QString& entity_name,
        Logger& logger,
        EmitFunc emit_error) {

        const auto details = QString::fromStdString(
            boost::diagnostic_information(e));

        BOOST_LOG_SEV(logger, ores::logging::error)
            << "Exception fetching " << entity_name.toStdString() << ": "
            << details.toStdString();

        const auto message = QString("Failed to fetch %1 from server.")
            .arg(entity_name);

        emit_error(message, details);
    }
};

}

#endif
