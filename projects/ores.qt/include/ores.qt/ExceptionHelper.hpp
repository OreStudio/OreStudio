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

#include <optional>
#include <QString>
#include <exception>
#include <boost/exception/diagnostic_information.hpp>
#include "ores.logging/boost_severity.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.comms/messaging/error_protocol.hpp"

namespace ores::qt {

/**
 * @brief Result of checking a response frame for server errors.
 */
struct server_error_info {
    QString message;
    QString details;
};

/**
 * @brief Helper class for handling exceptions and server errors in async operations.
 *
 * Provides standardized handling for:
 * - Exceptions thrown from QFuture operations
 * - Server error responses (message_type::error_response)
 */
class exception_helper final {
public:
    /**
     * @brief Check if a response frame is an error_response and extract the message.
     *
     * Call this after receiving a response but before attempting to deserialize
     * as the expected response type. If the server sent an error_response, this
     * extracts the human-readable error message.
     *
     * @param response The response frame from sendRequest().
     * @return The error info if this is an error_response, nullopt otherwise.
     */
    static std::optional<server_error_info>
    check_error_response(const comms::messaging::frame& response) {
        using comms::messaging::message_type;
        using comms::messaging::error_response;

        if (response.header().type != message_type::error_response) {
            return std::nullopt;
        }

        auto payload_result = response.decompressed_payload();
        if (!payload_result) {
            return server_error_info{
                .message = "Server returned an error (failed to decompress)",
                .details = {}
            };
        }

        auto err_resp = error_response::deserialize(*payload_result);
        if (!err_resp) {
            return server_error_info{
                .message = "Server returned an error (failed to parse)",
                .details = {}
            };
        }

        return server_error_info{
            .message = QString("Server error: %1")
                .arg(QString::fromStdString(err_resp->message)),
            .details = QString("Error code: %1 (%2)")
                .arg(QString::fromStdString(
                    ores::utility::serialization::to_string(err_resp->code)))
                .arg(static_cast<int>(err_resp->code))
        };
    }

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

    /**
     * @brief Wraps an async fetch operation to capture exceptions before Qt can wrap them.
     *
     * This function catches exceptions inside the async task (before Qt's
     * exception handling), capturing full diagnostic information. The result
     * type must have the following fields:
     * - bool success
     * - QString error_message
     * - QString error_details
     *
     * @tparam ResultType The FetchResult type (must have success, error_message, error_details).
     * @tparam FetchFunc Callable that returns ResultType.
     * @param fetch_func The function that performs the actual fetch.
     * @param entity_name The name of the entity being fetched (for error messages).
     * @return ResultType with success=false and error fields populated on exception.
     */
    template<typename ResultType, typename FetchFunc>
    static ResultType wrap_async_fetch(
        FetchFunc&& fetch_func,
        const QString& entity_name) {

        try {
            return fetch_func();
        } catch (const std::exception& e) {
            ResultType result{};
            result.success = false;
            result.error_message = QString("Failed to fetch %1 from server.")
                .arg(entity_name);
            result.error_details = QString::fromStdString(
                boost::diagnostic_information(e));
            return result;
        }
    }
};

}

#endif
