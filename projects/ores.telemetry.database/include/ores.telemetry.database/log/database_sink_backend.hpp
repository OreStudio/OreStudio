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
#ifndef ORES_TELEMETRY_DATABASE_LOG_DATABASE_SINK_HPP
#define ORES_TELEMETRY_DATABASE_LOG_DATABASE_SINK_HPP

#include <memory>
#include <string>
#include <functional>
#include <boost/log/sinks/basic_sink_backend.hpp>
#include <boost/log/sinks/frontend_requirements.hpp>
#include <boost/uuid/uuid.hpp>
#include "ores.telemetry/domain/resource.hpp"
#include "ores.telemetry/domain/telemetry_log_entry.hpp"
#include "ores.database/domain/context.hpp"

namespace ores::telemetry::database::log {

/**
 * @brief Handler function type for processing log entries destined for database storage.
 *
 * The handler is called for each log entry after conversion from Boost.Log format.
 * Implementations may store to database, cache for batch operations, or perform
 * any other processing needed for database persistence.
 */
using database_log_handler = std::function<void(const domain::telemetry_log_entry&)>;

/**
 * @brief Boost.Log sink backend that prepares logs for database storage.
 *
 * This sink extracts log information from Boost.Log records and converts them
 * to the telemetry domain model for storage in the database. It's designed to 
 * be used primarily for unit testing scenarios where logs need to be captured 
 * in the database for inspection and validation.
 *
 * The sink can be enabled/disabled via configuration and operates independently
 * of other logging sinks (console, file, telemetry export). It accepts a handler
 * function that performs the actual database storage, allowing for flexibility
 * in how the data is persisted.
 *
 * Usage:
 * @code
 * auto resource = std::make_shared<domain::resource>(
 *     domain::resource::from_environment("test-app", "1.0.0"));
 * auto handler = [](const domain::telemetry_log_entry& entry) {
 *     // Store to database
 * };
 * auto backend = boost::make_shared<database_sink_backend>(resource, handler);
 * auto sink = boost::make_shared<
 *     boost::log::sinks::asynchronous_sink<database_sink_backend>>(backend);
 * boost::log::core::get()->add_sink(sink);
 * @endcode
 */
class database_sink_backend :
    public boost::log::sinks::basic_sink_backend<
        boost::log::sinks::synchronized_feeding> {
public:
    /**
     * @brief Constructs the database sink backend.
     *
     * @param resource The resource describing the entity producing logs.
     *                 Shared across all log records from this source.
     * @param handler Function called for each converted log entry.
     * @param source_type Type of source ('client' or 'server', default: 'test').
     * @param source_name Name of the source application (default: 'unit-test').
     */
    explicit database_sink_backend(
        std::shared_ptr<domain::resource> resource,
        database_log_handler handler,
        const std::string& source_type = "test",
        const std::string& source_name = "unit-test");

    /**
     * @brief Processes a Boost.Log record and converts it for database storage.
     *
     * This method is called by Boost.Log for each log record that passes
     * the sink's filter. It extracts all relevant attributes and creates
     * a telemetry_log_entry for the handler to store in the database.
     *
     * @param rec The Boost.Log record view to process.
     */
    void consume(const boost::log::record_view& rec) override;

    /**
     * @brief Sets the session ID for logs produced by this sink.
     *
     * This is useful for correlating test logs with specific test sessions.
     *
     * @param session_id The session UUID to associate with logs.
     */
    void set_session_id(const boost::uuids::uuid& session_id);

    /**
     * @brief Sets the account ID for logs produced by this sink.
     *
     * This is useful for associating test logs with specific accounts.
     *
     * @param account_id The account UUID to associate with logs.
     */
    void set_account_id(const boost::uuids::uuid& account_id);

private:
    std::shared_ptr<domain::resource> resource_;
    database_log_handler handler_;
    std::string source_type_;
    std::string source_name_;
    boost::uuids::uuid session_id_;
    boost::uuids::uuid account_id_;
    bool has_session_id_ = false;
    bool has_account_id_ = false;
};

}

#endif