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
#ifndef ORES_TELEMETRY_LOG_LIFECYCLE_MANAGER_HPP
#define ORES_TELEMETRY_LOG_LIFECYCLE_MANAGER_HPP

#include <memory>
#include <optional>
#include <boost/shared_ptr.hpp>
#include <boost/log/sinks.hpp>
#include "ores.logging/lifecycle_manager.hpp"
#include "ores.logging/logging_options.hpp"
#include "ores.telemetry/log/telemetry_sink_backend.hpp"
#include "ores.telemetry/log/database_sink_backend.hpp"
#include "ores.telemetry/domain/resource.hpp"

namespace ores::telemetry::log {

using ores::logging::logging_options;

/**
 * @brief Extends the base lifecycle_manager with telemetry sink support.
 *
 * This class inherits from ores::logging::lifecycle_manager and adds
 * the ability to attach a telemetry sink for OpenTelemetry log correlation.
 * The telemetry sink extracts trace_id and span_id from log attributes
 * and creates domain::log_record instances for export.
 *
 * Note: this class uses boost shared_ptr due to legacy reasons (boost log does
 * not support std::shared_ptr).
 */
class lifecycle_manager final : public ores::logging::lifecycle_manager {
private:
    using telemetry_sink_type = boost::log::sinks::asynchronous_sink<
        telemetry_sink_backend>;

public:
    lifecycle_manager() = delete;
    lifecycle_manager(lifecycle_manager&&) = delete;
    lifecycle_manager& operator=(const lifecycle_manager&) = delete;

public:
    /**
     * @brief Initialise logging for the entire application.
     *
     * If no configuration is supplied, logging is disabled.
     * This constructor delegates to the base class for console and file
     * sink setup, and initializes the telemetry sink member.
     *
     * @note Must be done in a thread-safe context.
     */
    explicit lifecycle_manager(std::optional<logging_options> ocfg);

    /**
     * @brief Shutdown logging for the entire application.
     *
     * Stops and flushes the telemetry sink before base class cleanup.
     */
    ~lifecycle_manager() override;

    /**
     * @brief Adds a telemetry sink for log record correlation.
     *
     * The telemetry sink extracts trace_id and span_id attributes from log
     * records and creates domain::log_record instances for export. The sink
     * is asynchronous to avoid blocking the logging thread.
     *
     * @param resource The resource describing the entity producing logs.
     * @param handler Function called for each converted log record.
     *
     * Example:
     * @code
     * auto resource = domain::resource::from_environment("my-service", "1.0");
     * auto exporter = std::make_shared<file_log_exporter>("logs/telemetry.jsonl");
     * lifecycle_manager lm(logging_options);
     * lm.add_telemetry_sink(resource, [exporter](auto rec) {
     *     exporter->export_record(std::move(rec));
     * });
     * @endcode
     */
    void add_telemetry_sink(
        std::shared_ptr<domain::resource> resource,
        telemetry_sink_backend::log_record_handler handler);

    /**
     * @brief Adds a database sink for direct database logging.
     *
     * The database sink converts log records to the telemetry domain model
     * and stores them in the database. This is primarily intended for
     * unit testing scenarios where logs need to be captured in the database
     * for inspection and validation.
     *
     * @param resource The resource describing the entity producing logs.
     * @param handler Function called for each converted log entry.
     * @param source_type Type of source ('client', 'server', 'test', etc.).
     * @param source_name Name of the source application.
     *
     * Example:
     * @code
     * auto resource = domain::resource::from_environment("test-app", "1.0");
     * auto repo = std::make_shared<telemetry_repository>(db_context);
     * lifecycle_manager lm(logging_options);
     * lm.add_database_sink(resource, [repo](const auto& entry) {
     *     repo->create(entry);  // Store to database
     * }, "test", "unit-test-suite");
     * @endcode
     */
    void add_database_sink(
        std::shared_ptr<domain::resource> resource,
        database_log_handler handler,
        const std::string& source_type = "test",
        const std::string& source_name = "unit-test");

private:
    boost::shared_ptr<telemetry_sink_type> telemetry_sink_;
    boost::shared_ptr<database_sink_backend> database_sink_; // Added for database sink
};

}

#endif
