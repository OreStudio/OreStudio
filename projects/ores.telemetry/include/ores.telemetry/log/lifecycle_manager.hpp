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
#include <filesystem>
#include <boost/shared_ptr.hpp>
#include <boost/log/sinks.hpp>
#include "ores.telemetry/log/boost_severity.hpp"
#include "ores.telemetry/log/logging_options.hpp"
#include "ores.telemetry/log/telemetry_sink_backend.hpp"
#include "ores.telemetry/domain/resource.hpp"

namespace ores::telemetry::log {

/**
 * @brief Manages the starting and stopping of logging for an application.
 *
 * This class handles the lifecycle of all logging sinks including console,
 * file, and telemetry sinks. The telemetry sink enables correlation of logs
 * with distributed traces by extracting trace_id and span_id from log
 * attributes.
 *
 * Note: this class uses boost shared_ptr due to legacy reasons (boost log does
 * not support std::shared_ptr).
 */
class lifecycle_manager final {
private:
    using file_sink_type = boost::log::sinks::synchronous_sink<
        boost::log::sinks::text_file_backend>;
    using console_sink_type = boost::log::sinks::synchronous_sink<
        boost::log::sinks::text_ostream_backend>;
    using telemetry_sink_type = boost::log::sinks::asynchronous_sink<
        telemetry_sink_backend>;

public:
    lifecycle_manager() = delete;
    lifecycle_manager(lifecycle_manager&&) = delete;
    lifecycle_manager& operator=(const lifecycle_manager&) = delete;

private:
    /**
     * @brief Creates a boost log file sink.
     *
     * @note path is non-const by ref by design.
     */
  static boost::shared_ptr<file_sink_type>
  make_file_sink(std::filesystem::path path, boost_severity severity,
      std::string tag);

    /**
     * @brief Creates a boost log console sink.
     */
    static boost::shared_ptr<console_sink_type> make_console_sink(
        boost_severity severity, std::string tag);

public:

    /**
     * @brief Initialise logging for the entire application.
     *
     * If no configuration is supplied, logging is disabled.
     *
     * @note Must be done in a thread-safe context.
     */
    explicit lifecycle_manager(std::optional<logging_options> ocfg);

    /**
     * @brief Shutdown logging for the entire application.
     *
     * Should be done in a thread-safe context.
     */
    ~lifecycle_manager();

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

private:
    boost::shared_ptr<file_sink_type> file_sink_;
    boost::shared_ptr<console_sink_type> console_sink_;
    boost::shared_ptr<telemetry_sink_type> telemetry_sink_;
};

}

#endif
