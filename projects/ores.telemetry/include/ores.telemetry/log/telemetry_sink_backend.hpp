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
#ifndef ORES_TELEMETRY_LOG_TELEMETRY_SINK_BACKEND_HPP
#define ORES_TELEMETRY_LOG_TELEMETRY_SINK_BACKEND_HPP

#include <memory>
#include <functional>
#include <boost/log/sinks/basic_sink_backend.hpp>
#include <boost/log/sinks/frontend_requirements.hpp>
#include "ores.telemetry/domain/log_record.hpp"
#include "ores.telemetry/domain/resource.hpp"

namespace ores::telemetry::log {

/**
 * @brief Boost.Log sink backend that creates telemetry log_records.
 *
 * This sink extracts trace context from log attributes and constructs
 * OpenTelemetry-compatible log_records for export. It integrates with the
 * existing Boost.Log infrastructure, receiving all log records and converting
 * them to the telemetry domain model.
 *
 * Usage:
 * @code
 * auto resource = domain::resource::from_environment("my-service", "1.0.0");
 * auto backend = boost::make_shared<telemetry_sink_backend>(
 *     resource,
 *     [](domain::log_record rec) { exporter.export_record(std::move(rec)); }
 * );
 * auto sink = boost::make_shared<
 *     boost::log::sinks::asynchronous_sink<telemetry_sink_backend>>(backend);
 * boost::log::core::get()->add_sink(sink);
 * @endcode
 */
class telemetry_sink_backend :
    public boost::log::sinks::basic_sink_backend<
        boost::log::sinks::synchronized_feeding> {
public:
    /**
     * @brief Handler function type for processing log records.
     *
     * The handler is called for each log record after conversion to the
     * telemetry domain model. Implementations may export to file, send to
     * a collector, or perform any other processing.
     */
    using log_record_handler = std::function<void(domain::log_record)>;

    /**
     * @brief Constructs the telemetry sink backend.
     *
     * @param res The resource describing the entity producing logs.
     *            Shared across all log records from this source.
     * @param handler Function called for each converted log record.
     */
    explicit telemetry_sink_backend(std::shared_ptr<domain::resource> res,
                                     log_record_handler handler);

    /**
     * @brief Processes a Boost.Log record and converts it to telemetry format.
     *
     * This method is called by Boost.Log for each log record that passes
     * the sink's filter. It extracts all relevant attributes, converts the
     * severity level, and creates a domain::log_record with trace correlation
     * if trace_id and span_id attributes are present.
     *
     * @param rec The Boost.Log record view to process.
     */
    void consume(const boost::log::record_view& rec);

private:
    std::shared_ptr<domain::resource> resource_;
    log_record_handler handler_;
};

}

#endif
