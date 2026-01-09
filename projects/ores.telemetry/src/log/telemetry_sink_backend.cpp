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
#include "ores.telemetry/log/telemetry_sink_backend.hpp"

#include <boost/log/attributes/value_extraction.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include "ores.logging/boost_severity.hpp"
#include "ores.telemetry/domain/trace_id.hpp"
#include "ores.telemetry/domain/span_id.hpp"

namespace ores::telemetry::log {

namespace {

/**
 * @brief Converts boost::posix_time::ptime to std::chrono::system_clock::time_point.
 */
std::chrono::system_clock::time_point to_system_clock(
    const boost::posix_time::ptime& pt) {
    if (pt.is_not_a_date_time()) {
        return std::chrono::system_clock::now();
    }

    static const boost::posix_time::ptime epoch(
        boost::gregorian::date(1970, 1, 1));
    const auto duration = pt - epoch;
    const auto microseconds = duration.total_microseconds();
    return std::chrono::system_clock::time_point(
        std::chrono::microseconds(microseconds));
}

}

telemetry_sink_backend::telemetry_sink_backend(
    std::shared_ptr<domain::resource> res,
    log_record_handler handler)
    : resource_(std::move(res))
    , handler_(std::move(handler)) {}

void telemetry_sink_backend::consume(const boost::log::record_view& rec) {
    domain::log_record telemetry_rec;

    // Extract timestamp using boost::log::extract
    auto timestamp_val = boost::log::extract<boost::posix_time::ptime>(
        "TimeStamp", rec);
    if (timestamp_val) {
        telemetry_rec.timestamp = to_system_clock(timestamp_val.get());
    } else {
        telemetry_rec.timestamp = std::chrono::system_clock::now();
    }

    // Extract and convert severity
    auto severity_val = boost::log::extract<logging::boost_severity>("Severity", rec);
    if (severity_val) {
        telemetry_rec.severity = logging::to_domain_severity(severity_val.get());
    } else {
        telemetry_rec.severity = logging::severity_level::info;
    }

    // Extract message body
    auto message_val = boost::log::extract<std::string>("Message", rec);
    if (message_val) {
        telemetry_rec.body = message_val.get();
    }

    // Extract channel/logger name
    auto channel_val = boost::log::extract<std::string>("Channel", rec);
    if (channel_val) {
        telemetry_rec.logger_name = channel_val.get();
    }

    // Extract trace context if present (for log correlation)
    auto trace_id_val = boost::log::extract<std::string>("trace_id", rec);
    if (trace_id_val) {
        try {
            telemetry_rec.trace = domain::trace_id::from_hex(trace_id_val.get());
        } catch (...) {
            // Invalid trace_id format - leave as nullopt
        }
    }

    auto span_id_val = boost::log::extract<std::string>("span_id", rec);
    if (span_id_val) {
        try {
            telemetry_rec.span = domain::span_id::from_hex(span_id_val.get());
        } catch (...) {
            // Invalid span_id format - leave as nullopt
        }
    }

    // Attach resource
    telemetry_rec.source_resource = resource_;

    // Forward to handler
    if (handler_) {
        handler_(std::move(telemetry_rec));
    }
}

}
