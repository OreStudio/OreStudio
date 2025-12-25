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

#include <boost/log/expressions.hpp>
#include <boost/log/attributes/value_extraction.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include "ores.telemetry/log/boost_severity.hpp"
#include "ores.telemetry/domain/trace_id.hpp"
#include "ores.telemetry/domain/span_id.hpp"

namespace ores::telemetry::log {

namespace {

namespace expr = boost::log::expressions;

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

    // Extract timestamp
    auto timestamp_attr = rec[expr::attr<boost::posix_time::ptime>("TimeStamp")];
    if (timestamp_attr) {
        telemetry_rec.timestamp = to_system_clock(timestamp_attr.get());
    } else {
        telemetry_rec.timestamp = std::chrono::system_clock::now();
    }

    // Extract and convert severity
    auto severity_attr = rec[expr::attr<boost_severity>("Severity")];
    if (severity_attr) {
        telemetry_rec.severity = to_domain_severity(severity_attr.get());
    } else {
        telemetry_rec.severity = domain::severity_level::info;
    }

    // Extract message body
    auto message_attr = rec[expr::smessage];
    if (message_attr) {
        telemetry_rec.body = message_attr.get();
    }

    // Extract channel/logger name
    auto channel_attr = rec[expr::attr<std::string_view>("Channel")];
    if (channel_attr) {
        telemetry_rec.logger_name = std::string(channel_attr.get());
    }

    // Extract trace context if present (for log correlation)
    auto trace_id_attr = rec[expr::attr<std::string>("trace_id")];
    if (trace_id_attr) {
        try {
            telemetry_rec.trace = domain::trace_id::from_hex(trace_id_attr.get());
        } catch (...) {
            // Invalid trace_id format - leave as nullopt
        }
    }

    auto span_id_attr = rec[expr::attr<std::string>("span_id")];
    if (span_id_attr) {
        try {
            telemetry_rec.span = domain::span_id::from_hex(span_id_attr.get());
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
