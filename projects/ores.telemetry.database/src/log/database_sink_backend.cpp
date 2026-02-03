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
#include "ores.telemetry.database/log/database_sink_backend.hpp"

#include <boost/log/attributes/value_extraction.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.logging/boost_severity.hpp"
#include "ores.telemetry/domain/trace_id.hpp"
#include "ores.telemetry/domain/span_id.hpp"

namespace ores::telemetry::database::log {

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

/**
 * @brief Converts boost severity to string representation.
 */
std::string severity_to_string(ores::logging::boost_severity sev) {
    switch(sev) {
        case ores::logging::boost_severity::trace: return "trace";
        case ores::logging::boost_severity::debug: return "debug";
        case ores::logging::boost_severity::info: return "info";
        case ores::logging::boost_severity::warn: return "warn";
        case ores::logging::boost_severity::error: return "error";
        default: return "info";
    }
}

} // anonymous namespace

database_sink_backend::database_sink_backend(
    std::shared_ptr<domain::resource> resource,
    database_log_handler handler,
    const std::string& source_type,
    const std::string& source_name)
    : resource_(std::move(resource))
    , handler_(std::move(handler))
    , source_type_(source_type)
    , source_name_(source_name) {
    // Initialize with null UUIDs
    // These will be settable via the setter methods if needed
}

void database_sink_backend::consume(const boost::log::record_view& rec) {
    // Create a telemetry log entry from the Boost.Log record
    domain::telemetry_log_entry entry;
    
    // Extract timestamp
    auto timestamp_val = boost::log::extract<boost::posix_time::ptime>(
        "TimeStamp", rec);
    if (timestamp_val) {
        entry.timestamp = to_system_clock(timestamp_val.get());
    } else {
        entry.timestamp = std::chrono::system_clock::now();
    }

    // Extract and convert severity
    auto severity_val = boost::log::extract<ores::logging::boost_severity>("Severity", rec);
    if (severity_val) {
        entry.level = severity_to_string(severity_val.get());
    } else {
        entry.level = "info";
    }

    // Extract message body
    auto message_val = boost::log::extract<std::string>("Message", rec);
    if (message_val) {
        entry.message = message_val.get();
    } else {
        entry.message = "(no message)";
    }

    // Extract channel/logger name
    auto channel_val = boost::log::extract<std::string>("Channel", rec);
    if (channel_val) {
        entry.component = channel_val.get();
    } else {
        entry.component = "unknown";
    }

    // Set source information
    entry.source = source_type_;
    entry.source_name = source_name_;

    // Extract trace context if present (for log correlation)
    auto trace_id_val = boost::log::extract<std::string>("trace_id", rec);
    if (trace_id_val) {
        try {
            // Convert hex string to trace_id and then to string for the database
            auto trace_id = domain::trace_id::from_hex(trace_id_val.get());
            entry.tag = trace_id.to_hex(); // Using tag field to store trace_id for now
        } catch (...) {
            // Invalid trace_id format - skip
        }
    }

    // Set session and account IDs if available
    if (has_session_id_) {
        entry.session_id = boost::lexical_cast<std::string>(session_id_);
    } else {
        entry.session_id = ""; // Empty string for no session
    }

    if (has_account_id_) {
        entry.account_id = boost::lexical_cast<std::string>(account_id_);
    } else {
        entry.account_id = ""; // Empty string for no account
    }

    // Call the handler to store the log entry (could be to database, file, etc.)
    if (handler_) {
        handler_(entry);
    }
}

void database_sink_backend::set_session_id(const boost::uuids::uuid& session_id) {
    session_id_ = session_id;
    has_session_id_ = true;
}

void database_sink_backend::set_account_id(const boost::uuids::uuid& account_id) {
    account_id_ = account_id;
    has_account_id_ = true;
}

}