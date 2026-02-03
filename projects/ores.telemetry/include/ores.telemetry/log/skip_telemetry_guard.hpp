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
#ifndef ORES_TELEMETRY_LOG_SKIP_TELEMETRY_GUARD_HPP
#define ORES_TELEMETRY_LOG_SKIP_TELEMETRY_GUARD_HPP

#include <boost/log/attributes/value_extraction.hpp>
#include "ores.logging/scoped_attribute.hpp"

namespace ores::telemetry::log {

/**
 * @brief Well-known attribute name used to signal telemetry sinks to skip
 * processing a log record.
 *
 * When this attribute is present and set to true on a log record, telemetry
 * sinks (database sink, OTLP sink, etc.) should skip processing the record to
 * prevent recursive logging loops.
 */
inline constexpr const char* skip_telemetry_attribute = "SkipTelemetry";

/**
 * @brief RAII guard that marks log records to be skipped by telemetry sinks.
 *
 * Use this guard in any code that is part of the telemetry pipeline to prevent
 * recursive logging. When logs are created while this guard is active, they
 * will still be processed by console and file sinks, but telemetry sinks will
 * skip them.
 *
 * The guard affects all log records created on the same thread, including
 * records from nested function calls.
 *
 * Example usage:
 * @code
 * void telemetry_repository::create(const telemetry_log_entry& entry) {
 *     skip_telemetry_guard guard;
 *     BOOST_LOG_SEV(lg(), trace) << "Creating entry...";  // Skipped by telemetry sinks
 *     // ... database operations that may log ...
 * }
 * @endcode
 */
class skip_telemetry_guard : public ores::logging::scoped_attribute {
public:
    skip_telemetry_guard()
        : scoped_attribute(skip_telemetry_attribute) {}
};

/**
 * @brief Checks if a log record should be skipped by telemetry sinks.
 *
 * @param rec The log record to check.
 * @return true if the record has the SkipTelemetry attribute set to true.
 */
template<typename RecordView>
bool should_skip_telemetry(const RecordView& rec) {
    auto skip_attr = boost::log::extract<bool>(skip_telemetry_attribute, rec);
    return skip_attr && skip_attr.get();
}

}

#endif
