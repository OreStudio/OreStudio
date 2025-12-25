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
#ifndef ORES_TELEMETRY_LOG_MACROS_HPP
#define ORES_TELEMETRY_LOG_MACROS_HPP

#include <boost/log/sources/record_ostream.hpp>
#include <boost/log/attributes/constant.hpp>
#include "ores.telemetry/log/boost_severity.hpp"
#include "ores.telemetry/domain/telemetry_context.hpp"

namespace ores::telemetry::log {

/**
 * @brief Helper to add trace context attributes to a log record.
 *
 * This is used by the TLOG_SEV macro to inject trace_id and span_id
 * into the log record for correlation with distributed traces.
 */
struct trace_context_injector {
    const domain::telemetry_context& ctx;

    explicit trace_context_injector(const domain::telemetry_context& c)
        : ctx(c) {}
};

/**
 * @brief Stream operator to inject trace context into Boost.Log record.
 */
template<typename StreamT>
inline StreamT& operator<<(StreamT& strm, const trace_context_injector& injector) {
    strm << boost::log::add_value("trace_id",
        injector.ctx.get_trace_id().to_hex());
    strm << boost::log::add_value("span_id",
        injector.ctx.get_span_id().to_hex());
    return strm;
}

}

/**
 * @brief Log with telemetry context for trace correlation.
 *
 * This macro logs a message with the given severity level and automatically
 * injects the trace_id and span_id from the provided telemetry context.
 * All sinks (console, file, telemetry) will receive these attributes.
 *
 * Usage:
 * @code
 * TLOG_SEV(lg(), ctx, info) << "Processing request for user " << user_id;
 * @endcode
 *
 * @param logger The Boost.Log logger instance.
 * @param ctx The telemetry_context containing trace/span information.
 * @param sev The severity level (trace, debug, info, warn, error).
 */
#define TLOG_SEV(logger, ctx, sev) \
    BOOST_LOG_SEV(logger, ::ores::telemetry::log::boost_severity::sev) \
        << ::ores::telemetry::log::trace_context_injector(ctx)

/**
 * @brief Log without telemetry context (backward compatible).
 *
 * This macro provides the same interface as BOOST_LOG_SEV but uses the
 * telemetry severity type. Use this when no telemetry context is available.
 *
 * Usage:
 * @code
 * ORES_LOG_SEV(lg(), info) << "Application starting";
 * @endcode
 */
#define ORES_LOG_SEV(logger, sev) \
    BOOST_LOG_SEV(logger, ::ores::telemetry::log::boost_severity::sev)

#endif
