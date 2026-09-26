/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.logging/boost_severity.hpp"
#include "ores.logging/make_logger.hpp"
#include <catch2/catch_test_macros.hpp>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>

namespace {

const std::string_view test_suite("ores.logging.tests");
const std::string tags("[logging]");

}

using ores::logging::boost_severity;
using ores::logging::severity_level;
using ores::logging::to_boost_severity;
using ores::logging::to_domain_severity;

TEST_CASE("to_boost_severity_maps_every_lower_case_name", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const boost_severity trace_level(to_boost_severity("trace"));
    const boost_severity debug_level(to_boost_severity("debug"));
    const boost_severity info_level(to_boost_severity("info"));
    const boost_severity warn_level(to_boost_severity("warn"));
    const boost_severity error_level(to_boost_severity("error"));

    BOOST_LOG_SEV(lg, ores::logging::info)
        << "trace: " << trace_level << " debug: " << debug_level << " info: " << info_level
        << " warn: " << warn_level << " error: " << error_level;

    CHECK(trace_level == boost_severity::trace);
    CHECK(debug_level == boost_severity::debug);
    CHECK(info_level == boost_severity::info);
    CHECK(warn_level == boost_severity::warn);
    CHECK(error_level == boost_severity::error);
}

TEST_CASE("to_boost_severity_rejects_fatal", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    BOOST_LOG_SEV(lg, ores::logging::info) << "converting the literal fatal";

    CHECK_THROWS_AS(to_boost_severity("fatal"), std::invalid_argument);
}

TEST_CASE("to_boost_severity_rejects_mixed_case_name", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    BOOST_LOG_SEV(lg, ores::logging::info) << "converting the literal INFO";

    CHECK_THROWS_AS(to_boost_severity("INFO"), std::invalid_argument);
}

TEST_CASE("to_boost_severity_rejects_empty_name", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    BOOST_LOG_SEV(lg, ores::logging::info) << "converting the empty string";

    CHECK_THROWS_AS(to_boost_severity(""), std::invalid_argument);
}

TEST_CASE("to_boost_severity_maps_every_domain_level_to_itself", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    BOOST_LOG_SEV(lg, ores::logging::info) << "converting domain levels";

    CHECK(to_boost_severity(severity_level::trace) == boost_severity::trace);
    CHECK(to_boost_severity(severity_level::debug) == boost_severity::debug);
    CHECK(to_boost_severity(severity_level::info) == boost_severity::info);
    CHECK(to_boost_severity(severity_level::warn) == boost_severity::warn);
    CHECK(to_boost_severity(severity_level::error) == boost_severity::error);
}

TEST_CASE("to_boost_severity_maps_fatal_to_error", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    BOOST_LOG_SEV(lg, ores::logging::info) << "converting severity_level::fatal";

    CHECK(to_boost_severity(severity_level::fatal) == boost_severity::error);
}

TEST_CASE("to_domain_severity_round_trips_every_boost_level", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    BOOST_LOG_SEV(lg, ores::logging::info) << "round-tripping boost severities";

    CHECK(to_domain_severity(boost_severity::trace) == severity_level::trace);
    CHECK(to_domain_severity(boost_severity::debug) == severity_level::debug);
    CHECK(to_domain_severity(boost_severity::info) == severity_level::info);
    CHECK(to_domain_severity(boost_severity::warn) == severity_level::warn);
    CHECK(to_domain_severity(boost_severity::error) == severity_level::error);
}

TEST_CASE("stream_operator_renders_upper_case_level_names", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    std::ostringstream trace_stream;
    trace_stream << boost_severity::trace;
    std::ostringstream debug_stream;
    debug_stream << boost_severity::debug;
    std::ostringstream info_stream;
    info_stream << boost_severity::info;
    std::ostringstream warn_stream;
    warn_stream << boost_severity::warn;
    std::ostringstream error_stream;
    error_stream << boost_severity::error;

    BOOST_LOG_SEV(lg, ores::logging::info)
        << "trace: " << trace_stream.str() << " error: " << error_stream.str();

    CHECK(trace_stream.str() == "TRACE");
    CHECK(debug_stream.str() == "DEBUG");
    CHECK(info_stream.str() == "INFO");
    CHECK(warn_stream.str() == "WARN");
    CHECK(error_stream.str() == "ERROR");
}
