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
#include "ores.logging/logging_options.hpp"
#include "ores.logging/make_logger.hpp"
#include <catch2/catch_test_macros.hpp>
#include <sstream>
#include <string>
#include <string_view>

namespace {

const std::string_view test_suite("ores.logging.tests");
const std::string tags("[logging]");

ores::logging::logging_options make_populated_options() {
    ores::logging::logging_options r;
    r.severity = "info";
    r.filename = "app.log";
    r.output_to_console = true;
    r.output_directory = "log";
    r.include_pid = true;
    r.replica_index = 3;
    r.tag = "TestSuite";
    return r;
}

std::string stream_to_string(const ores::logging::logging_options& v) {
    std::ostringstream os;
    os << v;
    return os.str();
}

}

TEST_CASE("logging_options_stream_renders_every_field_name", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const std::string json(stream_to_string(make_populated_options()));
    BOOST_LOG_SEV(lg, ores::logging::info) << json;

    CHECK(json.contains("\"severity\""));
    CHECK(json.contains("\"filename\""));
    CHECK(json.contains("\"output_to_console\""));
    CHECK(json.contains("\"output_directory\""));
    CHECK(json.contains("\"include_pid\""));
    CHECK(json.contains("\"replica_index\""));
    CHECK(json.contains("\"tag\""));
}

TEST_CASE("logging_options_stream_renders_populated_field_values", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const std::string json(stream_to_string(make_populated_options()));
    BOOST_LOG_SEV(lg, ores::logging::info) << json;

    CHECK(json.contains("\"severity\":\"info\""));
    CHECK(json.contains("\"filename\":\"app.log\""));
    CHECK(json.contains("\"output_to_console\":true"));
    CHECK(json.contains("\"output_directory\":\"log\""));
    CHECK(json.contains("\"include_pid\":true"));
    CHECK(json.contains("\"replica_index\":3"));
    CHECK(json.contains("\"tag\":\"TestSuite\""));
}
