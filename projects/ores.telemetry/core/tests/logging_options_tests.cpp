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
#include "ores.logging/logging_options.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[logging_options]");

using namespace ores::logging;

}

TEST_CASE("logging_options_stream_insertion_produces_valid_json", tags) {
    logging_options opts;
    opts.severity = "debug";
    opts.filename = "test.log";
    opts.output_to_console = true;
    opts.output_directory = "/tmp/logs";
    opts.include_pid = false;
    opts.tag = "";

    std::ostringstream ss;
    ss << opts;
    const std::string json = ss.str();

    REQUIRE_FALSE(json.empty());
    REQUIRE(json.find("debug") != std::string::npos);
    REQUIRE(json.find("test.log") != std::string::npos);
    REQUIRE(json.find("/tmp/logs") != std::string::npos);
}

TEST_CASE("logging_options_stream_insertion_includes_all_fields", tags) {
    logging_options opts;
    opts.severity = "info";
    opts.filename = "app.log";
    opts.output_to_console = false;
    opts.output_directory = "/var/log";
    opts.include_pid = true;
    opts.tag = "my_tag";

    std::ostringstream ss;
    ss << opts;
    const std::string json = ss.str();

    REQUIRE(json.find("severity") != std::string::npos);
    REQUIRE(json.find("filename") != std::string::npos);
    REQUIRE(json.find("output_to_console") != std::string::npos);
    REQUIRE(json.find("output_directory") != std::string::npos);
    REQUIRE(json.find("include_pid") != std::string::npos);
    REQUIRE(json.find("tag") != std::string::npos);
    REQUIRE(json.find("my_tag") != std::string::npos);
}
