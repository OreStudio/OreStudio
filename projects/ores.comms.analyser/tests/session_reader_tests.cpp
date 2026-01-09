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
#include "ores.comms.analyser/domain/session_reader.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.telemetry/log/make_logger.hpp"

namespace {

const std::string test_suite("ores.comms.analyser.tests");
const std::string tags("[domain]");

}

using namespace ores::comms::analyser::domain;
using namespace ores::telemetry::log;

TEST_CASE("session_reader_nonexistent_file_returns_error", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing session_reader with nonexistent file";

    auto result = session_reader::read("/nonexistent/path/session.ores");

    REQUIRE(!result.has_value());
    REQUIRE(result.error() == ores::comms::recording::session_file_error::file_open_failed);
}

TEST_CASE("session_reader_read_metadata_nonexistent_file", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing session_reader read_metadata with nonexistent file";

    auto result = session_reader::read_metadata("/nonexistent/path/session.ores");

    REQUIRE(!result.has_value());
    REQUIRE(result.error() == ores::comms::recording::session_file_error::file_open_failed);
}

TEST_CASE("session_reader_empty_path", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing session_reader with empty path";

    auto result = session_reader::read("");

    REQUIRE(!result.has_value());
}

TEST_CASE("session_reader_read_metadata_empty_path", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing session_reader read_metadata with empty path";

    auto result = session_reader::read_metadata("");

    REQUIRE(!result.has_value());
}

TEST_CASE("session_metadata_default_construction", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing session_metadata default construction";

    session_metadata sut;

    CHECK(sut.server_address.empty());
    CHECK(sut.protocol_version_major == 0);
    CHECK(sut.protocol_version_minor == 0);
}

TEST_CASE("session_metadata_with_values", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing session_metadata with values";

    session_metadata sut;
    sut.server_address = "192.168.1.100:8080";
    sut.protocol_version_major = 1;
    sut.protocol_version_minor = 2;
    sut.start_time = std::chrono::system_clock::now();

    CHECK(sut.server_address == "192.168.1.100:8080");
    CHECK(sut.protocol_version_major == 1);
    CHECK(sut.protocol_version_minor == 2);
}

TEST_CASE("recorded_frame_default_construction", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing recorded_frame default construction";

    recorded_frame sut;

    CHECK(sut.timestamp_offset_us == 0);
}

TEST_CASE("recorded_frame_with_values", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing recorded_frame with values";

    recorded_frame sut;
    sut.timestamp_offset_us = 1500000;
    sut.direction = ores::comms::recording::frame_direction::client_to_server;

    CHECK(sut.timestamp_offset_us == 1500000);
    CHECK(sut.direction == ores::comms::recording::frame_direction::client_to_server);
}

TEST_CASE("session_data_default_construction", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing session_data default construction";

    session_data sut;

    CHECK(sut.frames.empty());
}

TEST_CASE("session_data_with_frames", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing session_data with multiple frames";

    session_data sut;
    sut.metadata.server_address = "server:9000";

    recorded_frame frame1;
    frame1.timestamp_offset_us = 0;
    frame1.direction = ores::comms::recording::frame_direction::client_to_server;

    recorded_frame frame2;
    frame2.timestamp_offset_us = 100000;
    frame2.direction = ores::comms::recording::frame_direction::server_to_client;

    sut.frames.push_back(frame1);
    sut.frames.push_back(frame2);

    CHECK(sut.metadata.server_address == "server:9000");
    REQUIRE(sut.frames.size() == 2);
    CHECK(sut.frames[0].timestamp_offset_us == 0);
    CHECK(sut.frames[1].timestamp_offset_us == 100000);
}
