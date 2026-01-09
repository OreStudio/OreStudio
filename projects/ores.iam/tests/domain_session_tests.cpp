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
#include "ores.iam/domain/session.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/asio/ip/address.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.iam/domain/session_json_io.hpp" // IWYU pragma: keep.

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[domain]");

}

using ores::iam::domain::session;
using ores::iam::domain::session_statistics;
using namespace ores::logging;

TEST_CASE("create_session_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    session sut;
    sut.id = boost::uuids::random_generator()();
    sut.account_id = boost::uuids::random_generator()();
    sut.start_time = std::chrono::system_clock::now();
    sut.client_ip = boost::asio::ip::make_address("192.168.1.100");
    sut.client_identifier = "OreStudio Desktop";
    sut.client_version_major = 1;
    sut.client_version_minor = 0;
    sut.bytes_sent = 1024;
    sut.bytes_received = 2048;
    sut.country_code = "US";

    BOOST_LOG_SEV(lg, info) << "Session: " << sut;

    CHECK(sut.client_identifier == "OreStudio Desktop");
    CHECK(sut.client_version_major == 1);
    CHECK(sut.client_version_minor == 0);
    CHECK(sut.bytes_sent == 1024);
    CHECK(sut.bytes_received == 2048);
    CHECK(sut.country_code == "US");
}

TEST_CASE("session_is_active_when_no_end_time", tags) {
    auto lg(make_logger(test_suite));

    session sut;
    sut.id = boost::uuids::random_generator()();
    sut.account_id = boost::uuids::random_generator()();
    sut.start_time = std::chrono::system_clock::now();
    sut.client_ip = boost::asio::ip::make_address("10.0.0.1");
    sut.client_identifier = "Test Client";

    BOOST_LOG_SEV(lg, info) << "Active session: " << sut;

    CHECK(sut.is_active() == true);
    CHECK(!sut.end_time.has_value());
    CHECK(!sut.duration().has_value());
}

TEST_CASE("session_is_inactive_when_end_time_set", tags) {
    auto lg(make_logger(test_suite));

    session sut;
    sut.id = boost::uuids::random_generator()();
    sut.account_id = boost::uuids::random_generator()();
    sut.start_time = std::chrono::system_clock::now() - std::chrono::hours(1);
    sut.end_time = std::chrono::system_clock::now();
    sut.client_ip = boost::asio::ip::make_address("10.0.0.1");
    sut.client_identifier = "Test Client";

    BOOST_LOG_SEV(lg, info) << "Ended session: " << sut;

    CHECK(sut.is_active() == false);
    CHECK(sut.end_time.has_value());
    CHECK(sut.duration().has_value());
}

TEST_CASE("session_duration_calculation", tags) {
    auto lg(make_logger(test_suite));

    session sut;
    sut.id = boost::uuids::random_generator()();
    sut.account_id = boost::uuids::random_generator()();
    sut.start_time = std::chrono::system_clock::now();
    sut.end_time = sut.start_time + std::chrono::seconds(3600);
    sut.client_ip = boost::asio::ip::make_address("10.0.0.1");
    sut.client_identifier = "Test Client";

    BOOST_LOG_SEV(lg, info) << "Session with duration: " << sut;

    auto duration = sut.duration();
    CHECK(duration.has_value());
    CHECK(duration.value().count() == 3600);
}

TEST_CASE("session_with_ipv6_address", tags) {
    auto lg(make_logger(test_suite));

    session sut;
    sut.id = boost::uuids::random_generator()();
    sut.account_id = boost::uuids::random_generator()();
    sut.start_time = std::chrono::system_clock::now();
    sut.client_ip = boost::asio::ip::make_address("2001:0db8:85a3:0000:0000:8a2e:0370:7334");
    sut.client_identifier = "IPv6 Client";

    BOOST_LOG_SEV(lg, info) << "IPv6 session: " << sut;

    CHECK(sut.client_ip.is_v6());
    CHECK(sut.client_identifier == "IPv6 Client");
}

TEST_CASE("session_serialization_to_json", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::string_generator uuid_gen;
    const auto session_uuid = uuid_gen("123e4567-e89b-12d3-a456-426614174000");
    const auto account_uuid = uuid_gen("987fcdeb-51a2-12d3-a456-426614174999");

    // Use a fixed time point for deterministic output
    std::tm tm = {};
    tm.tm_year = 2025 - 1900;
    tm.tm_mon = 1 - 1;
    tm.tm_mday = 15;
    tm.tm_hour = 10;
    tm.tm_min = 30;
    tm.tm_sec = 0;
    const auto fixed_time = std::chrono::system_clock::from_time_t(std::mktime(&tm));

    session sut;
    sut.id = session_uuid;
    sut.account_id = account_uuid;
    sut.start_time = fixed_time;
    sut.client_ip = boost::asio::ip::make_address("198.51.100.42");
    sut.client_identifier = "Desktop App";
    sut.client_version_major = 2;
    sut.client_version_minor = 5;
    sut.country_code = "GB";

    BOOST_LOG_SEV(lg, info) << "Session: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("123e4567-e89b-12d3-a456-426614174000") != std::string::npos);
    CHECK(json_output.find("987fcdeb-51a2-12d3-a456-426614174999") != std::string::npos);
    CHECK(json_output.find("Desktop App") != std::string::npos);
    CHECK(json_output.find("GB") != std::string::npos);
}

TEST_CASE("session_without_geolocation", tags) {
    auto lg(make_logger(test_suite));

    session sut;
    sut.id = boost::uuids::random_generator()();
    sut.account_id = boost::uuids::random_generator()();
    sut.start_time = std::chrono::system_clock::now();
    sut.client_ip = boost::asio::ip::make_address("127.0.0.1");
    sut.client_identifier = "Local Client";
    // country_code remains empty for localhost/private IPs

    BOOST_LOG_SEV(lg, info) << "Session without geo: " << sut;

    CHECK(sut.country_code.empty());
}

TEST_CASE("create_session_with_faker", tags) {
    auto lg(make_logger(test_suite));

    session sut;
    sut.id = boost::uuids::random_generator()();
    sut.account_id = boost::uuids::random_generator()();
    sut.start_time = std::chrono::system_clock::now();
    sut.client_ip = boost::asio::ip::make_address(faker::internet::ipv4());
    sut.client_identifier = std::string(faker::word::noun()) + " Client";
    sut.client_version_major = static_cast<uint16_t>(faker::number::integer(1, 10));
    sut.client_version_minor = static_cast<uint16_t>(faker::number::integer(0, 99));
    sut.bytes_sent = faker::number::integer<uint64_t>(0, 1000000);
    sut.bytes_received = faker::number::integer<uint64_t>(0, 1000000);
    sut.country_code = faker::location::countryCode();

    BOOST_LOG_SEV(lg, info) << "Faker session: " << sut;

    CHECK(!sut.client_identifier.empty());
    CHECK(sut.client_version_major >= 1);
    CHECK(!sut.country_code.empty());
}

TEST_CASE("create_multiple_random_sessions", tags) {
    auto lg(make_logger(test_suite));

    for (int i = 0; i < 3; ++i) {
        session sut;
        sut.id = boost::uuids::random_generator()();
        sut.account_id = boost::uuids::random_generator()();
        sut.start_time = std::chrono::system_clock::now() -
            std::chrono::hours(faker::number::integer(0, 168));

        if (faker::datatype::boolean()) {
            sut.end_time = sut.start_time +
                std::chrono::minutes(faker::number::integer(1, 480));
        }

        sut.client_ip = boost::asio::ip::make_address(faker::internet::ipv4());
        sut.client_identifier = std::string(faker::word::noun()) + " App";
        sut.client_version_major = static_cast<uint16_t>(faker::number::integer(1, 5));
        sut.client_version_minor = static_cast<uint16_t>(faker::number::integer(0, 20));
        sut.bytes_sent = faker::number::integer<uint64_t>(100, 10000);
        sut.bytes_received = faker::number::integer<uint64_t>(100, 10000);

        BOOST_LOG_SEV(lg, info) << "Session " << i << ": " << sut;

        CHECK(!sut.client_identifier.empty());

        if (sut.end_time.has_value()) {
            CHECK(!sut.is_active());
            CHECK(sut.duration().has_value());
        } else {
            CHECK(sut.is_active());
        }
    }
}

TEST_CASE("create_session_statistics_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    session_statistics sut;
    sut.period_start = std::chrono::system_clock::now() - std::chrono::hours(24);
    sut.period_end = std::chrono::system_clock::now();
    sut.account_id = boost::uuids::random_generator()();
    sut.session_count = 10;
    sut.avg_duration_seconds = 1800.0;
    sut.total_bytes_sent = 1048576;
    sut.total_bytes_received = 2097152;
    sut.avg_bytes_sent = 104857.6;
    sut.avg_bytes_received = 209715.2;
    sut.unique_countries = 3;

    BOOST_LOG_SEV(lg, info) << "Session statistics: " << sut;

    CHECK(sut.session_count == 10);
    CHECK(sut.avg_duration_seconds == 1800.0);
    CHECK(sut.total_bytes_sent == 1048576);
    CHECK(sut.total_bytes_received == 2097152);
    CHECK(sut.unique_countries == 3);
}

TEST_CASE("session_statistics_serialization_to_json", tags) {
    auto lg(make_logger(test_suite));

    session_statistics sut;
    sut.period_start = std::chrono::system_clock::now() - std::chrono::hours(24);
    sut.period_end = std::chrono::system_clock::now();
    sut.session_count = 100;
    sut.avg_duration_seconds = 3600.0;
    sut.total_bytes_sent = 10000000;
    sut.total_bytes_received = 20000000;
    sut.avg_bytes_sent = 100000.0;
    sut.avg_bytes_received = 200000.0;
    sut.unique_countries = 10;

    BOOST_LOG_SEV(lg, info) << "Session statistics: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("100") != std::string::npos);
    CHECK(json_output.find("10000000") != std::string::npos);
}

TEST_CASE("create_session_statistics_with_faker", tags) {
    auto lg(make_logger(test_suite));

    session_statistics sut;
    sut.period_start = std::chrono::system_clock::now() -
        std::chrono::hours(faker::number::integer(1, 720));
    sut.period_end = std::chrono::system_clock::now();
    sut.account_id = boost::uuids::random_generator()();
    sut.session_count = faker::number::integer<uint64_t>(1, 1000);
    sut.avg_duration_seconds = faker::number::decimal(60.0, 7200.0);
    sut.total_bytes_sent = faker::number::integer<uint64_t>(1000, 100000000);
    sut.total_bytes_received = faker::number::integer<uint64_t>(1000, 100000000);
    sut.avg_bytes_sent = static_cast<double>(sut.total_bytes_sent) /
        static_cast<double>(sut.session_count);
    sut.avg_bytes_received = static_cast<double>(sut.total_bytes_received) /
        static_cast<double>(sut.session_count);
    sut.unique_countries = faker::number::integer<uint32_t>(1, 50);

    BOOST_LOG_SEV(lg, info) << "Faker session statistics: " << sut;

    CHECK(sut.session_count >= 1);
    CHECK(sut.avg_duration_seconds >= 60.0);
    CHECK(sut.unique_countries >= 1);
}
