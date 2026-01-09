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
#include "ores.http/net/http_server_options.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string test_suite("ores.http.tests");
const std::string tags("[http_server_options]");

}

using namespace ores::http::net;
using namespace ores::logging;

TEST_CASE("http_server_options_default_construction", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing http_server_options default construction";

    http_server_options sut;

    CHECK(sut.address == "0.0.0.0");
    CHECK(sut.port == 8080);
    CHECK(sut.max_connections == 100);
    CHECK(sut.request_timeout.count() == 30);
    CHECK_FALSE(sut.enable_ssl);
    CHECK(sut.certificate_file.empty());
    CHECK(sut.private_key_file.empty());
    CHECK(sut.jwt_secret.empty());
    CHECK(sut.jwt_public_key_file.empty());
    CHECK(sut.jwt_issuer == "ores");
    CHECK(sut.jwt_audience == "ores-api");
    CHECK(sut.enable_cors);
    CHECK(sut.cors_allowed_origins == "*");
    CHECK(sut.server_identifier == "ores-http-server");
}

TEST_CASE("http_server_options_custom_values", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing http_server_options with custom values";

    http_server_options sut;
    sut.address = "127.0.0.1";
    sut.port = 3000;
    sut.max_connections = 500;
    sut.request_timeout = std::chrono::seconds{60};
    sut.enable_ssl = true;
    sut.certificate_file = "/etc/ssl/cert.pem";
    sut.private_key_file = "/etc/ssl/key.pem";
    sut.jwt_secret = "my-secret-key";
    sut.jwt_issuer = "my-app";
    sut.jwt_audience = "my-api";
    sut.enable_cors = false;
    sut.cors_allowed_origins = "https://example.com";
    sut.server_identifier = "my-server";

    CHECK(sut.address == "127.0.0.1");
    CHECK(sut.port == 3000);
    CHECK(sut.max_connections == 500);
    CHECK(sut.request_timeout.count() == 60);
    CHECK(sut.enable_ssl);
    CHECK(sut.certificate_file == "/etc/ssl/cert.pem");
    CHECK(sut.private_key_file == "/etc/ssl/key.pem");
    CHECK(sut.jwt_secret == "my-secret-key");
    CHECK(sut.jwt_issuer == "my-app");
    CHECK(sut.jwt_audience == "my-api");
    CHECK_FALSE(sut.enable_cors);
    CHECK(sut.cors_allowed_origins == "https://example.com");
    CHECK(sut.server_identifier == "my-server");
}

TEST_CASE("http_server_options_streaming", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing http_server_options streaming operator";

    http_server_options sut;
    sut.address = "192.168.1.1";
    sut.port = 9000;
    sut.enable_ssl = true;

    std::ostringstream os;
    os << sut;
    const std::string output = os.str();

    BOOST_LOG_SEV(lg, debug) << "Output: " << output;

    CHECK(!output.empty());
    CHECK(output.find("192.168.1.1") != std::string::npos);
    CHECK(output.find("9000") != std::string::npos);
    CHECK(output.find("true") != std::string::npos);
}

TEST_CASE("http_server_options_streaming_contains_all_fields", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing http_server_options streaming contains all fields";

    http_server_options sut;

    std::ostringstream os;
    os << sut;
    const std::string output = os.str();

    CHECK(output.find("address") != std::string::npos);
    CHECK(output.find("port") != std::string::npos);
    CHECK(output.find("max_connections") != std::string::npos);
    CHECK(output.find("request_timeout") != std::string::npos);
    CHECK(output.find("enable_ssl") != std::string::npos);
    CHECK(output.find("jwt_issuer") != std::string::npos);
    CHECK(output.find("jwt_audience") != std::string::npos);
    CHECK(output.find("enable_cors") != std::string::npos);
    CHECK(output.find("server_identifier") != std::string::npos);
}
