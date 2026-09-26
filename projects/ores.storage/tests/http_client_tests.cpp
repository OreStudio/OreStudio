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
#include "loopback_http_server.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/scoped_temp_directory.hpp"
#include "ores.platform/filesystem/scoped_temp_file.hpp"
#include "ores.storage/net/http_client.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <stdexcept>
#include <string>
#include <string_view>

using ores::platform::filesystem::scoped_temp_directory;
using ores::platform::filesystem::scoped_temp_file;
using ores::storage::net::http_client;
using ores::storage::tests::loopback_http_server;
using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.storage.tests");
const std::string tags("[net]");

void write_file(const std::filesystem::path& path, const std::string& content) {
    std::ofstream out(path, std::ios::binary);
    out.write(content.data(), static_cast<std::streamsize>(content.size()));
}

std::string read_file(const std::filesystem::path& path) {
    std::ifstream in(path, std::ios::binary);
    return std::string((std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());
}

}

TEST_CASE("get_writes_the_exact_server_body_to_the_destination", tags) {
    auto lg(make_logger(test_suite));

    loopback_http_server server;
    const std::string body = "first line\nsecond line\nbinary-ish \x01\x02 tail\n";
    server.set_get_body(body);

    scoped_temp_file destination;
    http_client::get(server.base_url() + "/objects/download.bin", destination.path());

    BOOST_LOG_SEV(lg, info) << "Downloaded to: " << destination.path();
    CHECK(read_file(destination.path()) == body);
    CHECK(server.last_method() == "GET");
    CHECK(server.last_target() == "/objects/download.bin");
}

TEST_CASE("put_delivers_the_exact_source_file_bytes", tags) {
    auto lg(make_logger(test_suite));

    loopback_http_server server;
    const std::string content = "uploaded payload\nwith two lines\n";

    scoped_temp_file source;
    write_file(source.path(), content);

    http_client::put(server.base_url() + "/objects/upload.bin", source.path());

    BOOST_LOG_SEV(lg, info) << "Server recorded PUT body of " << server.last_put_body().size()
                            << " bytes";
    CHECK(server.last_method() == "PUT");
    CHECK(server.last_target() == "/objects/upload.bin");
    CHECK(server.last_put_body() == content);
}

TEST_CASE("put_returning_body_returns_the_exact_configured_response", tags) {
    auto lg(make_logger(test_suite));

    loopback_http_server server;
    const std::string response = "{\"checksum\":\"deadbeef\",\"size\":21}";
    server.set_put_response_body(response);

    scoped_temp_file source;
    write_file(source.path(), "payload");

    const auto actual =
        http_client::put_returning_body(server.base_url() + "/objects/upload.bin", source.path());

    BOOST_LOG_SEV(lg, info) << "Response body: " << actual;
    CHECK(actual == response);
}

TEST_CASE("get_of_a_not_found_resource_throws_and_creates_no_file", tags) {
    auto lg(make_logger(test_suite));

    loopback_http_server server;
    server.set_status(404);
    server.set_get_body("no such object");

    scoped_temp_directory parent;
    const auto destination = parent.path() / "absent.bin";

    BOOST_LOG_SEV(lg, info) << "Expecting 404 for: " << destination;
    CHECK_THROWS_AS(http_client::get(server.base_url() + "/objects/missing", destination),
                    std::runtime_error);
    CHECK(!std::filesystem::exists(destination));
}

TEST_CASE("get_of_a_not_found_resource_leaves_an_existing_file_untouched", tags) {
    auto lg(make_logger(test_suite));

    loopback_http_server server;
    server.set_status(404);

    const std::string original_content = "existing destination content";
    scoped_temp_file destination;
    write_file(destination.path(), original_content);

    BOOST_LOG_SEV(lg, info) << "Expecting 404 to leave " << destination.path() << " untouched";
    CHECK_THROWS_AS(http_client::get(server.base_url() + "/objects/missing", destination.path()),
                    std::runtime_error);
    CHECK(read_file(destination.path()) == original_content);
}
