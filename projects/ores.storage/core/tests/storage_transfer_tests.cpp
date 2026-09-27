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
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/scoped_temp_directory.hpp"
#include "ores.platform/filesystem/scoped_temp_file.hpp"
#include "ores.storage.core/net/storage_transfer.hpp"
#include "support/loopback_http_server.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <span>
#include <string>
#include <string_view>
#include <vector>

using ores::platform::filesystem::scoped_temp_directory;
using ores::platform::filesystem::scoped_temp_file;
using ores::storage::net::storage_transfer;
using ores::storage::tests::loopback_http_server;
using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.storage.tests");
const std::string tags("[net]");

/// The caller token every transfer carries.
const std::string test_token("ores-storage-test-token");

void write_file(const std::filesystem::path& path, const std::string& content) {
    std::filesystem::create_directories(path.parent_path());
    std::ofstream out(path, std::ios::binary);
    out.write(content.data(), static_cast<std::streamsize>(content.size()));
}

std::string read_file(const std::filesystem::path& path) {
    std::ifstream in(path, std::ios::binary);
    return std::string((std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());
}

}

TEST_CASE("upload_puts_the_file_bytes_to_the_object_path", tags) {
    auto lg(make_logger(test_suite));

    loopback_http_server server;
    storage_transfer sut(server.base_url(), test_token);

    const std::string content = "package tarball bytes\n";
    scoped_temp_file source;
    write_file(source.path(), content);

    sut.upload("ores", "compute/packages/oscar-1.0/oscar.tar.gz", source.path());

    BOOST_LOG_SEV(lg, info) << "Server saw target: " << server.last_target();
    CHECK(server.last_method() == "PUT");
    CHECK(server.last_target() == "/api/v1/storage/ores/compute/packages/oscar-1.0/oscar.tar.gz");
    CHECK(server.last_put_body() == content);
    CHECK(server.last_authorization() == "Bearer " + test_token);
}

TEST_CASE("download_writes_the_exact_server_bytes", tags) {
    auto lg(make_logger(test_suite));

    loopback_http_server server;
    storage_transfer sut(server.base_url(), test_token);

    const std::string content = "downloaded object bytes\nsecond line\n";
    server.set_get_body(content);

    scoped_temp_file destination;
    sut.download("ores", "compute/packages/oscar-1.0/oscar.tar.gz", destination.path());

    BOOST_LOG_SEV(lg, info) << "Server saw target: " << server.last_target();
    CHECK(server.last_target() == "/api/v1/storage/ores/compute/packages/oscar-1.0/oscar.tar.gz");
    CHECK(read_file(destination.path()) == content);
}

TEST_CASE("upload_returning_response_returns_the_exact_server_body", tags) {
    auto lg(make_logger(test_suite));

    loopback_http_server server;
    storage_transfer sut(server.base_url(), test_token);

    const std::string response = "{\"checksum\":\"cafebabe\"}";
    server.set_put_response_body(response);

    scoped_temp_file source;
    write_file(source.path(), "payload");

    const auto actual = sut.upload_returning_response("bucket", "key", source.path());

    BOOST_LOG_SEV(lg, info) << "Response body: " << actual;
    CHECK(actual == response);
}

TEST_CASE("pack_and_upload_then_fetch_and_unpack_reproduces_the_tree", tags) {
    auto lg(make_logger(test_suite));

    loopback_http_server server;
    storage_transfer sut(server.base_url(), test_token);

    scoped_temp_directory source;
    const std::string manifest_content = "name=oscar\nversion=1.2.3\n";
    const std::string payload_content = "{\"kind\":\"compute-package\"}\n";
    write_file(source.path() / "manifest.txt", manifest_content);
    write_file(source.path() / "nested" / "payload.json", payload_content);

    sut.pack_and_upload(source.path(), "ores", "compute/packages/tree.tar.gz");

    const auto uploaded = server.last_put_body();
    BOOST_LOG_SEV(lg, info) << "Uploaded archive of " << uploaded.size() << " bytes";
    CHECK(server.last_target() == "/api/v1/storage/ores/compute/packages/tree.tar.gz");
    CHECK(!uploaded.empty());

    server.set_get_body(uploaded);

    scoped_temp_directory destination;
    sut.fetch_and_unpack("ores", "compute/packages/tree.tar.gz", destination.path());

    CHECK(std::filesystem::exists(destination.path() / "manifest.txt"));
    CHECK(read_file(destination.path() / "manifest.txt") == manifest_content);
    CHECK(std::filesystem::exists(destination.path() / "nested" / "payload.json"));
    CHECK(read_file(destination.path() / "nested" / "payload.json") == payload_content);
}

TEST_CASE("upload_blob_then_download_blob_round_trips_arbitrary_bytes", tags) {
    auto lg(make_logger(test_suite));

    loopback_http_server server;
    storage_transfer sut(server.base_url(), test_token);

    std::vector<char> original = {'b', 'l', 'o', 'b', '\0', 'p', 'a', 'y', 'l', 'o', 'a', 'd'};
    for (int i = 0; i < 256; ++i)
        original.push_back(static_cast<char>(i));

    sut.upload_blob(
        "blobs", "binary/blob.bin", std::span<const char>(original.data(), original.size()));

    const auto compressed = server.last_put_body();
    BOOST_LOG_SEV(lg, info) << "Uploaded compressed blob of " << compressed.size() << " bytes";
    CHECK(!compressed.empty());

    server.set_get_body(compressed);

    const auto round_tripped = sut.download_blob("blobs", "binary/blob.bin");

    BOOST_LOG_SEV(lg, info) << "Round-tripped " << round_tripped.size() << " bytes";
    CHECK(round_tripped == original);
}

TEST_CASE("remove_targets_the_object_url_and_returns_the_server_answer", tags) {
    auto lg(make_logger(test_suite));

    loopback_http_server server;
    storage_transfer sut(server.base_url(), test_token);

    const std::string answer = R"({"success":true,"removed":true})";
    server.set_get_body(answer);

    const auto actual = sut.remove("ores", "ore/imports/abc.tar.gz");

    BOOST_LOG_SEV(lg, info) << "Server saw " << server.last_method() << " " << server.last_target();
    CHECK(server.last_method() == "DELETE");
    CHECK(server.last_target() == "/api/v1/storage/ores/ore/imports/abc.tar.gz");
    CHECK(server.last_authorization() == "Bearer " + test_token);
    CHECK(actual == answer);
}

TEST_CASE("list_asks_the_bucket_url_for_the_requested_page", tags) {
    auto lg(make_logger(test_suite));

    loopback_http_server server;
    storage_transfer sut(server.base_url(), test_token);

    const std::string listing =
        R"({"success":true,"total_available_count":2,)"
        R"("objects":[{"key":"ore/imports/a.tar.gz","size_bytes":12}]})";
    server.set_get_body(listing);

    const auto actual = sut.list("ores", "ore/imports/", 5, 25);

    BOOST_LOG_SEV(lg, info) << "Server saw " << server.last_target();
    CHECK(server.last_method() == "GET");
    CHECK(server.last_target() ==
          "/api/v1/storage/ores?prefix=ore/imports/&offset=5&limit=25");
    CHECK(actual == listing);
}

TEST_CASE("list_escapes_a_prefix_that_would_otherwise_change_the_query", tags) {
    auto lg(make_logger(test_suite));

    loopback_http_server server;
    storage_transfer sut(server.base_url(), test_token);

    sut.list("ores", "a b&c=d", 0, 100);

    BOOST_LOG_SEV(lg, info) << "Server saw " << server.last_target();
    CHECK(server.last_target() == "/api/v1/storage/ores?prefix=a%20b%26c%3Dd&offset=0&limit=100");
}
