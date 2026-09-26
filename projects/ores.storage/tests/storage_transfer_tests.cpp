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
#include "ores.storage/net/storage_transfer.hpp"
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
    storage_transfer sut(server.base_url());

    const std::string content = "package tarball bytes\n";
    scoped_temp_file source;
    write_file(source.path(), content);

    sut.upload("compute-packages", "oscar/1.0/oscar.tar.gz", source.path());

    BOOST_LOG_SEV(lg, info) << "Server saw target: " << server.last_target();
    CHECK(server.last_method() == "PUT");
    CHECK(server.last_target() == "/api/v1/storage/compute-packages/oscar/1.0/oscar.tar.gz");
    CHECK(server.last_put_body() == content);
}

TEST_CASE("download_writes_the_exact_server_bytes", tags) {
    auto lg(make_logger(test_suite));

    loopback_http_server server;
    storage_transfer sut(server.base_url());

    const std::string content = "downloaded object bytes\nsecond line\n";
    server.set_get_body(content);

    scoped_temp_file destination;
    sut.download("compute-packages", "oscar/1.0/oscar.tar.gz", destination.path());

    BOOST_LOG_SEV(lg, info) << "Server saw target: " << server.last_target();
    CHECK(server.last_target() == "/api/v1/storage/compute-packages/oscar/1.0/oscar.tar.gz");
    CHECK(read_file(destination.path()) == content);
}

TEST_CASE("upload_returning_response_returns_the_exact_server_body", tags) {
    auto lg(make_logger(test_suite));

    loopback_http_server server;
    storage_transfer sut(server.base_url());

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
    storage_transfer sut(server.base_url());

    scoped_temp_directory source;
    const std::string manifest_content = "name=oscar\nversion=1.2.3\n";
    const std::string payload_content = "{\"kind\":\"compute-package\"}\n";
    write_file(source.path() / "manifest.txt", manifest_content);
    write_file(source.path() / "nested" / "payload.json", payload_content);

    sut.pack_and_upload(source.path(), "compute-packages", "releases/tree.tar.gz");

    const auto uploaded = server.last_put_body();
    BOOST_LOG_SEV(lg, info) << "Uploaded archive of " << uploaded.size() << " bytes";
    CHECK(server.last_target() == "/api/v1/storage/compute-packages/releases/tree.tar.gz");
    CHECK(!uploaded.empty());

    server.set_get_body(uploaded);

    scoped_temp_directory destination;
    sut.fetch_and_unpack("compute-packages", "releases/tree.tar.gz", destination.path());

    CHECK(std::filesystem::exists(destination.path() / "manifest.txt"));
    CHECK(read_file(destination.path() / "manifest.txt") == manifest_content);
    CHECK(std::filesystem::exists(destination.path() / "nested" / "payload.json"));
    CHECK(read_file(destination.path() / "nested" / "payload.json") == payload_content);
}

TEST_CASE("upload_blob_then_download_blob_round_trips_arbitrary_bytes", tags) {
    auto lg(make_logger(test_suite));

    loopback_http_server server;
    storage_transfer sut(server.base_url());

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
