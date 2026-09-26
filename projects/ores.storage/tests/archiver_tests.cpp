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
#include "ores.storage/filesystem/archiver.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <stdexcept>
#include <string>
#include <string_view>

using ores::platform::filesystem::scoped_temp_directory;
using ores::platform::filesystem::scoped_temp_file;
using ores::storage::filesystem::archiver;
using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.storage.tests");
const std::string tags("[filesystem]");

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

TEST_CASE("pack_and_extract_round_trips_a_directory_tree", tags) {
    auto lg(make_logger(test_suite));

    scoped_temp_directory source;
    const std::string manifest_content = "name=ores.storage\nversion=1\n";
    const std::string server_content = "[server]\nport=51000\n";
    const std::string readme_content = "# ores.storage\n\nArchive round trip.\n";

    write_file(source.path() / "manifest.txt", manifest_content);
    write_file(source.path() / "config" / "server.ini", server_content);
    write_file(source.path() / "docs" / "nested" / "README.md", readme_content);

    scoped_temp_file archive;
    archiver::pack(source.path(), archive.path());

    BOOST_LOG_SEV(lg, info) << "Packed archive: " << archive.path();
    CHECK(std::filesystem::exists(archive.path()));
    CHECK(std::filesystem::file_size(archive.path()) > 0);

    scoped_temp_directory destination;
    archiver::extract(archive.path(), destination.path());

    CHECK(std::filesystem::exists(destination.path() / "manifest.txt"));
    CHECK(read_file(destination.path() / "manifest.txt") == manifest_content);

    CHECK(std::filesystem::exists(destination.path() / "config" / "server.ini"));
    CHECK(read_file(destination.path() / "config" / "server.ini") == server_content);

    CHECK(std::filesystem::exists(destination.path() / "docs" / "nested" / "README.md"));
    CHECK(read_file(destination.path() / "docs" / "nested" / "README.md") == readme_content);
}

TEST_CASE("pack_of_a_missing_source_directory_throws", tags) {
    auto lg(make_logger(test_suite));

    scoped_temp_directory parent;
    scoped_temp_file archive;
    const auto missing = parent.path() / "no-such-directory";

    BOOST_LOG_SEV(lg, info) << "Packing missing directory: " << missing;
    CHECK_THROWS_AS(archiver::pack(missing, archive.path()), std::runtime_error);
}

TEST_CASE("extract_of_a_file_that_is_not_an_archive_throws", tags) {
    auto lg(make_logger(test_suite));

    scoped_temp_file not_an_archive;
    write_file(not_an_archive.path(), "this file is not a tar.gz archive");

    scoped_temp_directory destination;
    BOOST_LOG_SEV(lg, info) << "Extracting non-archive: " << not_an_archive.path();
    CHECK_THROWS_AS(archiver::extract(not_an_archive.path(), destination.path()),
                    std::runtime_error);
}
