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
#include "ores.storage.core/filesystem/archive_reader.hpp"
#include "ores.storage.core/filesystem/archiver.hpp"
#include "test_suite.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <string>
#include <vector>

using ores::platform::filesystem::scoped_temp_directory;
using ores::platform::filesystem::scoped_temp_file;
using ores::storage::filesystem::archive_reader;
using ores::storage::filesystem::archiver;
using namespace ores::logging;

namespace {

void write_archive_file(const std::filesystem::path& path, const std::string& content) {
    std::filesystem::create_directories(path.parent_path());
    std::ofstream out(path, std::ios::binary);
    out.write(content.data(), static_cast<std::streamsize>(content.size()));
}

/// The bytes of a file, as the store would hold them.
std::vector<std::byte> packed_of(const std::filesystem::path& path) {
    std::ifstream in(path, std::ios::binary);
    std::vector<char> bytes((std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());
    std::vector<std::byte> out(bytes.size());
    std::memcpy(out.data(), bytes.data(), bytes.size());
    return out;
}

std::string text_of(const std::vector<std::byte>& bytes) {
    return std::string(reinterpret_cast<const char*>(bytes.data()), bytes.size());
}

}

TEST_CASE("archive_reader_states_the_files_a_package_holds", "[filesystem]") {
    auto lg(make_logger(test_suite));

    scoped_temp_directory source;
    write_archive_file(source.path() / "manifest.txt", "name=ores.reporting\n");
    write_archive_file(source.path() / "output" / "fx.csv", "pair,value\nEUR/USD,1.09\n");

    scoped_temp_file archive;
    archiver::pack(source.path(), archive.path());
    const auto packed = packed_of(archive.path());

    // The archive states its entries in the order they were packed, which the
    // packer does not promise, so the test reads them as the set they are.
    auto entries = archive_reader::index(packed);
    std::ranges::sort(entries, {}, &archive_reader::entry::name);
    REQUIRE(entries.size() == 2);
    CHECK(entries[0].name == "manifest.txt");
    CHECK(entries[0].size == std::string("name=ores.reporting\n").size());
    CHECK(entries[1].name == "output/fx.csv");
}

TEST_CASE("archive_reader_reads_one_file_out_of_a_package", "[filesystem]") {
    auto lg(make_logger(test_suite));

    scoped_temp_directory source;
    write_archive_file(source.path() / "manifest.txt", "name=ores.reporting\n");
    write_archive_file(source.path() / "output" / "fx.csv", "pair,value\nEUR/USD,1.09\n");

    scoped_temp_file archive;
    archiver::pack(source.path(), archive.path());
    const auto packed = packed_of(archive.path());

    CHECK(text_of(archive_reader::extract(packed, "output/fx.csv")) ==
          "pair,value\nEUR/USD,1.09\n");
    CHECK(archive_reader::extract(packed, "output/none.csv").empty());
}

TEST_CASE("archive_reader_refuses_bytes_that_are_no_archive", "[filesystem]") {
    auto lg(make_logger(test_suite));

    const std::string not_an_archive = "these are not the bytes of an archive";
    std::vector<std::byte> packed(not_an_archive.size());
    std::memcpy(packed.data(), not_an_archive.data(), not_an_archive.size());

    CHECK_THROWS_AS(archive_reader::index(packed), std::runtime_error);
}
