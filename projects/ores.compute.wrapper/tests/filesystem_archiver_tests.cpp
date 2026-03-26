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
#include "ores.compute.wrapper/filesystem/archiver.hpp"

#include <fstream>
#include <stdexcept>
#include <filesystem>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.compute.wrapper.tests");
const std::string tags("[filesystem][archiver]");

namespace fs = std::filesystem;
using archiver = ores::compute::wrapper::filesystem::archiver;
using namespace ores::logging;

/**
 * @brief Creates (or recreates) a unique scratch directory under the system
 * temp dir. The caller is responsible for removing it after the test.
 */
fs::path make_scratch(const std::string& name) {
    const fs::path dir = fs::temp_directory_path() / ("ores_archiver_" + name);
    fs::remove_all(dir);
    fs::create_directories(dir);
    return dir;
}

void write_text(const fs::path& p, const std::string& content) {
    fs::create_directories(p.parent_path());
    std::ofstream f(p);
    f << content;
}

std::string read_text(const fs::path& p) {
    std::ifstream f(p);
    return {std::istreambuf_iterator<char>(f), {}};
}

}

TEST_CASE("pack_and_extract_single_file", tags) {
    auto lg(make_logger(test_suite));

    const auto scratch = make_scratch("single_file");
    const auto src  = scratch / "src";
    const auto out  = scratch / "output.tar.gz";
    const auto dest = scratch / "dest";
    fs::create_directories(src);
    fs::create_directories(dest);

    const std::string content = "hello, archiver";
    write_text(src / "hello.txt", content);

    BOOST_LOG_SEV(lg, info) << "Packing: " << src;
    archiver::pack(src, out);

    CHECK(fs::exists(out));
    CHECK(fs::file_size(out) > 0);

    BOOST_LOG_SEV(lg, info) << "Extracting to: " << dest;
    archiver::extract(out, dest);

    const auto extracted = dest / "hello.txt";
    REQUIRE(fs::exists(extracted));
    CHECK(read_text(extracted) == content);

    fs::remove_all(scratch);
}

TEST_CASE("pack_and_extract_multiple_files", tags) {
    auto lg(make_logger(test_suite));

    const auto scratch = make_scratch("multiple_files");
    const auto src  = scratch / "src";
    const auto out  = scratch / "output.tar.gz";
    const auto dest = scratch / "dest";
    fs::create_directories(src);
    fs::create_directories(dest);

    const std::vector<std::pair<std::string, std::string>> files = {
        { "alpha.txt",   "content of alpha" },
        { "beta.txt",    "content of beta"  },
        { "gamma.txt",   "content of gamma" },
    };

    for (const auto& [name, content] : files)
        write_text(src / name, content);

    BOOST_LOG_SEV(lg, info) << "Packing " << files.size() << " files from: " << src;
    archiver::pack(src, out);
    archiver::extract(out, dest);

    for (const auto& [name, expected] : files) {
        const auto extracted = dest / name;
        BOOST_LOG_SEV(lg, debug) << "Checking: " << extracted;
        REQUIRE(fs::exists(extracted));
        CHECK(read_text(extracted) == expected);
    }

    fs::remove_all(scratch);
}

TEST_CASE("pack_and_extract_preserves_subdirectory_structure", tags) {
    auto lg(make_logger(test_suite));

    const auto scratch = make_scratch("subdir_structure");
    const auto src  = scratch / "src";
    const auto out  = scratch / "output.tar.gz";
    const auto dest = scratch / "dest";
    fs::create_directories(dest);

    // Three levels deep
    write_text(src / "root.txt",              "root level");
    write_text(src / "a" / "a1.txt",          "a/a1");
    write_text(src / "a" / "b" / "ab1.txt",   "a/b/ab1");

    BOOST_LOG_SEV(lg, info) << "Packing nested structure from: " << src;
    archiver::pack(src, out);
    archiver::extract(out, dest);

    REQUIRE(fs::exists(dest / "root.txt"));
    CHECK(read_text(dest / "root.txt") == "root level");

    REQUIRE(fs::exists(dest / "a" / "a1.txt"));
    CHECK(read_text(dest / "a" / "a1.txt") == "a/a1");

    REQUIRE(fs::exists(dest / "a" / "b" / "ab1.txt"));
    CHECK(read_text(dest / "a" / "b" / "ab1.txt") == "a/b/ab1");

    fs::remove_all(scratch);
}

TEST_CASE("pack_and_extract_binary_content_roundtrip", tags) {
    auto lg(make_logger(test_suite));

    const auto scratch = make_scratch("binary_content");
    const auto src  = scratch / "src";
    const auto out  = scratch / "output.tar.gz";
    const auto dest = scratch / "dest";
    fs::create_directories(src);
    fs::create_directories(dest);

    // Build a buffer with all byte values 0x00–0xFF repeated
    std::vector<char> original(512);
    for (std::size_t i = 0; i < original.size(); ++i)
        original[i] = static_cast<char>(i % 256);

    const auto bin_path = src / "data.bin";
    {
        std::ofstream f(bin_path, std::ios::binary);
        f.write(original.data(), static_cast<std::streamsize>(original.size()));
    }

    BOOST_LOG_SEV(lg, info) << "Packing binary file (" << original.size()
        << " bytes) from: " << src;
    archiver::pack(src, out);
    archiver::extract(out, dest);

    const auto extracted = dest / "data.bin";
    REQUIRE(fs::exists(extracted));
    REQUIRE(fs::file_size(extracted) == original.size());

    std::ifstream f(extracted, std::ios::binary);
    const std::vector<char> actual(
        std::istreambuf_iterator<char>(f), {});
    CHECK(actual == original);

    fs::remove_all(scratch);
}

TEST_CASE("extract_nonexistent_archive_throws", tags) {
    auto lg(make_logger(test_suite));

    const auto dest = fs::temp_directory_path() / "ores_archiver_bad_extract";
    fs::create_directories(dest);

    const fs::path bad = fs::temp_directory_path() / "does_not_exist_xyz.tar.gz";

    BOOST_LOG_SEV(lg, info) << "Attempting to extract non-existent archive: " << bad;
    CHECK_THROWS_AS(archiver::extract(bad, dest), std::runtime_error);

    fs::remove_all(dest);
}
