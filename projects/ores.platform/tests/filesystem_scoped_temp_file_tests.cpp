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
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <filesystem>
#include <fstream>
#include <set>
#include <string>
#include <system_error>
#include <utility>

namespace {

const std::string_view test_suite("ores.platform.tests");
const std::string tags("[filesystem]");

/**
 * @brief Moves a guard into a short-lived owner and reports the path that
 * owner held, so the caller can observe what the owner's destructor removed.
 */
template <typename Guard>
std::filesystem::path path_removed_by_owner(Guard&& source) {
    Guard owner(std::move(source));
    return owner.path();
}

}

using namespace ores::logging;
using ores::platform::filesystem::scoped_temp_directory;
using ores::platform::filesystem::scoped_temp_file;

TEST_CASE("create_temp_file_at_path_that_exists", tags) {
    auto lg(make_logger(test_suite));

    scoped_temp_file sut;

    BOOST_LOG_SEV(lg, info) << "Created temp file: " << sut.path();
    CHECK(std::filesystem::exists(sut.path()));
}

TEST_CASE("delete_temp_file_on_destruction", tags) {
    auto lg(make_logger(test_suite));

    std::filesystem::path temp_path;
    {
        scoped_temp_file sut;
        temp_path = sut.path();
    }

    BOOST_LOG_SEV(lg, info) << "Released temp file: " << temp_path;
    CHECK(!std::filesystem::exists(temp_path));
}

TEST_CASE("place_temp_file_under_system_temp_directory", tags) {
    auto lg(make_logger(test_suite));

    scoped_temp_file sut;
    const auto temp_dir = std::filesystem::temp_directory_path();
    const auto parent = sut.path().parent_path();

    BOOST_LOG_SEV(lg, info) << "Temp file parent: " << parent;
    CHECK(parent == temp_dir);
}

TEST_CASE("create_temp_directory_at_path_that_exists", tags) {
    auto lg(make_logger(test_suite));

    scoped_temp_directory sut;

    BOOST_LOG_SEV(lg, info) << "Created temp directory: " << sut.path();
    CHECK(std::filesystem::exists(sut.path()));
    CHECK(std::filesystem::is_directory(sut.path()));
}

TEST_CASE("delete_temp_directory_and_its_contents_on_destruction", tags) {
    auto lg(make_logger(test_suite));

    std::filesystem::path temp_path;
    {
        scoped_temp_directory sut;
        temp_path = sut.path();
        std::ofstream(temp_path / "payload.txt") << "payload";
        std::filesystem::create_directory(temp_path / "nested");
    }

    BOOST_LOG_SEV(lg, info) << "Released temp directory: " << temp_path;
    CHECK(!std::filesystem::exists(temp_path));
}

TEST_CASE("place_temp_directory_under_system_temp_directory", tags) {
    auto lg(make_logger(test_suite));

    scoped_temp_directory sut;
    const auto temp_dir = std::filesystem::temp_directory_path();
    const auto parent = sut.path().parent_path();

    BOOST_LOG_SEV(lg, info) << "Temp directory parent: " << parent;
    CHECK(parent == temp_dir);
}

TEST_CASE("create_distinct_paths_for_consecutive_temp_files", tags) {
    auto lg(make_logger(test_suite));

    scoped_temp_file first;
    scoped_temp_file second;

    BOOST_LOG_SEV(lg, info) << "First: " << first.path() << " second: " << second.path();
    CHECK(first.path() != second.path());
}

TEST_CASE("create_distinct_paths_for_consecutive_temp_directories", tags) {
    auto lg(make_logger(test_suite));

    scoped_temp_directory first;
    scoped_temp_directory second;

    BOOST_LOG_SEV(lg, info) << "First: " << first.path() << " second: " << second.path();
    CHECK(first.path() != second.path());
}

TEST_CASE("create_distinct_paths_for_many_temp_files", tags) {
    auto lg(make_logger(test_suite));

    const std::size_t count = 100;
    std::set<std::filesystem::path> paths;
    for (std::size_t i = 0; i < count; ++i) {
        scoped_temp_file sut;
        paths.insert(sut.path());
    }

    BOOST_LOG_SEV(lg, info) << "Created " << paths.size() << " temp files";
    CHECK(paths.size() == count);
}

TEST_CASE("create_temp_file_as_a_regular_file", tags) {
    auto lg(make_logger(test_suite));

    scoped_temp_file sut;

    BOOST_LOG_SEV(lg, info) << "Temp file: " << sut.path();
    CHECK(std::filesystem::is_regular_file(sut.path()));
    CHECK(!std::filesystem::is_directory(sut.path()));
}

TEST_CASE("hold_content_written_to_temp_file", tags) {
    auto lg(make_logger(test_suite));

    const std::string expected_content = "Hello, World!";
    scoped_temp_file sut;
    {
        std::ofstream stream(sut.path());
        stream << expected_content;
    }

    std::ifstream stream(sut.path());
    const std::string content((std::istreambuf_iterator<char>(stream)),
                              std::istreambuf_iterator<char>());

    BOOST_LOG_SEV(lg, info) << "Read back: " << content;
    CHECK(content == expected_content);
}

TEST_CASE("keep_temp_file_on_disk_after_release", tags) {
    auto lg(make_logger(test_suite));

    std::filesystem::path temp_path;
    {
        scoped_temp_file sut;
        temp_path = sut.release();
    }

    BOOST_LOG_SEV(lg, info) << "Released temp file: " << temp_path;
    CHECK(!temp_path.empty());
    CHECK(std::filesystem::exists(temp_path));

    std::error_code ec;
    std::filesystem::remove(temp_path, ec);
}

TEST_CASE("keep_temp_directory_on_disk_after_release", tags) {
    auto lg(make_logger(test_suite));

    std::filesystem::path temp_path;
    {
        scoped_temp_directory sut;
        temp_path = sut.release();
    }

    BOOST_LOG_SEV(lg, info) << "Released temp directory: " << temp_path;
    CHECK(!temp_path.empty());
    CHECK(std::filesystem::exists(temp_path));

    std::error_code ec;
    std::filesystem::remove_all(temp_path, ec);
}

TEST_CASE("keep_temp_file_of_moved_to_guard_after_moved_from_guard_dies", tags) {
    auto lg(make_logger(test_suite));

    scoped_temp_file source;
    const auto source_path = source.path();

    BOOST_LOG_SEV(lg, info) << "Source: " << source_path;
    CHECK(std::filesystem::exists(source_path));

    const auto removed = path_removed_by_owner(std::move(source));

    CHECK(removed == source_path);
    CHECK(!std::filesystem::exists(source_path));
}

TEST_CASE("keep_temp_directory_of_moved_to_guard_after_moved_from_guard_dies", tags) {
    auto lg(make_logger(test_suite));

    scoped_temp_directory source;
    const auto source_path = source.path();

    BOOST_LOG_SEV(lg, info) << "Source: " << source_path;
    CHECK(std::filesystem::exists(source_path));

    const auto removed = path_removed_by_owner(std::move(source));

    CHECK(removed == source_path);
    CHECK(!std::filesystem::exists(source_path));
}

TEST_CASE("keep_temp_file_held_by_another_owner_when_moved_from_guard_dies", tags) {
    auto lg(make_logger(test_suite));

    scoped_temp_file source;
    const auto evicted_path = source.path();
    std::filesystem::path target_path;
    {
        scoped_temp_file target;
        target_path = target.path();
        source = std::move(target);
    }

    BOOST_LOG_SEV(lg, info) << "Source now holds: " << source.path();
    CHECK(source.path() == target_path);
    CHECK(std::filesystem::exists(target_path));
    CHECK(!std::filesystem::exists(evicted_path));
}

TEST_CASE("keep_temp_directory_held_by_another_owner_when_moved_from_guard_dies", tags) {
    auto lg(make_logger(test_suite));

    scoped_temp_directory source;
    const auto evicted_path = source.path();
    std::filesystem::path target_path;
    {
        scoped_temp_directory target;
        target_path = target.path();
        source = std::move(target);
    }

    BOOST_LOG_SEV(lg, info) << "Source now holds: " << source.path();
    CHECK(source.path() == target_path);
    CHECK(std::filesystem::exists(target_path));
    CHECK(!std::filesystem::exists(evicted_path));
}
