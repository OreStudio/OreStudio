/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.platform/filesystem/file.hpp"
#include "ores.platform/filesystem/file_not_found.hpp"
#include "ores.platform/filesystem/io_error.hpp"
#include "ores.platform/filesystem/scoped_temp_file.hpp"
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <filesystem>
#include <fstream>
#include <list>
#include <sstream>

using ores::platform::filesystem::file;
using ores::platform::filesystem::file_not_found;
using ores::platform::filesystem::scoped_temp_file;
using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.platform.tests");
const std::string tags("[filesystem]");

scoped_temp_file create_temp_file(const std::string& content) {
    scoped_temp_file sut;
    file::write_content(sut.path(), content);
    return sut;
}

struct streamable_test_object {
    int value;
};

std::ostream& operator<<(std::ostream& os, const streamable_test_object& s) {
    return os << "value=" << s.value;
}

}

TEST_CASE("read_content_from_existing_file", tags) {
    auto lg(make_logger(test_suite));

    const std::string expected_content = "Hello, World!";
    auto temp_file = create_temp_file(expected_content);
    BOOST_LOG_SEV(lg, info) << "Created temp file: " << temp_file.path();

    std::string content = file::read_content(temp_file.path());

    CHECK(content == expected_content);
}

TEST_CASE("read_content_from_file_with_multiline_content", tags) {
    auto lg(make_logger(test_suite));

    const std::string expected_content = "Line 1\nLine 2\nLine 3\n";
    auto temp_file = create_temp_file(expected_content);
    BOOST_LOG_SEV(lg, info) << "Created temp file with multiline content";

    std::string content = file::read_content(temp_file.path());

    CHECK(content == expected_content);
}

TEST_CASE("read_content_from_nonexistent_file_throws_exception", tags) {
    auto lg(make_logger(test_suite));

    std::filesystem::path nonexistent("/nonexistent/path/to/file.txt");
    BOOST_LOG_SEV(lg, info) << "Attempting to read nonexistent file";

    CHECK_THROWS_AS(file::read_content(nonexistent), file_not_found);
}

TEST_CASE("read_content_from_istream", tags) {
    auto lg(make_logger(test_suite));

    const std::string expected_content = "Stream content here";
    std::istringstream iss(expected_content);
    BOOST_LOG_SEV(lg, info) << "Reading from istream";

    std::string content = file::read_content(iss);

    CHECK(content == expected_content);
}

TEST_CASE("read_empty_file", tags) {
    auto lg(make_logger(test_suite));

    auto temp_file = create_temp_file("");
    BOOST_LOG_SEV(lg, info) << "Reading empty file";

    std::string content = file::read_content(temp_file.path());

    CHECK(content.empty());
}

TEST_CASE("write_content_to_new_file", tags) {
    auto lg(make_logger(test_suite));

    scoped_temp_file sut;
    const std::string content = "Written content";

    BOOST_LOG_SEV(lg, info) << "Writing to file: " << sut.path();
    file::write_content(sut.path(), content);

    CHECK(std::filesystem::exists(sut.path()));
    std::string read_back = file::read_content(sut.path());
    CHECK(read_back == content);
}

TEST_CASE("write_content_overwrites_existing_file", tags) {
    auto lg(make_logger(test_suite));

    auto temp_file = create_temp_file("Original content");
    const std::string new_content = "New content";

    BOOST_LOG_SEV(lg, info) << "Overwriting file: " << temp_file.path();
    file::write_content(temp_file.path(), new_content);

    std::string read_back = file::read_content(temp_file.path());
    CHECK(read_back == new_content);
}

TEST_CASE("write_template_to_file", tags) {
    auto lg(make_logger(test_suite));

    scoped_temp_file sut;

    streamable_test_object obj{42};
    BOOST_LOG_SEV(lg, info) << "Writing streamable object to file";
    file::write(sut.path(), obj);

    CHECK(std::filesystem::exists(sut.path()));
    std::string content = file::read_content(sut.path());
    CHECK(content == "value=42");
}

TEST_CASE("remove_files_from_list", tags) {
    auto lg(make_logger(test_suite));

    auto file1 = create_temp_file("content1");
    auto file2 = create_temp_file("content2");

    std::list<std::filesystem::path> files = {file1.path(), file2.path()};
    BOOST_LOG_SEV(lg, info) << "Removing files from list";

    CHECK(std::filesystem::exists(file1.path()));
    CHECK(std::filesystem::exists(file2.path()));

    file::remove(files);

    CHECK(!std::filesystem::exists(file1.path()));
    CHECK(!std::filesystem::exists(file2.path()));
}

TEST_CASE("write_and_read_with_faker_content", tags) {
    auto lg(make_logger(test_suite));

    scoped_temp_file sut;

    std::string faker_content = std::string(faker::lorem::paragraph());
    BOOST_LOG_SEV(lg, info) << "Writing faker content to: " << sut.path();

    file::write_content(sut.path(), faker_content);
    std::string read_back = file::read_content(sut.path());

    CHECK(read_back == faker_content);
}
