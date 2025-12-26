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
#include "ores.platform/filesystem/file.hpp"

#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <fstream>
#include "ores.utility/log/make_logger.hpp"
#include "ores.platform/filesystem/file_not_found.hpp"
#include "ores.platform/filesystem/io_error.hpp"

namespace {

const std::string_view test_suite("ores.platform.tests");
const std::string tags("[filesystem]");

std::filesystem::path create_temp_file(const std::string& content) {
    auto temp_dir = std::filesystem::temp_directory_path();
    auto temp_file = temp_dir / ("test_file_" + std::to_string(std::rand()) + ".txt");
    std::ofstream ofs(temp_file);
    ofs << content;
    ofs.close();
    return temp_file;
}

std::filesystem::path create_temp_directory() {
    auto temp_dir = std::filesystem::temp_directory_path();
    auto test_dir = temp_dir / ("test_dir_" + std::to_string(std::rand()));
    std::filesystem::create_directories(test_dir);
    return test_dir;
}

struct streamable_test_object {
    int value;
};

std::ostream& operator<<(std::ostream& os, const streamable_test_object& s) {
    return os << "value=" << s.value;
}

}

using ores::platform::filesystem::file;
using ores::platform::filesystem::file_not_found;
using ores::platform::filesystem::io_error;
using namespace ores::utility::log;

TEST_CASE("read_content_from_existing_file", tags) {
    auto lg(make_logger(test_suite));

    const std::string expected_content = "Hello, World!";
    auto temp_file = create_temp_file(expected_content);
    BOOST_LOG_SEV(lg, info) << "Created temp file: " << temp_file;

    std::string content = file::read_content(temp_file);

    CHECK(content == expected_content);

    std::filesystem::remove(temp_file);
}

TEST_CASE("read_content_from_file_with_multiline_content", tags) {
    auto lg(make_logger(test_suite));

    const std::string expected_content = "Line 1\nLine 2\nLine 3\n";
    auto temp_file = create_temp_file(expected_content);
    BOOST_LOG_SEV(lg, info) << "Created temp file with multiline content";

    std::string content = file::read_content(temp_file);

    CHECK(content == expected_content);

    std::filesystem::remove(temp_file);
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

TEST_CASE("write_content_to_new_file", tags) {
    auto lg(make_logger(test_suite));

    auto temp_dir = std::filesystem::temp_directory_path();
    auto temp_file = temp_dir / ("write_test_" + std::to_string(std::rand()) + ".txt");
    const std::string content = "Written content";

    BOOST_LOG_SEV(lg, info) << "Writing to file: " << temp_file;
    file::write_content(temp_file, content);

    CHECK(std::filesystem::exists(temp_file));
    std::string read_back = file::read_content(temp_file);
    CHECK(read_back == content);

    std::filesystem::remove(temp_file);
}

TEST_CASE("write_content_overwrites_existing_file", tags) {
    auto lg(make_logger(test_suite));

    auto temp_file = create_temp_file("Original content");
    const std::string new_content = "New content";

    BOOST_LOG_SEV(lg, info) << "Overwriting file: " << temp_file;
    file::write_content(temp_file, new_content);

    std::string read_back = file::read_content(temp_file);
    CHECK(read_back == new_content);

    std::filesystem::remove(temp_file);
}

TEST_CASE("write_template_to_file", tags) {
    auto lg(make_logger(test_suite));

    auto temp_dir = std::filesystem::temp_directory_path();
    auto temp_file = temp_dir / ("write_template_" + std::to_string(std::rand()) + ".txt");

    streamable_test_object obj{42};
    BOOST_LOG_SEV(lg, info) << "Writing streamable object to file";
    file::write(temp_file, obj);

    CHECK(std::filesystem::exists(temp_file));
    std::string content = file::read_content(temp_file);
    CHECK(content == "value=42");

    std::filesystem::remove(temp_file);
}

TEST_CASE("find_files_in_directory", tags) {
    auto lg(make_logger(test_suite));

    auto temp_dir = create_temp_directory();

    // Create some files
    std::ofstream(temp_dir / "file1.txt") << "content1";
    std::ofstream(temp_dir / "file2.txt") << "content2";
    std::filesystem::create_directories(temp_dir / "subdir");
    std::ofstream(temp_dir / "subdir" / "file3.txt") << "content3";

    BOOST_LOG_SEV(lg, info) << "Finding files in: " << temp_dir;
    auto files = file::find_files(temp_dir);

    BOOST_LOG_SEV(lg, info) << "Found " << files.size() << " files";
    CHECK(files.size() == 3);

    std::filesystem::remove_all(temp_dir);
}

TEST_CASE("find_files_in_nonexistent_directory_throws", tags) {
    auto lg(make_logger(test_suite));

    std::filesystem::path nonexistent("/nonexistent/directory");
    BOOST_LOG_SEV(lg, info) << "Attempting to find files in nonexistent directory";

    CHECK_THROWS_AS(file::find_files(nonexistent), file_not_found);
}

TEST_CASE("find_files_with_file_path_throws", tags) {
    auto lg(make_logger(test_suite));

    auto temp_file = create_temp_file("content");
    BOOST_LOG_SEV(lg, info) << "Attempting to find files with file path";

    CHECK_THROWS_AS(file::find_files(temp_file), file_not_found);

    std::filesystem::remove(temp_file);
}

TEST_CASE("find_files_in_multiple_directories_vector", tags) {
    auto lg(make_logger(test_suite));

    auto dir1 = create_temp_directory();
    auto dir2 = create_temp_directory();

    std::ofstream(dir1 / "file1.txt") << "content1";
    std::ofstream(dir2 / "file2.txt") << "content2";

    std::vector<std::filesystem::path> dirs = {dir1, dir2};
    BOOST_LOG_SEV(lg, info) << "Finding files in multiple directories";

    auto files = file::find_files(dirs);

    CHECK(files.size() == 2);

    std::filesystem::remove_all(dir1);
    std::filesystem::remove_all(dir2);
}

TEST_CASE("find_files_in_multiple_directories_list", tags) {
    auto lg(make_logger(test_suite));

    auto dir1 = create_temp_directory();
    auto dir2 = create_temp_directory();

    std::ofstream(dir1 / "file1.txt") << "content1";
    std::ofstream(dir2 / "file2.txt") << "content2";

    std::list<std::filesystem::path> dirs = {dir1, dir2};
    BOOST_LOG_SEV(lg, info) << "Finding files in multiple directories (list)";

    auto files = file::find_files(dirs);

    CHECK(files.size() == 2);

    std::filesystem::remove_all(dir1);
    std::filesystem::remove_all(dir2);
}

TEST_CASE("find_file_recursively_upwards_with_absolute_path", tags) {
    auto lg(make_logger(test_suite));

    auto temp_file = create_temp_file("content");
    BOOST_LOG_SEV(lg, info) << "Finding absolute path: " << temp_file;

    auto result = file::find_file_recursively_upwards(
        std::filesystem::temp_directory_path(), temp_file);

    CHECK(result == temp_file);

    std::filesystem::remove(temp_file);
}

TEST_CASE("find_file_recursively_upwards_in_current_directory", tags) {
    auto lg(make_logger(test_suite));

    auto temp_dir = create_temp_directory();
    std::ofstream(temp_dir / "target.txt") << "content";

    BOOST_LOG_SEV(lg, info) << "Finding file in current directory: " << temp_dir;

    auto result = file::find_file_recursively_upwards(temp_dir, "target.txt");

    CHECK(!result.empty());
    CHECK(std::filesystem::exists(result));

    std::filesystem::remove_all(temp_dir);
}

TEST_CASE("find_file_recursively_upwards_not_found", tags) {
    auto lg(make_logger(test_suite));

    auto temp_dir = create_temp_directory();
    BOOST_LOG_SEV(lg, info) << "Looking for nonexistent file upwards from: " << temp_dir;

    auto result = file::find_file_recursively_upwards(temp_dir, "nonexistent_file_xyz.txt");

    CHECK(result.empty());

    std::filesystem::remove_all(temp_dir);
}

TEST_CASE("find_file_recursively_upwards_with_invalid_directory_throws", tags) {
    auto lg(make_logger(test_suite));

    auto temp_file = create_temp_file("content");
    BOOST_LOG_SEV(lg, info) << "Attempting upward search from file path";

    CHECK_THROWS_AS(
        file::find_file_recursively_upwards(temp_file, "something.txt"),
        file_not_found);

    std::filesystem::remove(temp_file);
}

TEST_CASE("remove_files_from_list", tags) {
    auto lg(make_logger(test_suite));

    auto file1 = create_temp_file("content1");
    auto file2 = create_temp_file("content2");

    std::list<std::filesystem::path> files = {file1, file2};
    BOOST_LOG_SEV(lg, info) << "Removing files from list";

    CHECK(std::filesystem::exists(file1));
    CHECK(std::filesystem::exists(file2));

    file::remove(files);

    CHECK(!std::filesystem::exists(file1));
    CHECK(!std::filesystem::exists(file2));
}

TEST_CASE("remove_empty_directories", tags) {
    auto lg(make_logger(test_suite));

    auto temp_dir = create_temp_directory();
    std::filesystem::create_directories(temp_dir / "empty1");
    std::filesystem::create_directories(temp_dir / "empty2");
    std::filesystem::create_directories(temp_dir / "nonempty");
    std::ofstream(temp_dir / "nonempty" / "file.txt") << "content";

    BOOST_LOG_SEV(lg, info) << "Removing empty directories from: " << temp_dir;

    // The function removes empty dirs recursively, including the parent if it becomes empty
    // but in this case nonempty has a file
    file::remove_empty_directories(temp_dir / "empty1");
    file::remove_empty_directories(temp_dir / "empty2");

    CHECK(!std::filesystem::exists(temp_dir / "empty1"));
    CHECK(!std::filesystem::exists(temp_dir / "empty2"));
    CHECK(std::filesystem::exists(temp_dir / "nonempty"));
    CHECK(std::filesystem::exists(temp_dir / "nonempty" / "file.txt"));

    std::filesystem::remove_all(temp_dir);
}

TEST_CASE("remove_empty_directories_nonexistent_throws", tags) {
    auto lg(make_logger(test_suite));

    std::filesystem::path nonexistent("/nonexistent/directory");
    BOOST_LOG_SEV(lg, info) << "Attempting to remove empty dirs from nonexistent path";

    CHECK_THROWS_AS(file::remove_empty_directories(nonexistent), file_not_found);
}

TEST_CASE("remove_empty_directories_on_file_throws", tags) {
    auto lg(make_logger(test_suite));

    auto temp_file = create_temp_file("content");
    BOOST_LOG_SEV(lg, info) << "Attempting to remove empty dirs on file path";

    CHECK_THROWS_AS(file::remove_empty_directories(temp_file), io_error);

    std::filesystem::remove(temp_file);
}

TEST_CASE("recreate_directory_creates_new", tags) {
    auto lg(make_logger(test_suite));

    auto temp_base = std::filesystem::temp_directory_path();
    auto new_dir = temp_base / ("recreate_test_" + std::to_string(std::rand()));

    BOOST_LOG_SEV(lg, info) << "Creating new directory: " << new_dir;
    CHECK(!std::filesystem::exists(new_dir));

    file::recreate_directory(new_dir);

    CHECK(std::filesystem::exists(new_dir));
    CHECK(std::filesystem::is_directory(new_dir));

    std::filesystem::remove_all(new_dir);
}

TEST_CASE("recreate_directory_clears_existing", tags) {
    auto lg(make_logger(test_suite));

    auto temp_dir = create_temp_directory();
    std::ofstream(temp_dir / "existing_file.txt") << "content";

    BOOST_LOG_SEV(lg, info) << "Recreating existing directory: " << temp_dir;
    CHECK(std::filesystem::exists(temp_dir / "existing_file.txt"));

    file::recreate_directory(temp_dir);

    CHECK(std::filesystem::exists(temp_dir));
    CHECK(std::filesystem::is_directory(temp_dir));
    CHECK(!std::filesystem::exists(temp_dir / "existing_file.txt"));

    std::filesystem::remove_all(temp_dir);
}

TEST_CASE("recreate_directory_on_file_throws", tags) {
    auto lg(make_logger(test_suite));

    auto temp_file = create_temp_file("content");
    BOOST_LOG_SEV(lg, info) << "Attempting to recreate directory on file path";

    CHECK_THROWS_AS(file::recreate_directory(temp_file), io_error);

    std::filesystem::remove(temp_file);
}

TEST_CASE("read_empty_file", tags) {
    auto lg(make_logger(test_suite));

    auto temp_file = create_temp_file("");
    BOOST_LOG_SEV(lg, info) << "Reading empty file";

    std::string content = file::read_content(temp_file);

    CHECK(content.empty());

    std::filesystem::remove(temp_file);
}

TEST_CASE("write_and_read_with_faker_content", tags) {
    auto lg(make_logger(test_suite));

    auto temp_dir = std::filesystem::temp_directory_path();
    auto temp_file = temp_dir / ("faker_test_" + std::to_string(std::rand()) + ".txt");

    std::string faker_content = std::string(faker::lorem::paragraph());
    BOOST_LOG_SEV(lg, info) << "Writing faker content to: " << temp_file;

    file::write_content(temp_file, faker_content);
    std::string read_back = file::read_content(temp_file);

    CHECK(read_back == faker_content);

    std::filesystem::remove(temp_file);
}
