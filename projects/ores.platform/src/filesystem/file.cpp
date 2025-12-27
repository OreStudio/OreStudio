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
#include "ores.platform/filesystem/file.hpp"

#include <filesystem>
#include <system_error>
#include <boost/throw_exception.hpp>
#include "ores.platform/filesystem/io_error.hpp"
#include "ores.platform/filesystem/file_not_found.hpp"

namespace ores::platform::filesystem {

std::string file::read_content(std::istream& s) {
    s.exceptions(std::ifstream::failbit | std::ifstream::badbit);
    std::string r(
        (std::istreambuf_iterator<char>(s)),
        std::istreambuf_iterator<char>());
    return r;
}

std::string file::read_content(const std::filesystem::path& path) {
    if (!std::filesystem::exists(path)) {
        static const std::string file_not_found_msg("File not found: ");
        const auto gs(path.generic_string());
        BOOST_THROW_EXCEPTION(file_not_found(file_not_found_msg + gs));
    }

    std::ifstream s(path);
    return read_content(s);
}

void file::write_content(const std::filesystem::path& path,
    const std::string& content) {
    std::ofstream stream(path);
    stream.exceptions(std::ifstream::failbit | std::ifstream::badbit);
    stream << content;
}

std::set<std::filesystem::path>
file::find_files(const std::filesystem::path& dir) {
    using namespace std::filesystem;
    std::set<path> r;

    if (!exists(dir)) {
        const std::string directory_not_found("Could not find directory: ");
        const auto gs(dir.generic_string());
        BOOST_THROW_EXCEPTION(file_not_found(directory_not_found + gs));
    }

    if (!is_directory(dir)) {
        const std::string invalid_directory("Not a directory: ");
        const auto gs(dir.generic_string());
        BOOST_THROW_EXCEPTION(file_not_found(invalid_directory + gs));
    }

    recursive_directory_iterator i(dir);
    const recursive_directory_iterator end;
    while (i != end) {
        const auto p(i->path());
        const auto abs(std::filesystem::absolute(p));
        if (is_regular_file(abs))
            r.insert(abs);
        ++i;
    }
    return r;
}

std::set<std::filesystem::path>
file::find_files(const std::vector<std::filesystem::path>& dirs) {
    std::set<std::filesystem::path> r;

    for (const auto& d : dirs) {
        const auto files(find_files(d));
        r.insert(files.begin(), files.end());
    }

    return r;
}

std::set<std::filesystem::path>
file::find_files(const std::list<std::filesystem::path>& dirs) {
    std::set<std::filesystem::path> r;

    for (const auto& d : dirs) {
        const auto files(find_files(d));
        r.insert(files.begin(), files.end());
    }

    return r;
}

std::filesystem::path file::find_file_recursively_upwards(
    std::filesystem::path starting_directory,
    const std::filesystem::path& relative_file_path) {

    if (relative_file_path.is_absolute()) {
        return relative_file_path;
    }

    if (starting_directory.empty())
        starting_directory = std::filesystem::current_path();

    if (!std::filesystem::is_directory(starting_directory)) {
        const std::string invalid_directory("Not a directory: ");
        const auto gs(starting_directory.generic_string());
        BOOST_THROW_EXCEPTION(file_not_found(invalid_directory + gs));
    }

    using namespace std::filesystem;
    auto directory_path(starting_directory);

    do {
        path abs = absolute(directory_path / relative_file_path);
        if (exists(abs)) {
            return abs;
        }

        directory_path = directory_path.parent_path();
    } while (directory_path.has_parent_path());

    return {};
}

void file::remove(const std::list<std::filesystem::path>& files) {
    for (const auto& f : files) {
        std::filesystem::remove(f);
    }
}

void file::remove_empty_directories(const std::filesystem::path& dir) {
    const auto gs(dir.generic_string());
    if (!exists(dir)) {
        const std::string directory_not_found("Could not find directory: ");
        BOOST_THROW_EXCEPTION(file_not_found(directory_not_found + gs));
    }

    if (!is_directory(dir)) {
        const std::string invalid_directory("Not a directory: ");
        BOOST_THROW_EXCEPTION(io_error(invalid_directory + gs));
    }

    using std::filesystem::directory_iterator;
    directory_iterator end;
    directory_iterator i(dir);
    while (i != end) {
        if (is_directory(i->status())) {
            const auto p(i->path());
            ++i;
            remove_empty_directories(p);
        } else {
            ++i;
        }
    }

    if (is_empty(dir)) {
        std::filesystem::remove(dir);
        return;
    }
}

void file::remove_empty_directories(const std::list<std::filesystem::path>& dirs) {
    for (const auto& dir : dirs)
        remove_empty_directories(dir);
}

void file::recreate_directory(const std::filesystem::path& dir) {
    if (std::filesystem::exists(dir)) {
        if (!is_directory(dir)) {
            const std::string invalid_directory("Not a directory: ");
            const auto gs(dir.generic_string());
            BOOST_THROW_EXCEPTION(io_error(invalid_directory + gs));
        }

        std::error_code ec;
        std::filesystem::remove_all(dir, ec);
        if (ec) {
            const std::string failed_delete("Failed to delete output directory.");
            BOOST_THROW_EXCEPTION(io_error(failed_delete));
        }
    }

    std::error_code ec;
    std::filesystem::create_directories(dir, ec);
    if (ec) {
        const std::string failed_create("Failed to create output directory.");
        BOOST_THROW_EXCEPTION(io_error(failed_create));
    }
}

}
