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
#include <filesystem>
#include <system_error>
#include <boost/throw_exception.hpp>
#include "ores.utility/log/logger.hpp"
#include "ores.utility/filesystem/io_error.hpp"
#include "ores.utility/filesystem/file_not_found.hpp"
#include "ores.utility/filesystem/file.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("utility.filesystem.file"));

const std::string invalid_directory("Not a directory: ");
const std::string file_not_found_msg("File not found: ");
const std::string directory_not_found("Could not find directory: ");
const std::string failed_delete("Failed to delete output directory.");
const std::string failed_create("Failed to create output directory.");

const std::string dot(".");

}

namespace ores::utility::filesystem {

std::string read_file_content(std::istream& s) {
    s.exceptions(std::ifstream::failbit | std::ifstream::badbit);
    std::string r(
        (std::istreambuf_iterator<char>(s)),
        std::istreambuf_iterator<char>());

    BOOST_LOG_SEV(lg, trace) << "Total bytes read:" << r.size();
    return r;
}

std::string read_file_content(const std::filesystem::path& path) {
    BOOST_LOG_SEV(lg, trace) << "Reading content for path:"
                             << path.generic_string();

    if (!std::filesystem::exists(path)) {
        const auto gs(path.generic_string());
        BOOST_LOG_SEV(lg, error) << file_not_found_msg << gs;
        BOOST_THROW_EXCEPTION(file_not_found(file_not_found_msg + gs));
    }

    std::ifstream s(path);
    return read_file_content(s);
}

void write_file_content(const std::filesystem::path& path,
    const std::string& content) {
    std::ofstream stream(path);
    stream.exceptions(std::ifstream::failbit | std::ifstream::badbit);
    stream << content;
}

std::set<std::filesystem::path>
find_files(const std::filesystem::path& dir) {
    using namespace std::filesystem;
    std::set<path> r;

    if (!exists(dir)) {
        const auto gs(dir.generic_string());
        BOOST_LOG_SEV(lg, error) << directory_not_found << gs;
        BOOST_THROW_EXCEPTION(file_not_found(directory_not_found + gs));
    }

    if (!is_directory(dir)) {
        const auto gs(dir.generic_string());
        BOOST_LOG_SEV(lg, error) << invalid_directory << gs;
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
find_files(const std::vector<std::filesystem::path>& dirs) {
    std::set<std::filesystem::path> r;

    for (const auto& d : dirs) {
        const auto files(find_files(d));
        r.insert(files.begin(), files.end());
    }

    return r;
}

std::set<std::filesystem::path>
find_files(const std::list<std::filesystem::path>& dirs) {
    std::set<std::filesystem::path> r;

    for (const auto& d : dirs) {
        const auto files(find_files(d));
        r.insert(files.begin(), files.end());
    }

    return r;
}

std::filesystem::path find_file_recursively_upwards(
    std::filesystem::path starting_directory,
    const std::filesystem::path& relative_file_path) {

    BOOST_LOG_SEV(lg, debug) << "Starting directory: " << starting_directory
                             << " relative file path: " << relative_file_path;

    if (relative_file_path.is_absolute()) {
        /*
         * User is lying, the path is already absolute.
         */
        const auto gs(relative_file_path.generic_string());
        BOOST_LOG_SEV(lg, debug) << "Path is absolute: " << gs;
        return relative_file_path;
    }

    if (starting_directory.empty())
        starting_directory = std::filesystem::current_path();

    if (!std::filesystem::is_directory(starting_directory)) {
        const auto gs(starting_directory.generic_string());
        BOOST_LOG_SEV(lg, error) << invalid_directory << gs;
        BOOST_THROW_EXCEPTION(file_not_found(invalid_directory + gs));
    }

    using namespace std::filesystem;
    auto directory_path(starting_directory);

    do {
        path abs = absolute(directory_path / relative_file_path);
        BOOST_LOG_SEV(lg, debug) << "Trying: " << abs.generic_string();

        if (exists(abs)) {
            BOOST_LOG_SEV(lg, debug) << "Found file.";
            return abs;
        }

        directory_path = directory_path.parent_path();
    } while (directory_path.has_parent_path());

    const auto gs(relative_file_path.generic_string());
    BOOST_LOG_SEV(lg, debug) << "Could not find file: " << gs;
    return {};
}

void remove(const std::list<std::filesystem::path>& files) {
    for (const auto& f : files) {
        BOOST_LOG_SEV(lg, debug) << "Removing file: " << f.generic_string();
        std::filesystem::remove(f);
    }
}

void remove_empty_directories(const std::filesystem::path& dir) {
    const auto gs(dir.generic_string());
    if (!exists(dir)) {
        BOOST_LOG_SEV(lg, error) << directory_not_found << gs;
        BOOST_THROW_EXCEPTION(file_not_found(directory_not_found + gs));
    }

    if (!is_directory(dir)) {
        BOOST_LOG_SEV(lg, error) << invalid_directory << gs;
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
        BOOST_LOG_SEV(lg, debug) << "Removing empty directory: " << gs;
        remove(dir);
        return;
    }

    BOOST_LOG_SEV(lg, trace) << "Ignoring non-empty directory: " << gs;
}

void remove_empty_directories(const std::list<std::filesystem::path>& dirs) {
    for (const auto& dir : dirs)
        remove_empty_directories(dir);
}

void recreate_directory(const std::filesystem::path& dir) {
    if (std::filesystem::exists(dir)) {
        BOOST_LOG_SEV(lg, debug) << "Path already exists: "
                                 << dir.generic_string();

        if (!is_directory(dir)) {
            const auto gs(dir.generic_string());
            BOOST_LOG_SEV(lg, error) << invalid_directory << gs;
            BOOST_THROW_EXCEPTION(io_error(invalid_directory + gs));
        }

        std::error_code ec;
        std::filesystem::remove_all(dir, ec);
        if (ec) {
            BOOST_LOG_SEV(lg, error) << failed_delete;
            BOOST_THROW_EXCEPTION(io_error(failed_delete));
        }
        BOOST_LOG_SEV(lg, debug) << "Deleted output data directory.";
    }

    std::error_code ec;
    std::filesystem::create_directories(dir, ec);
    if (ec) {
        BOOST_LOG_SEV(lg, error) << failed_create;
        BOOST_THROW_EXCEPTION(io_error(failed_create));
    }
    BOOST_LOG_SEV(lg, debug) << "Created output data directory: "
                             << dir.generic_string();
}

}
