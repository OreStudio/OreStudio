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
#ifndef ORES_PLATFORM_FILESYSTEM_FILE_HPP
#define ORES_PLATFORM_FILESYSTEM_FILE_HPP

#include <set>
#include <list>
#include <string>
#include <fstream>
#include <sstream>
#include <filesystem>
#include "ores.platform/filesystem/io_error.hpp"

namespace ores::platform::filesystem {

class file final {
public:
    /**
     * @brief Returns the contents of the file.
     */
    static std::string read_content(std::istream& s);

    /**
     * @brief Returns the contents of the file.
     */
    static std::string read_content(const std::filesystem::path& p);

    /**
     * @brief Writes the string to a file located at path.
     */
    static void write_content(const std::filesystem::path& path,
        const std::string& content);

    /**
     * @brief Dump the object as a stream to the file.
     */
    template<typename Ioable>
    static void write(const std::filesystem::path& path, const Ioable& target) {
        try {
            std::ofstream os(path);
            os.exceptions(std::ifstream::failbit | std::ifstream::badbit);
            os << target;
        } catch (const std::exception& e) {
            std::ostringstream s;
            s << "An error occurred whilst trying to write a file. Path: '"
              << path.generic_string() << "'. Error: " << e.what();
            throw io_error(s.str());
        }
    }

    /**
     * @brief Returns all files available in all input directories, recursively.
     *
     * Returned paths are absolute paths. If you supply the same directory
     * multiple times, the files will be read multiple times but returned
     * on once in the set.
     */
    /**@{*/
    static std::set<std::filesystem::path>
    find_files(const std::filesystem::path& dir);
    static std::set<std::filesystem::path>
    find_files(const std::vector<std::filesystem::path>& dirs);
    static std::set<std::filesystem::path>
    find_files(const std::list<std::filesystem::path>& dirs);
    /**@}*/

    /**
     * @brief Finds the relative path, by searching recursively upwards
     * from the starting directory.
     */
    static std::filesystem::path find_file_recursively_upwards(
        std::filesystem::path starting_directory,
        const std::filesystem::path& relative_file_path);

    /**
     * @brief Deletes all files in the supplied list.
     */
    static void remove(const std::list<std::filesystem::path>& files);

    /**
     * @brief Removes all empty directories, recursively.
     */
    /**@{*/
    static void remove_empty_directories(const std::filesystem::path& dir);
    static void remove_empty_directories(const std::list<std::filesystem::path>& dirs);
    /**@}*/

    /**
     * @brief If the directory exists, deletes it and recreates it. Otherwise,
     * creates it.
     *
     * @throws If the path points to a file.
     */
    static void recreate_directory(const std::filesystem::path& dir);
};

}

#endif
