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
#include "ores.platform/filesystem/file_not_found.hpp"
#include "ores.platform/filesystem/io_error.hpp"
#include <boost/throw_exception.hpp>
#include <filesystem>
#include <fstream>
#include <string>

namespace ores::platform::filesystem {

std::string file::read_content(std::istream& s) {
    s.exceptions(std::ifstream::failbit | std::ifstream::badbit);
    std::string r((std::istreambuf_iterator<char>(s)), std::istreambuf_iterator<char>());
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

void file::write_content(const std::filesystem::path& path, const std::string& content) {
    std::ofstream stream(path);
    stream.exceptions(std::ifstream::failbit | std::ifstream::badbit);
    stream << content;
}

void file::remove(const std::list<std::filesystem::path>& files) {
    for (const auto& f : files) {
        std::filesystem::remove(f);
    }
}

}
