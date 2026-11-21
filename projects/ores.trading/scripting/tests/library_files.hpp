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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_TRADING_SCRIPTING_TESTS_LIBRARY_FILES_HPP
#define ORES_TRADING_SCRIPTING_TESTS_LIBRARY_FILES_HPP

#include <filesystem>
#include <fstream>
#include <sstream>
#include <string>

/**
 * @file library_files.hpp
 * @brief The files the scripting tests read: the script libraries ORE's examples
 * ship, and the seed the store's library is loaded from.
 */
namespace ores::trading::scripting::tests {

/// The repository's root, from this file's place in it.
inline std::filesystem::path project_root() {
    return std::filesystem::path(__FILE__)
        .parent_path()
        .parent_path()
        .parent_path()
        .parent_path()
        .parent_path();
}

/// A file's bytes.
inline std::string file_text(const std::filesystem::path& path) {
    std::ifstream in(path, std::ios::binary);
    std::ostringstream out;
    out << in.rdbuf();
    return out.str();
}

}

#endif
