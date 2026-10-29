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
#ifndef ORES_TESTING_TEST_FILES_HPP
#define ORES_TESTING_TEST_FILES_HPP

#include "ores.testing/project_root.hpp"
#include <filesystem>
#include <fstream>
#include <sstream>
#include <string>

namespace ores::testing {

/**
 * @brief A file of the ORE corpus the repository ships, by its path under it.
 *
 * Several suites read the same corpus, so the path is resolved in one place.
 */
inline std::filesystem::path ore_path(const std::string& relative) {
    return project_root::resolve("external/ore/" + relative);
}

/**
 * @brief The bytes of a file, as text.
 */
inline std::string read_file(const std::filesystem::path& path) {
    std::ifstream in(path, std::ios::binary);
    std::ostringstream buffer;
    buffer << in.rdbuf();
    return buffer.str();
}

}

#endif
