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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_TESTING_PROJECT_ROOT_HPP
#define ORES_TESTING_PROJECT_ROOT_HPP

#include <filesystem>

namespace ores::testing {

/**
 * @brief Provides access to project root directory for test resource location.
 *
 * This helper finds the project root by walking up from the current working
 * directory looking for a .git directory. Tests can use this to locate
 * resources using project-relative paths, avoiding fragile relative paths
 * like "../../../".
 */
class project_root {
public:
    /**
     * @brief Returns the absolute path to the project root directory.
     *
     * Searches upward from the current working directory for a directory
     * containing .git. The result is cached after the first call.
     *
     * @throws std::runtime_error if .git directory cannot be found.
     */
    static std::filesystem::path get();

    /**
     * @brief Resolves a project-relative path to an absolute path.
     *
     * @param relative_path Path relative to the project root (e.g., "external/ore/examples")
     * @return Absolute path to the resource
     * @throws std::runtime_error if project root cannot be found.
     */
    static std::filesystem::path resolve(const std::filesystem::path& relative_path);
};

}

#endif
