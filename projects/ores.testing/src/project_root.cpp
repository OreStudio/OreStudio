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
#include "ores.testing/project_root.hpp"

#include <stdexcept>

namespace ores::testing {

namespace {

std::filesystem::path find_project_root() {
    auto current = std::filesystem::current_path();

    while (!current.empty() && current != current.root_path()) {
        if (std::filesystem::exists(current / ".git")) {
            return current;
        }
        current = current.parent_path();
    }

    throw std::runtime_error(
        "Could not find project root: no .git directory found in path hierarchy");
}

std::filesystem::path& cached_root() {
    static std::filesystem::path root;
    return root;
}

}

std::filesystem::path project_root::get() {
    auto& root = cached_root();
    if (root.empty()) {
        root = find_project_root();
    }
    return root;
}

std::filesystem::path project_root::resolve(const std::filesystem::path& relative_path) {
    return get() / relative_path;
}

}
