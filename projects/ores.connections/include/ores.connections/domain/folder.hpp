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
#ifndef ORES_CONNECTIONS_DOMAIN_FOLDER_HPP
#define ORES_CONNECTIONS_DOMAIN_FOLDER_HPP

#include <string>
#include <optional>
#include <boost/uuid/uuid.hpp>

namespace ores::connections::domain {

/**
 * @brief Represents a folder for organizing server connections hierarchically.
 *
 * Folders can be nested to create a tree structure. A folder with no parent_id
 * is a root-level folder.
 */
struct folder final {
    /**
     * @brief Unique identifier for this folder.
     */
    boost::uuids::uuid id;

    /**
     * @brief Display name of the folder.
     */
    std::string name;

    /**
     * @brief Parent folder ID, if this is a nested folder.
     *
     * If empty, this folder is at the root level.
     */
    std::optional<boost::uuids::uuid> parent_id;

    /**
     * @brief Optional description or notes about this folder.
     */
    std::string description;
};

}

#endif
