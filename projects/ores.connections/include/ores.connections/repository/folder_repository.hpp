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
#ifndef ORES_CONNECTIONS_REPOSITORY_FOLDER_REPOSITORY_HPP
#define ORES_CONNECTIONS_REPOSITORY_FOLDER_REPOSITORY_HPP

#include <vector>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.connections/domain/folder.hpp"
#include "ores.connections/repository/sqlite_context.hpp"

namespace ores::connections::repository {

/**
 * @brief Repository for managing folders in the SQLite database.
 */
class folder_repository final {
public:
    explicit folder_repository(sqlite_context& ctx);

    /**
     * @brief Create or update a folder.
     */
    void write(const domain::folder& f);

    /**
     * @brief Create or update multiple folders.
     */
    void write(const std::vector<domain::folder>& folders);

    /**
     * @brief Read all folders.
     */
    std::vector<domain::folder> read_all();

    /**
     * @brief Read a folder by ID.
     */
    std::optional<domain::folder> read_by_id(const boost::uuids::uuid& id);

    /**
     * @brief Read folders by parent ID (null for root folders).
     */
    std::vector<domain::folder> read_by_parent(
        const std::optional<boost::uuids::uuid>& parent_id);

    /**
     * @brief Delete a folder by ID.
     */
    void remove(const boost::uuids::uuid& id);

private:
    sqlite_context& ctx_;
};

}

#endif
