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
#ifndef ORES_CONNECTIONS_REPOSITORY_FOLDER_MAPPER_HPP
#define ORES_CONNECTIONS_REPOSITORY_FOLDER_MAPPER_HPP

#include <vector>
#include "ores.connections/domain/folder.hpp"
#include "ores.connections/repository/folder_entity.hpp"

namespace ores::connections::repository {

/**
 * @brief Maps between folder domain objects and database entities.
 */
class folder_mapper final {
public:
    static folder_entity to_entity(const domain::folder& f);
    static domain::folder to_domain(const folder_entity& e);
    static std::vector<folder_entity> to_entities(const std::vector<domain::folder>& folders);
    static std::vector<domain::folder> to_domain(const std::vector<folder_entity>& entities);
};

}

#endif
