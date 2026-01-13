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
#ifndef ORES_CONNECTIONS_REPOSITORY_TAG_MAPPER_HPP
#define ORES_CONNECTIONS_REPOSITORY_TAG_MAPPER_HPP

#include <vector>
#include "ores.connections/domain/tag.hpp"
#include "ores.connections/repository/tag_entity.hpp"

namespace ores::connections::repository {

class tag_mapper final {
public:
    static tag_entity to_entity(const domain::tag& t);
    static domain::tag to_domain(const tag_entity& e);
    static std::vector<tag_entity> to_entities(const std::vector<domain::tag>& tags);
    static std::vector<domain::tag> to_domain(const std::vector<tag_entity>& entities);
};

}

#endif
