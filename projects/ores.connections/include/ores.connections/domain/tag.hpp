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
#ifndef ORES_CONNECTIONS_DOMAIN_TAG_HPP
#define ORES_CONNECTIONS_DOMAIN_TAG_HPP

#include <string>
#include <boost/uuid/uuid.hpp>

namespace ores::connections::domain {

/**
 * @brief Represents a tag for categorizing server connections.
 *
 * Tags provide a flexible way to categorize connections orthogonal to the
 * folder hierarchy. A connection can have multiple tags, and tags can be
 * used for filtering and searching.
 */
struct tag final {
    /**
     * @brief Unique identifier for this tag.
     */
    boost::uuids::uuid id;

    /**
     * @brief Display name of the tag.
     */
    std::string name;
};

}

#endif
