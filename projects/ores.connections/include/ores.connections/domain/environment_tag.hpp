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
#ifndef ORES_CONNECTIONS_DOMAIN_ENVIRONMENT_TAG_HPP
#define ORES_CONNECTIONS_DOMAIN_ENVIRONMENT_TAG_HPP

#include <boost/uuid/uuid.hpp>

namespace ores::connections::domain {

/**
 * @brief Junction type for the many-to-many relationship between environments and tags.
 *
 * This type represents the association between a server_environment and a tag.
 * An environment can have multiple tags, and a tag can be applied to multiple
 * environments.
 */
struct environment_tag final {
    /**
     * @brief The environment being tagged.
     */
    boost::uuids::uuid environment_id;

    /**
     * @brief The tag being applied.
     */
    boost::uuids::uuid tag_id;
};

}

#endif
