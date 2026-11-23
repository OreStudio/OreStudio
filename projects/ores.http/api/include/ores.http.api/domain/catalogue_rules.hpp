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
#ifndef ORES_HTTP_API_DOMAIN_CATALOGUE_RULES_HPP
#define ORES_HTTP_API_DOMAIN_CATALOGUE_RULES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/route.hpp"
#include "ores.http.api/export.hpp"
#include <string>
#include <string_view>

namespace ores::http::domain {

/**
 * @brief The published area a component's routes belong to, or the component
 * itself when the table names none.
 */
ORES_HTTP_API_EXPORT std::string area_of_component(std::string_view component);

/**
 * @brief Whether an operation only reads, by its name: one that lists, gets,
 * reads, finds, searches or looks up changes nothing, unless it also makes, as
 * get_or_create does.
 */
ORES_HTTP_API_EXPORT bool operation_only_reads(std::string_view name);

/**
 * @brief The catalogue place of a generated entity route: the published area
 * its path names, the entity, a tool named for what it does to the entity, and
 * whether it writes, which a GET or a bulk get never does.
 */
ORES_HTTP_API_EXPORT route_catalogue entity_route_catalogue(http_method method,
                                                            std::string_view pattern,
                                                            std::string_view component,
                                                            std::string_view entity,
                                                            std::string_view identifier);

/**
 * @brief The catalogue place of a generated operation route: the component's
 * area, the operation model's entity, the operation's own name as the tool, and
 * whether it writes, by that name.
 */
ORES_HTTP_API_EXPORT route_catalogue operation_route_catalogue(std::string_view component,
                                                               std::string_view entity,
                                                               std::string_view name);

}

#endif
