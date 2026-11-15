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
#ifndef ORES_HTTP_ROUTES_REPORTING_JSON_SHAPE_HPP
#define ORES_HTTP_ROUTES_REPORTING_JSON_SHAPE_HPP

#include "ores.http.api/net/record_routes.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::reporting::shape {

/**
 * @brief One member of a JSON object, its value already rendered.
 *
 * A page here is a dozen small objects, so the members are listed and joined
 * rather than built one string at a time.
 */
[[nodiscard]] inline std::string member(std::string_view name, std::string_view value) {
    return "\"" + std::string(name) + "\":" + std::string(value);
}

/// The members of one JSON object, joined.
[[nodiscard]] inline std::string object(const std::vector<std::string>& members) {
    return ores::http::net::record_routes::object_of(members);
}

}

#endif
