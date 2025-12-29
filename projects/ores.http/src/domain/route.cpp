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
#include "ores.http/domain/route.hpp"

namespace ores::http::domain {

bool route::match(const std::string& path,
    std::unordered_map<std::string, std::string>& path_params) const {

    std::smatch matches;
    if (!std::regex_match(path, matches, regex)) {
        return false;
    }

    // Extract path parameters
    for (std::size_t i = 0; i < param_names.size() && i + 1 < matches.size(); ++i) {
        path_params[param_names[i]] = matches[i + 1].str();
    }

    return true;
}

}
