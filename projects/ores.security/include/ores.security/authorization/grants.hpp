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
#ifndef ORES_SECURITY_AUTHORIZATION_GRANTS_HPP
#define ORES_SECURITY_AUTHORIZATION_GRANTS_HPP

#include <span>
#include <string>
#include <string_view>

namespace ores::security::authorization {

/**
 * @brief Whether a set of granted permission codes covers a required one.
 *
 * A grant covers a code when it is the code itself, the wildcard =*=, or a
 * component wildcard such as =refdata::*=, which covers every code that starts
 * with =refdata::=. This is the one rule every permission check uses, so the
 * token check and the database check cannot disagree about the same grant.
 */
inline bool grants(std::span<const std::string> granted, std::string_view required) {
    for (const auto& grant : granted) {
        if (grant == "*" || grant == required)
            return true;
        if (grant.size() > 1 && grant.ends_with("::*") &&
            required.starts_with(std::string_view(grant).substr(0, grant.size() - 1)))
            return true;
    }
    return false;
}

}

#endif
