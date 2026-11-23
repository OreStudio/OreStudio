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
#include "ores.iam.core/service/agent_permissions.hpp"
#include "ores.security/authorization/agent_withholding.hpp"
#include "ores.security/authorization/grants.hpp"
#include <array>

namespace ores::iam::service {

namespace {

bool is_wildcard(std::string_view code) {
    return code == "*" || code.ends_with("::*");
}

}

bool is_read_permission(std::string_view code) {
    return security::authorization::is_read_permission(code);
}

bool is_withheld_from_agents(std::string_view code) {
    return security::authorization::is_withheld_from_agents(code);
}

std::vector<std::string> agent_permissions(std::span<const std::string> catalogue,
                                           std::span<const std::string> role_codes,
                                           std::span<const std::string> person_codes) {
    std::vector<std::string> out;
    for (const auto& code : catalogue) {
        if (is_wildcard(code) || is_withheld_from_agents(code))
            continue;
        if (security::authorization::grants(role_codes, code) &&
            security::authorization::grants(person_codes, code))
            out.push_back(code);
    }
    return out;
}

std::string agent_role_refusal(std::span<const std::string> role_codes,
                               std::span<const std::string> person_codes) {
    for (const auto& code : role_codes) {
        if (is_wildcard(code))
            return "The role holds the wildcard " + code +
                   "; a role for an agent names explicit codes.";
        if (is_withheld_from_agents(code))
            return "The role holds " + code + ", which no agent may hold.";
        if (!security::authorization::grants(person_codes, code))
            return "You do not hold " + code + ", so you cannot delegate it.";
    }
    return {};
}

}
