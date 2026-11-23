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
#ifndef ORES_SECURITY_AUTHORIZATION_AGENT_WITHHOLDING_HPP
#define ORES_SECURITY_AUTHORIZATION_AGENT_WITHHOLDING_HPP

#include <array>
#include <string_view>

namespace ores::security::authorization {

/**
 * @brief Whether a permission code is a read: it ends in :read or :lookup.
 */
inline bool is_read_permission(std::string_view code) {
    return code.ends_with(":read") || code.ends_with(":lookup");
}

/**
 * @brief Whether a permission is withheld from every LLM agent, whatever its
 * role and whatever its person holds.
 *
 * IAM removes these from every agent's token, and every service refuses them to
 * a token whose act names an agent, so the rule holds twice: once where the
 * token is made, and again where the permission is checked. The rules are
 * stated in the withholding note of the agents cluster.
 */
inline bool is_withheld_from_agents(std::string_view code) {
    if (code.starts_with("installation::"))
        return true;
    if (code == "iam::agent_grants:read" || code == "iam::agent_keys:read")
        return true;
    if (is_read_permission(code))
        return false;
    // Identity and access, the approval queue, and the system's own
    // configuration are administered by people.
    if (code.starts_with("iam::") || code.starts_with("inbox::") ||
        code.starts_with("variability::"))
        return true;
    static constexpr std::array<std::string_view, 6> decisions = {
        "approval", "authorisation", "authorization", "decision", ":decide", "authorise"};
    for (const auto word : decisions)
        if (code.find(word) != std::string_view::npos)
            return true;
    static constexpr std::array<std::string_view, 5> identities = {
        ":exchange", ":mint", ":impersonate", ":enter_as_service", ":provision"};
    for (const auto verb : identities)
        if (code.ends_with(verb))
            return true;
    // A checker's review, a person's acceptance of terms, and a write made as
    // a service are never an agent's.
    if (code.ends_with(":review") || code.ends_with(":accept_terms") ||
        code.ends_with("_service_origin"))
        return true;
    return false;
}

}

#endif
