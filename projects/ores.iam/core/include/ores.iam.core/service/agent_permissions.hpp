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
#ifndef ORES_IAM_CORE_SERVICE_AGENT_PERMISSIONS_HPP
#define ORES_IAM_CORE_SERVICE_AGENT_PERMISSIONS_HPP

#include "ores.iam.core/export.hpp"
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Whether a permission code is a read: it ends in :read or :lookup.
 */
ORES_IAM_CORE_EXPORT bool is_read_permission(std::string_view code);

/**
 * @brief Whether a permission is withheld from every LLM agent, whatever its
 * role and whatever its person holds.
 *
 * Withheld are every installation code; every IAM code that is not a read, and
 * the reads of other people's agent grants and keys; every inbox code that is
 * not a read; every code that is not a read and names an approval, an
 * authorisation or a decision, in any component; and every code that
 * exchanges, mints, impersonates, enters as a service or provisions.
 */
ORES_IAM_CORE_EXPORT bool is_withheld_from_agents(std::string_view code);

/**
 * @brief The permissions an agent's token carries: every code of the catalogue
 * that the role covers and the person holds, less those withheld from agents.
 *
 * Wildcards in the role or the person's codes are expanded against the
 * catalogue, so the result is explicit codes and never a wildcard.
 */
ORES_IAM_CORE_EXPORT std::vector<std::string>
agent_permissions(std::span<const std::string> catalogue,
                  std::span<const std::string> role_codes,
                  std::span<const std::string> person_codes);

/**
 * @brief Why a role cannot be granted to an agent, or an empty string when it
 * can.
 *
 * A role can be granted when it names only explicit codes, none of which is
 * withheld from agents, and when the person holds every one of them.
 */
ORES_IAM_CORE_EXPORT std::string agent_role_refusal(std::span<const std::string> role_codes,
                                                    std::span<const std::string> person_codes);

}

#endif
