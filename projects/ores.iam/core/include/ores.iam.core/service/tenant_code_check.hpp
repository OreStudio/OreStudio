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
#ifndef ORES_IAM_CORE_SERVICE_TENANT_CODE_CHECK_HPP
#define ORES_IAM_CORE_SERVICE_TENANT_CODE_CHECK_HPP

#include <cctype>
#include <cstddef>
#include <string>

namespace ores::iam::service {

/// The longest tenant code the deployment accepts.
inline constexpr std::size_t tenant_code_max_length = 50;

/**
 * @brief Why a tenant code is not one this deployment accepts, or nothing.
 *
 * A code reaches further than the tenant it names: a screen derives it from a
 * display name, a deployment serves the tenant on it, and it appears inside an
 * administrator's principal. So its shape is the deployment's to state rather
 * than a form's to invent, and every caller that creates a tenant is held to
 * the same one: a lowercase letter first, then lowercase letters, digits and
 * underscores, at most fifty characters.
 *
 * The answer is a sentence rather than an exception, because the caller is a
 * handler that has to tell a person why their request was refused.
 */
[[nodiscard]] inline std::string check_tenant_code(const std::string& code) {
    if (code.empty()) {
        return "A tenant code is required.";
    }
    if (code.size() > tenant_code_max_length) {
        return "A tenant code may hold at most " + std::to_string(tenant_code_max_length) +
               " characters.";
    }
    const auto first = static_cast<unsigned char>(code.front());
    if (std::islower(first) == 0) {
        return "A tenant code must start with a lowercase letter.";
    }
    for (const auto character : code) {
        const auto value = static_cast<unsigned char>(character);
        if (std::islower(value) == 0 && std::isdigit(value) == 0 && character != '_') {
            return "A tenant code may hold only lowercase letters, digits and underscores.";
        }
    }
    return {};
}

}

#endif
