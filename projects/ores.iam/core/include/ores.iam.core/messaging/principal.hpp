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
#ifndef ORES_IAM_MESSAGING_PRINCIPAL_HPP
#define ORES_IAM_MESSAGING_PRINCIPAL_HPP

#include <string>

namespace ores::iam::messaging {

/**
 * @brief The two parts a principal is composed of.
 *
 * A principal is "username@hostname". The username is what an account row is
 * stored under and what a login looks up; the hostname routes the request to
 * the tenant that serves it. A principal with no '@' is a bare username and
 * routes nowhere.
 */
struct principal_parts {
    std::string username;
    std::string hostname;
    bool has_hostname = false;
};

/**
 * @brief Splits a principal into the username and the hostname.
 *
 * The rule lives here because it was once written out at every call site, and
 * the call sites that forgot it stored an account under "name@host" while the
 * login looked up "name", so no account was ever found. A caller that needs
 * only the username uses username_of(); one that routes by hostname reads the
 * hostname and checks has_hostname first.
 *
 * The split is on the last '@', so a username may itself contain one.
 */
[[nodiscard]] inline principal_parts split_principal(const std::string& principal) {
    const auto at_pos = principal.rfind('@');
    if (at_pos == std::string::npos)
        return principal_parts{principal, {}, false};
    return principal_parts{principal.substr(0, at_pos), principal.substr(at_pos + 1), true};
}

/**
 * @brief The username a principal names, whether or not it states a hostname.
 */
[[nodiscard]] inline std::string username_of(const std::string& principal) {
    return split_principal(principal).username;
}

}

#endif
