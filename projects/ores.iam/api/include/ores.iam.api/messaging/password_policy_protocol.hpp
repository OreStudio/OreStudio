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
#ifndef ORES_IAM_API_MESSAGING_PASSWORD_POLICY_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_PASSWORD_POLICY_PROTOCOL_HPP

#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <string>
#include <string_view>

namespace ores::iam::messaging {

/**
 * @brief Asks for the rules the server enforces on a password.
 *
 * The screen that shows the rules is the sign-in screen and the first-run
 * screen, and neither has a session yet, so this request needs none. The answer
 * is the validator's own policy: one statement of the rules, which the server
 * enforces and a client states.
 */
struct get_password_policy_request {
    using response_type = struct get_password_policy_response;
    static constexpr std::string_view nats_subject = "iam.v1.auth.password-policy";

    /**
     * @brief Whether the caller must have established a session first.
     */
    static constexpr bool requires_session = false;
};

struct get_password_policy_response {
    bool success = false;
    std::string message;

    /// The shortest password the policy accepts.
    int min_length = 0;
    bool require_uppercase = false;
    bool require_lowercase = false;
    bool require_digit = false;
    bool require_special = false;
    /// The symbols that satisfy the special-character rule.
    std::string special_chars;
};

}

#endif
