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
#ifndef ORES_SECURITY_VALIDATION_PASSWORD_VALIDATOR_HPP
#define ORES_SECURITY_VALIDATION_PASSWORD_VALIDATOR_HPP

#include "ores.security/export.hpp"
#include "ores.security/validation/validation_result.hpp"
#include <string>

namespace ores::security::validation {

/**
 * @brief The rules a password must satisfy, as data.
 *
 * The rules are stated once, here, and the validator enforces exactly what it
 * states. A caller that shows the rules to a person asks for this record
 * rather than keeping a copy of them, so the rules on screen cannot drift from
 * the rules the server applies.
 */
struct ORES_SECURITY_EXPORT password_policy {
    /// The shortest password the policy accepts.
    std::size_t min_length = 0;
    bool require_uppercase = false;
    bool require_lowercase = false;
    bool require_digit = false;
    bool require_special = false;
    /// The symbols that satisfy the special-character rule.
    std::string special_chars;
};

/**
 * @brief Validates passwords against a security policy.
 *
 * The password_validator class enforces a strong password policy based
 * on OWASP recommendations. Passwords must meet minimum length and complexity
 * requirements including uppercase, lowercase, numeric, and special character
 * constraints.
 *
 * For TESTING/DEVELOPMENT environments, password validation can be disabled via
 * the enforce_policy parameter. This should NEVER be disabled in production
 * environments.
 */
class ORES_SECURITY_EXPORT password_validator {
public:
    /**
     * @brief The policy the validator enforces.
     *
     * The single statement of the rules: validate() applies this record and
     * nothing else, so a caller that reads it states what the server applies.
     */
    [[nodiscard]] static password_policy policy();
    /**
     * @brief Validates a password against the security policy.
     *
     * The password must meet the following requirements:
     * - Minimum 12 characters in length
     * - At least one uppercase letter (A-Z)
     * - At least one lowercase letter (a-z)
     * - At least one digit (0-9)
     * - At least one special symbol from: !@#$%^&*()_+-=[]{}|;:,.<>?
     *
     * @param password The plaintext password to validate.
     * @param enforce_policy If false, validation is skipped (for
     * testing/development only).
     * @return validation_result containing is_valid flag and error message if
     * invalid.
     */
    static validation_result validate(const std::string& password, bool enforce_policy = true);

};

}

#endif
