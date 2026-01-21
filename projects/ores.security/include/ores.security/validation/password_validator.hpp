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

#include <string>
#include "ores.security/validation/validation_result.hpp"

namespace ores::security::validation {

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
class password_validator {
public:
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
    static validation_result validate(const std::string& password,
                                      bool enforce_policy = true);

private:
    static constexpr std::size_t MIN_LENGTH = 12;
    static constexpr const char* SPECIAL_CHARS = "!@#$%^&*()_+-=[]{}|;:,.<>?";
};

}

#endif
