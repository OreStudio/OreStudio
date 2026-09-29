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
#include "ores.security/validation/password_validator.hpp"
#include <cctype>
#include <cstring>
#include <format>

namespace ores::security::validation {

password_policy password_validator::policy() {
    return password_policy{.min_length = 12,
                           .require_uppercase = true,
                           .require_lowercase = true,
                           .require_digit = true,
                           .require_special = true,
                           .special_chars = "!@#$%^&*()_+-=[]{}|;:,.<>?"};
}

validation_result password_validator::validate(const std::string& password, bool enforce_policy) {
    validation_result result{.is_valid = true, .error_message = ""};

    /*
     * If policy enforcement is disabled - for testing or development for
     * example - skip validation.
     */
    if (!enforce_policy)
        return result;

    const auto rules = policy();

    if (password.length() < rules.min_length) {
        result.is_valid = false;
        result.error_message =
            std::format("Password must be at least {} characters long", rules.min_length);
        return result;
    }

    bool has_uppercase = false;
    bool has_lowercase = false;
    bool has_digit = false;
    bool has_special = false;

    for (char c : password) {
        if (std::isupper(c))
            has_uppercase = true;
        if (std::islower(c))
            has_lowercase = true;
        if (std::isdigit(c))
            has_digit = true;
        if (std::strchr(rules.special_chars.c_str(), c))
            has_special = true;
    }

    if (rules.require_uppercase && !has_uppercase) {
        result.is_valid = false;
        result.error_message = "Password must contain at least one uppercase letter (A-Z)";
        return result;
    }

    if (rules.require_lowercase && !has_lowercase) {
        result.is_valid = false;
        result.error_message = "Password must contain at least one lowercase letter (a-z)";
        return result;
    }

    if (rules.require_digit && !has_digit) {
        result.is_valid = false;
        result.error_message = "Password must contain at least one digit (0-9)";
        return result;
    }

    if (rules.require_special && !has_special) {
        result.is_valid = false;
        result.error_message = std::format(
            "Password must contain at least one special character ({})", rules.special_chars);
        return result;
    }

    return result;
}

}
