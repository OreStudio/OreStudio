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

#ifndef ORES_IAM_SECURITY_EMAIL_VALIDATOR_HPP
#define ORES_IAM_SECURITY_EMAIL_VALIDATOR_HPP

#include <string>

namespace ores::iam::security {

/**
 * @brief Validates email addresses against basic format requirements.
 *
 * The email_validator class performs basic validation of email addresses.
 * This is not a full RFC 5322 compliant validator, but checks for common
 * formatting issues.
 *
 * Current checks:
 * - Email is not empty
 * - Contains exactly one '@' symbol
 * - '@' is not at the start or end
 * - Domain part contains at least one '.'
 * - Local and domain parts are not empty
 */
class email_validator {
public:
    /**
     * @brief Result of email validation.
     */
    struct validation_result {
        bool is_valid;
        std::string error_message;
    };

    /**
     * @brief Validates an email address against basic format requirements.
     *
     * @param email The email address to validate.
     * @return validation_result containing is_valid flag and error message if
     * invalid.
     */
    static validation_result validate(const std::string& email);
};

}

#endif
