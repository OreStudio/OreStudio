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
#include "ores.accounts/security/email_validator.hpp"

#include <algorithm>

namespace ores::accounts::security {

email_validator::validation_result email_validator::
validate(const std::string& email) {
    validation_result result { .is_valid = true, .error_message = "" };

    if (email.empty()) {
        result.is_valid = false;
        result.error_message = "Email address cannot be empty";
        return result;
    }

    // Count '@' symbols - must be exactly one
    auto at_count = std::count(email.begin(), email.end(), '@');
    if (at_count == 0) {
        result.is_valid = false;
        result.error_message = "Email address must contain '@' symbol";
        return result;
    }

    if (at_count > 1) {
        result.is_valid = false;
        result.error_message = "Email address must contain only one '@' symbol";
        return result;
    }

    auto at_pos = email.find('@');

    // '@' cannot be at start
    if (at_pos == 0) {
        result.is_valid = false;
        result.error_message = "Email address cannot start with '@'";
        return result;
    }

    // '@' cannot be at end
    if (at_pos == email.length() - 1) {
        result.is_valid = false;
        result.error_message = "Email address cannot end with '@'";
        return result;
    }

    // Check domain part has at least one '.'
    auto domain = email.substr(at_pos + 1);
    auto dot_pos = domain.find('.');
    if (dot_pos == std::string::npos) {
        result.is_valid = false;
        result.error_message = "Email domain must contain a '.' (e.g., example.com)";
        return result;
    }

    // '.' cannot be at start or end of domain
    if (dot_pos == 0) {
        result.is_valid = false;
        result.error_message = "Email domain cannot start with '.'";
        return result;
    }

    if (domain.back() == '.') {
        result.is_valid = false;
        result.error_message = "Email domain cannot end with '.'";
        return result;
    }

    return result;
}

}
