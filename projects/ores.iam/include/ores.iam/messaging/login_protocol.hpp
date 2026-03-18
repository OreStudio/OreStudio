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
#ifndef ORES_IAM_MESSAGING_LOGIN_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_LOGIN_PROTOCOL_HPP

#include <string>
#include <vector>

namespace ores::iam::messaging {

struct party_summary {
    std::string id;
    std::string name;
    std::string party_category;
};

struct login_request {
    using response_type = struct login_response;
    static constexpr std::string_view nats_subject = "iam.v1.auth.login";
    std::string principal;
    std::string password;
};

struct login_response {
    bool success = false;
    std::string account_id;
    std::string tenant_id;
    std::string tenant_name;
    std::string username;
    std::string email;
    bool password_reset_required = false;
    bool tenant_bootstrap_mode = false;
    std::string token;
    std::string error_message;
    std::string message;
    std::string selected_party_id;
    std::vector<party_summary> available_parties;
};

struct logout_request {
    using response_type = struct logout_response;
    static constexpr std::string_view nats_subject = "iam.v1.auth.logout";
};

struct logout_response {
    bool success = false;
    std::string message;
};

struct public_key_request {
    static constexpr std::string_view nats_subject = "iam.v1.auth.public-key";
};

}

#endif
