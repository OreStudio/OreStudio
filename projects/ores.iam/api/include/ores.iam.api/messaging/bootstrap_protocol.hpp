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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_IAM_MESSAGING_BOOTSTRAP_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_BOOTSTRAP_PROTOCOL_HPP

#include <string>

namespace ores::iam::messaging {

struct bootstrap_status_request {
    using response_type = struct bootstrap_status_response;
    static constexpr std::string_view nats_subject = "iam.v1.bootstrap.status";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
};

struct bootstrap_status_response {
    bool is_in_bootstrap_mode = false;
    std::string message;
};

struct create_initial_admin_request {
    using response_type = struct create_initial_admin_response;
    static constexpr std::string_view nats_subject = "iam.v1.bootstrap.create-admin";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
    std::string principal;
    std::string password;
    std::string email;
};

struct create_initial_admin_response {
    bool success = false;
    std::string error_message;
    std::string account_id;
    std::string tenant_name;
    std::string tenant_id;
};

struct provision_tenant_request {
    using response_type = struct provision_tenant_response;
    static constexpr std::string_view nats_subject = "iam.v1.bootstrap.provision-tenant";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
    /*
     * tenant type (e.g., "corporate")
     */
    std::string type;
    /*
     * unique tenant code
     */
    std::string code;
    /*
     * display name
     */
    std::string name;
    /*
     * unique hostname
     */
    std::string hostname;
    /*
     * optional description
     */
    std::string description;
    /*
     * username for the admin account
     */
    std::string principal;
    std::string password;
    std::string email;
};

struct provision_tenant_response {
    bool success = false;
    std::string error_message;
    std::string account_id;
    std::string tenant_id;
};

}

#endif
