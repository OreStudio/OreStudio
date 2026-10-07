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
#ifndef ORES_IAM_API_MESSAGING_SIGNUP_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_SIGNUP_PROTOCOL_HPP

#include <string>

namespace ores::iam::messaging {

struct signup_request {
    using response_type = struct signup_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.signup";
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
    /**
     * @brief The address the registration arrived at.
     *
     * The address names the tenant the account lands in: the service resolves
     * it by hostname and uses the tenant flagged @c is_registration_default
     * only when the address names none. Without it a registration lands in the
     * system tenant by omission, which is the defect the survey found.
     */
    std::string hostname;
};

struct signup_response {
    bool success = false;
    std::string message;
    std::string account_id;
    /**
     * @brief The state the account was created in: @c active or @c pending.
     *
     * An account is active when its tenant nominated a default party, so it
     * received an association and can sign in at once. It is pending when the
     * tenant nominated none: the account exists, but it waits for an
     * administrator to finish setting it up.
     */
    std::string account_status;
    /**
     * @brief The party the account received, when the tenant nominated one.
     *
     * Empty when the account is pending, because a pending account has no
     * association yet.
     */
    std::string party_id;
    /**
     * @brief The role the account received: the tenant's nominated default.
     *
     * Empty when the tenant nominated no role.
     */
    std::string role_id;
    /**
     * @brief The stable code a client branches on.
     *
     * Empty when the registration succeeded. The message beside it is for a
     * person and may change; this is what a screen branches on, so a refusal is
     * a value rather than a sentence to match. The codes a registration can
     * answer with are collected on the Entry journeys page.
     */
    std::string error_code;
};

}

#endif
