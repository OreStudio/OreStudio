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
#ifndef ORES_IAM_API_MESSAGING_REGISTRATION_POLICY_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_REGISTRATION_POLICY_PROTOCOL_HPP

#include <string>

namespace ores::iam::messaging {

struct registration_policy_request {
    using response_type = struct registration_policy_response;
    static constexpr std::string_view nats_subject = "iam.v1.auth.registration-policy";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
    /**
     * @brief The address the door was served at.
     *
     * The tenant is resolved from it. When it names no tenant, the tenant
     * flagged @c is_registration_default is used, and when the deployment
     * nominates no default either, the read refuses with
     * @c no_registration_destination rather than naming the system tenant.
     */
    std::string hostname;
};

struct registration_policy_response {
    bool success = false;
    std::string message;
    /**
     * @brief The stable code a client branches on.
     *
     * Empty when the read answered. @c signup_disabled and
     * @c signup_requires_authorization mean the deployment refuses the form;
     * @c no_registration_destination means it accepts registrations but has not
     * said where they land. @c no_default_role means it has not said what a new
     * account may do. Each names a different thing for an administrator to fix,
     * so a screen states the reason rather than one closed door.
     */
    std::string error_code;
    /**
     * @brief Whether the deployment accepts registrations at all.
     *
     * The setting @c system.user_signups, read at system scope: it is the
     * deployment's answer rather than a tenant's, because the person who opens
     * the door is not the person who decides what it opens onto.
     */
    bool signups_enabled = false;
    /**
     * @brief Whether a registration also waits on an approval step.
     *
     * The setting @c system.signup_requires_authorization. No approval workflow
     * exists yet, so the honest answer is that a deployment which sets this
     * refuses registrations and says why.
     */
    bool authorization_required = false;
    std::string tenant_id;
    std::string tenant_name;
    /**
     * @brief The party a new account joins, when the tenant nominates one.
     *
     * Empty when the tenant nominates none. A registration into a tenant with
     * no nominated party creates a pending account rather than failing.
     */
    std::string party_id;
    std::string party_name;
    /**
     * @brief The role a new account receives.
     *
     * Empty when the tenant nominates none, which closes registration: an
     * account that holds nothing is not an account somebody can use.
     */
    std::string role_id;
    std::string role_name;
    /**
     * @brief Whether a registration can sign in without an administrator.
     *
     * True when the destination names a tenant, a party and a role, because an
     * account needs all three to sign in. False means the account is created
     * pending and waits for an administrator to finish setting it up.
     */
    bool usable_now = false;
};

}

#endif
