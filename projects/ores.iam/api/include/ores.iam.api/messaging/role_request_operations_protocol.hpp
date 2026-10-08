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
#ifndef ORES_IAM_API_MESSAGING_ROLE_REQUEST_OPERATIONS_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ROLE_REQUEST_OPERATIONS_PROTOCOL_HPP

#include "ores.iam.api/domain/role.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::messaging {

/**
 * @brief Asks for roles for the signed-in person.
 *
 * Refused when a role is unknown, is not one the tenant offers to its
 * members, is already held, or is already asked for in a request that still
 * waits.
 */
struct ask_for_roles_request {
    using response_type = struct ask_for_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.ask_for_roles";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The roles asked for, as UUID strings.
     */
    std::vector<std::string> role_ids;
    /**
     * @brief Why the person asks, in their words.
     */
    std::string reason;
};

struct ask_for_roles_response {
    ores::utility::domain::result result;
    /**
     * @brief The approval request raised, when the outcome is ok.
     */
    std::string request_id;
};

/**
 * @brief Reads the roles one approval request asks for.
 *
 * Answered when the caller raised the request, and when the caller may read
 * the roles of role grant requests. Answered as not found otherwise, so a
 * caller learns nothing about a request they may not read.
 */
struct get_request_roles_request {
    using response_type = struct get_request_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.get_request_roles";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The approval request to read the roles of, as a UUID string.
     */
    std::string request_id;
};

/**
 * @brief One role a request asks for, with the request's own record of it.
 *
 * The role is the catalogue row, whole, so a screen draws it without a second
 * read. The tail is the junction row the ask wrote: when it was written, and
 * when IAM applied it afterwards and on whose authority. It is what the
 * request's story needs to say that a role was given, and the only place that
 * fact is kept.
 */
struct requested_role {
    ores::iam::domain::role role;
    /**
     * @brief When the request recorded the role, which is when the person asked.
     */
    std::chrono::system_clock::time_point asked_at;
    /**
     * @brief When IAM applied the role after the request was approved, or empty
     * until it has. A role the person already held is marked applied without a
     * grant, so this says the request was dealt with rather than that a role moved.
     */
    std::optional<std::chrono::system_clock::time_point> applied_at;
    /**
     * @brief Who applied it, or empty until somebody has.
     */
    std::string applied_by;
};

struct get_request_roles_response {
    ores::utility::domain::result result;
    std::vector<requested_role> roles;
};

}

#endif
