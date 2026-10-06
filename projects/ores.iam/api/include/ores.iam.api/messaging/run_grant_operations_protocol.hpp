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
#ifndef ORES_IAM_API_MESSAGING_RUN_GRANT_OPERATIONS_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_RUN_GRANT_OPERATIONS_PROTOCOL_HPP

#include <string>

namespace ores::iam::messaging {

/**
 * @brief Records a person's consent that runs of one resource act for them.
 *
 * Sent on behalf of the person, with their token. The grant's tenant and
 * party are the session's, and its grantor is the session's account. Refused
 * when the session acts for no party, or when the person does not hold every
 * permission of the role. A second create for the same resource returns the
 * active grant, and re-activates a revoked one.
 */
struct create_run_grant_request {
    using response_type = struct create_run_grant_response;
    static constexpr std::string_view nats_subject = "iam.v1.run_grants.create";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief What the grant serves, as <component>.<entity>/<id>.
     */
    std::string resource;
    /**
     * @brief The name of the role a run token carries.
     */
    std::string role;
    /**
     * @brief The service names that may exchange the grant, comma-separated.
     */
    std::string audience;
    /**
     * @brief The number of runs the grant serves; zero for a standing grant.
     */
    int max_runs = 0;
    /**
     * @brief How long the grant serves runs; zero for a standing grant.
     */
    int valid_seconds = 0;
};

/**
 * @brief The grant, or why there is none.
 */
struct create_run_grant_response {
    bool success = false;
    std::string message;
    std::string grant_id;
    /**
     * @brief True when this request created or re-activated the grant; false
     * when it returned one that was already active.
     */
    bool created = false;
};

/**
 * @brief Ends a run grant.
 *
 * The grantor may revoke their own grant, and a holder of
 * iam::run_grants:revoke may revoke any grant of the tenant. Revoking a
 * revoked grant succeeds and changes nothing.
 */
struct revoke_run_grant_request {
    using response_type = struct revoke_run_grant_response;
    static constexpr std::string_view nats_subject = "iam.v1.run_grants.revoke";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string grant_id;
    /**
     * @brief Why the grant ends: unscheduled, deleted, party_changed, or a
     * person's reason.
     */
    std::string reason;
};

/**
 * @brief Whether the grant is now revoked.
 */
struct revoke_run_grant_response {
    bool success = false;
    std::string message;
};

}

#endif
