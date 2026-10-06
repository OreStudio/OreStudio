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
#ifndef ORES_IAM_CORE_SERVICE_RUN_GRANT_OPERATIONS_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_RUN_GRANT_OPERATIONS_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/messaging/run_grant_operations_protocol.hpp"
#include "ores.iam.core/export.hpp"

namespace ores::iam::service {

/**
 * @brief Creates and revokes run grants: the only writes the table accepts.
 *
 * Each write makes a check no row write can express. A create needs a session
 * that acts for a party and carries the person's permissions, and the person
 * must hold every permission of the role. A revoke needs the grantor, or a
 * holder of iam::run_grants:revoke. The context is the request's, so the
 * tenant, the party and the person come from the token, never the request.
 */
class ORES_IAM_CORE_EXPORT run_grant_operations_service {
public:
    explicit run_grant_operations_service(ores::database::context ctx);

    /**
     * @brief Creates a grant, returns the active one, or re-activates a
     * revoked one for the same party, resource and grantor.
     */
    messaging::create_run_grant_response
    create_run_grant(const messaging::create_run_grant_request& request);

    /**
     * @brief Revokes a grant. Revoking a revoked grant succeeds and changes
     * nothing.
     */
    messaging::revoke_run_grant_response
    revoke_run_grant(const messaging::revoke_run_grant_request& request);

private:
    ores::database::context ctx_;
};

}

#endif
