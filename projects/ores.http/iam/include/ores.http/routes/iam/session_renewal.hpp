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
#ifndef ORES_HTTP_ROUTES_IAM_SESSION_RENEWAL_HPP
#define ORES_HTTP_ROUTES_IAM_SESSION_RENEWAL_HPP

#include "ores.http.api/domain/session_store.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <functional>
#include <memory>
#include <string>

namespace ores::http::routes::iam {

/**
 * @brief Renews a browser session's token through IAM, as the BFF's client did.
 *
 * IAM reads the token it renews from the Authorization header, so the request
 * carries the session's token there rather than as a delegation. A refusal, in
 * the X-Error header or the answer, ends the session; a request that fails or a
 * token that does not verify is a renewal that cannot be made now.
 */
class session_renewal final {
public:
    using verifier_finder =
        std::function<std::shared_ptr<ores::security::jwt::jwt_authenticator>()>;

    /**
     * @brief The renewer the gateway's session path calls.
     */
    [[nodiscard]] static ores::http::domain::token_renewer
    renewer(ores::nats::service::nats_client& session, verifier_finder find_verifier);

    /**
     * @brief What IAM's answer comes to, given its X-Error header, its answer
     * and the claims the new token states when it verifies.
     */
    [[nodiscard]] static ores::http::domain::token_renewal
    outcome_of(const std::string& x_error,
               bool success,
               const std::string& token,
               std::optional<ores::security::jwt::jwt_claims> claims);
};

}

#endif
