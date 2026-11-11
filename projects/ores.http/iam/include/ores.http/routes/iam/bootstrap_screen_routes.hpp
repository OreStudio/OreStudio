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
#ifndef ORES_HTTP_ROUTES_IAM_BOOTSTRAP_SCREEN_ROUTES_HPP
#define ORES_HTTP_ROUTES_IAM_BOOTSTRAP_SCREEN_ROUTES_HPP

#include "ores.http.api/domain/session_cookie.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.iam.api/messaging/bootstrap_protocol.hpp"
#include "ores.iam.api/messaging/registration_policy_protocol.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.workflow.api/messaging/workflow_protocol.hpp"
#include <memory>
#include <optional>
#include <string>

namespace ores::http::routes::iam {

/**
 * @brief The setup screens' routes: what the deployment says about its own
 * setup, the first administrator, the end of first-run setup, a tenant's own
 * setup run, and what the deployment offers somebody not in it yet.
 *
 * The status, the first administrator and the policy answer with no session,
 * because a deployment in bootstrap mode has nobody to sign in as. The status
 * also reads two settings through the browser's session when it has one, and
 * says whether the browser sent a session cookie at all, so it needs the
 * deployment's cookie.
 */
class bootstrap_screen_routes final {
public:
    bootstrap_screen_routes(ores::nats::service::nats_client& session,
                            ores::http::domain::cookie_settings cookie);

    /**
     * @brief The host a request arrived at, without its port, as the BFF read it.
     */
    [[nodiscard]] static std::string hostname_of(const std::string& host);

    /**
     * @brief The setup state as the screens read it.
     */
    [[nodiscard]] static std::string
    status_json(const ores::iam::messaging::bootstrap_status_response& status,
                bool onboarding_complete,
                bool onboarding_tenant_complete,
                const std::string& account_id,
                bool session_present);

    /**
     * @brief The registration policy as the door reads it.
     */
    [[nodiscard]] static std::string
    policy_json(const ores::iam::messaging::registration_policy_response& policy);

    /**
     * @brief A tenant's own setup run, or an empty one when it has none.
     */
    [[nodiscard]] static std::string
    setup_run_json(const std::optional<ores::workflow::messaging::workflow_instance_summary>& run);

    /**
     * @brief Register the setup screens' routes.
     */
    void register_routes(std::shared_ptr<ores::http::net::router> router,
                         std::shared_ptr<ores::http::openapi::endpoint_registry> registry);

private:
    ores::nats::service::nats_client& session_;
    ores::http::domain::cookie_settings cookie_;
};

}

#endif
