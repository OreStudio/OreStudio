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
#include "ores.http/routes/iam/iam_routes.hpp"
#include "ores.http/routes/iam/account_credential_routes.hpp"
#include "ores.http/routes/iam/account_operations_routes.hpp"
#include "ores.http/routes/iam/account_routes.hpp"
#include "ores.http/routes/iam/authorization_routes.hpp"
#include "ores.http/routes/iam/bootstrap_routes.hpp"
#include "ores.http/routes/iam/login_info_routes.hpp"
#include "ores.http/routes/iam/login_routes.hpp"
#include "ores.http/routes/iam/permission_routes.hpp"
#include "ores.http/routes/iam/role_routes.hpp"
#include "ores.http/routes/iam/auth_event_operations_routes.hpp"
#include "ores.http/routes/iam/geo_operations_routes.hpp"
#include "ores.http/routes/iam/session_statistics_operations_routes.hpp"
#include "ores.http/routes/iam/session_operations_routes.hpp"
#include "ores.http/routes/iam/session_routes.hpp"
#include "ores.http/routes/iam/signup_routes.hpp"

namespace ores::http::routes::iam {

void iam_routes::register_routes(std::shared_ptr<ores::http::net::router> router,
                                 std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                 ores::nats::service::nats_client& session) {

    account_routes::register_routes(router, registry, session);
    account_credential_routes::register_routes(router, registry, session);
    role_routes::register_routes(router, registry, session);
    permission_routes::register_routes(router, registry, session);
    session_routes::register_routes(router, registry, session);
    login_info_routes::register_routes(router, registry, session);

    account_operations_routes::register_routes(router, registry, session);
    authorization_routes::register_routes(router, registry, session);
    login_routes::register_routes(router, registry, session);
    signup_routes::register_routes(router, registry, session);
    bootstrap_routes::register_routes(router, registry, session);
    auth_event_operations_routes::register_routes(router, registry, session);
    geo_operations_routes::register_routes(router, registry, session);
    session_statistics_operations_routes::register_routes(router, registry, session);
    session_operations_routes::register_routes(router, registry, session);
}

}
