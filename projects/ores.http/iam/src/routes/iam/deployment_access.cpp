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
#include "ores.http/routes/iam/deployment_access.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.iam.api/messaging/authorization_protocol.hpp"
#include "ores.security/authorization/grants.hpp"
#include <vector>

namespace ores::http::routes::iam {

namespace rr = ores::http::net::record_routes;

bool deployment_access::covers(std::span<const std::string> granted) {
    return ores::security::authorization::grants(granted, deployment_read);
}

void deployment_access::require_read(const ores::http::domain::http_request& req,
                                     ores::nats::service::nats_client& session,
                                     std::string_view what) {
    const auto reply =
        ores::http::forwarding::forward(req, session, ores::iam::messaging::get_my_roles_request{});
    rr::require_done(reply.result);
    std::vector<std::string> granted;
    for (const auto& held : reply.roles)
        granted.insert(granted.end(), held.permission_codes.begin(), held.permission_codes.end());
    if (!covers(granted))
        throw rr::refusal{ores::http::domain::http_status::forbidden,
                          "forbidden",
                          std::string(what) + " needs the read of the deployment as a whole."};
}

}
