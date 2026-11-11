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
#ifndef ORES_HTTP_ROUTES_IAM_TENANT_INSIDE_ROUTES_HPP
#define ORES_HTTP_ROUTES_IAM_TENANT_INSIDE_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <functional>
#include <memory>
#include <string>
#include <string_view>

namespace ores::http::routes::iam {

/**
 * @brief One tenant's data read from system administration, behind the
 * tenant's screen: its parties, its people, one account's sign-ins, and its
 * pictures and flags.
 *
 * The read runs inside the tenant for as long as it lasts and no longer. The
 * caller's own token is untouched, so any other request is answered as system
 * administration. The caller's own tenant is read as the caller, because there
 * is nothing to enter.
 */
class tenant_inside_routes {
private:
    inline static std::string_view logger_name = "ores.http.routes.iam.tenant_inside_routes";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief This request, carrying the token of a tenant it entered: a forward
     * made with it is answered as that tenant.
     *
     * The claims say what the token says, so a reader of the copy sees the
     * entered tenant, and the tenant the caller acts from.
     */
    [[nodiscard]] static ores::http::domain::http_request
    as_token(const ores::http::domain::http_request& req,
             const std::string& token,
             const std::string& tenant_id);

    /**
     * @brief Run a read inside the tenant the address names.
     *
     * The caller must hold the deployment read. Only a refused entry is a
     * refusal, forbidden; a read that fails inside the tenant fails as itself,
     * so the browser is not told it may not look when the server failed to
     * answer. The exit is recorded when it can be, and one that cannot be is
     * logged: the tenant session then ends at its own expiry.
     *
     * @throws record_routes::refusal, not found, when no tenant has the code.
     */
    static ores::http::domain::http_response
    read_inside(const ores::http::domain::http_request& req,
                ores::nats::service::nats_client& session,
                const std::function<ores::http::domain::http_response(
                    const ores::http::domain::http_request&)>& read);

    /**
     * @brief Register the reads inside a tenant.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
