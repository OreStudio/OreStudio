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
#ifndef ORES_HTTP_ROUTES_IAM_ACCESS_READ_ROUTES_HPP
#define ORES_HTTP_ROUTES_IAM_ACCESS_READ_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.iam.api/messaging/authorization_protocol.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>
#include <optional>
#include <string>

namespace ores::http::routes::iam {

/**
 * @brief The access screens' reads, at the addresses and in the shapes ores.web reads.
 *
 * Who holds which roles, what those roles let an account do, a page of the
 * tenant's roles and who holds one. Each read forwards the caller's own token, so
 * the IAM service's permission check governs, and each answer is the camelCase
 * shape the BFF gave.
 */
class access_read_routes final {
public:
    /**
     * @brief One permission page a screen asks for, as the query states it.
     */
    struct permission_query final {
        std::string area;
        std::string search;
        int offset = 0;
        int limit = 15;
    };

    /**
     * @brief A role page a screen asks for, as the query states it.
     */
    struct roles_query final {
        std::string role_id;
        std::string search;
        std::string area;
        bool include_service = false;
        int offset = 0;
        int limit = 15;
    };

    /**
     * @brief Register the access reads.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);

    /**
     * @brief The permission page a query names, or nothing when it is not one.
     */
    [[nodiscard]] static std::optional<permission_query>
    read_permission_query(const ores::http::domain::http_request& req);

    /**
     * @brief The role page a query names, or nothing when it is not one.
     */
    [[nodiscard]] static std::optional<roles_query>
    read_roles_query(const ores::http::domain::http_request& req);

    /**
     * @brief The roles an account holds, as the screen reads them.
     */
    [[nodiscard]] static std::string
    access_json(const ores::iam::messaging::get_account_roles_response& reply);

    /**
     * @brief One permission page, as the screen reads it.
     */
    [[nodiscard]] static std::string
    page_json(const ores::iam::messaging::permission_page_response& reply);

    /**
     * @brief One page of roles, as the screen reads it.
     */
    [[nodiscard]] static std::string
    roles_json(const ores::iam::messaging::role_page_response& reply);

    /**
     * @brief One page of a role's holders, as the screen reads it: a holder with
     * no picture states imageId as null.
     */
    [[nodiscard]] static std::string
    holders_json(const ores::iam::messaging::role_holders_response& reply);
};

}

#endif
