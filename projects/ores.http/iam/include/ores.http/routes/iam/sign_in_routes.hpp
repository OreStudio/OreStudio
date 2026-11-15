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
#ifndef ORES_HTTP_ROUTES_IAM_SIGN_IN_ROUTES_HPP
#define ORES_HTTP_ROUTES_IAM_SIGN_IN_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <utility>

namespace ores::http::routes::iam {

/**
 * @brief The sign-in records and the sessions, at the addresses and in the
 * shapes ores.web reads.
 *
 * A page of the tenant's sign-in records and one account's, a page of the
 * tenant's sessions, the open ones, and the caller's own. Each forwards the
 * caller's own token, so IAM's permission check governs; the caller's own
 * record and sessions are self reads, which need no permission.
 */
class sign_in_routes final {
public:
    /**
     * @brief Register the routes.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);

    /**
     * @brief The offset and limit a page names, or nothing when they are not
     * whole numbers: an offset of zero or more and a limit of 1 to 1000.
     */
    [[nodiscard]] static std::optional<std::pair<std::uint32_t, std::uint32_t>>
    read_page(const ores::http::domain::http_request& req);

    /**
     * @brief The page of an account's sign-ins a query names, or nothing when
     * it is not one: an offset of zero or more and a limit of 1 to 200, 20 when
     * left out.
     */
    [[nodiscard]] static std::optional<std::pair<std::uint32_t, std::uint32_t>>
    read_sign_ins_page(const ores::http::domain::http_request& req);

    /**
     * @brief One account with its sign-in state and one page of its sessions,
     * newest first, read with the request's token, or nothing when no account
     * has the username.
     *
     * The token decides the tenant, so a read inside another tenant passes a
     * request that carries that tenant's token.
     */
    [[nodiscard]] static std::optional<std::string>
    sign_ins_json(const ores::http::domain::http_request& req,
                  ores::nats::service::nats_client& session,
                  const std::string& username,
                  std::pair<std::uint32_t, std::uint32_t> page);
};

}

#endif
