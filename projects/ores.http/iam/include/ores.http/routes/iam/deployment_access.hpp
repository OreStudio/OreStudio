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
#ifndef ORES_HTTP_ROUTES_IAM_DEPLOYMENT_ACCESS_HPP
#define ORES_HTTP_ROUTES_IAM_DEPLOYMENT_ACCESS_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <span>
#include <string>
#include <string_view>

namespace ores::http::routes::iam {

/**
 * @brief Whether a caller acts on the deployment as a whole.
 *
 * The deployment's tenants, services, grid, bus and logs are read across every
 * tenant. A caller acts on the deployment by holding the deployment read. A
 * tenant's administrator holds the wildcard, and the wildcard does not cover an
 * installation code, so only an account given the read, or the installation
 * component's wildcard, passes. The roles are read on each call, so a grant the
 * caller lost stops the next read and not the next sign-in.
 */
class deployment_access {
public:
    /**
     * @brief The permission that says a caller acts on the deployment, as the
     * browser and the BFF name it.
     */
    static constexpr std::string_view deployment_read = "installation::deployment:read";

    /**
     * @brief Whether the codes granted cover the deployment read.
     */
    [[nodiscard]] static bool covers(std::span<const std::string> granted);

    /**
     * @brief Refuse a caller who does not hold the deployment read.
     *
     * @param what The words that complete the refusal: what the caller tried
     * to read.
     * @throws record_routes::refusal, forbidden, naming what was refused.
     */
    static void require_read(const ores::http::domain::http_request& req,
                             ores::nats::service::nats_client& session,
                             std::string_view what);
};

}

#endif
