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
#ifndef ORES_HTTP_ROUTES_IAM_TENANCY_CATALOGUE_ROUTES_HPP
#define ORES_HTTP_ROUTES_IAM_TENANCY_CATALOGUE_ROUTES_HPP

#include "ores.dq.api/domain/badge_definition.hpp"
#include "ores.dq.api/messaging/lei_entity_summary_protocol.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.iam.api/domain/tenant_status.hpp"
#include "ores.iam.api/domain/tenant_type.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>
#include <string>
#include <vector>

namespace ores::http::routes::iam {

/**
 * @brief The tenancy screens' catalogues, at the addresses and in the shapes
 * ores.web reads.
 *
 * The tenant statuses and types, each with the badge it is painted with, and
 * the legal entities a tenant may start from. Each forwards the caller's own
 * token, so the owning service's permission check governs.
 */
class tenancy_catalogue_routes final {
public:
    /**
     * @brief Register the routes.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);

    /**
     * @brief The tenant statuses with their badges, in the service's order.
     */
    [[nodiscard]] static std::string
    statuses_json(const std::vector<ores::iam::domain::tenant_status>& statuses,
                  const std::vector<ores::dq::domain::badge_definition>& badges);

    /**
     * @brief The tenant types with their badges, in their display order.
     */
    [[nodiscard]] static std::string
    types_json(std::vector<ores::iam::domain::tenant_type> types,
               const std::vector<ores::dq::domain::badge_definition>& badges);

    /**
     * @brief The legal entities a search found, as the screen reads them.
     */
    [[nodiscard]] static std::string
    entities_json(const ores::dq::messaging::search_lei_entities_response& reply);
};

}

#endif
