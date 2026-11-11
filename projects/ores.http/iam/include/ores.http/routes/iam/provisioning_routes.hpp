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
#ifndef ORES_HTTP_ROUTES_IAM_PROVISIONING_ROUTES_HPP
#define ORES_HTTP_ROUTES_IAM_PROVISIONING_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.iam.api/domain/seed_profile.hpp"
#include "ores.iam.api/domain/seed_profile_parameter.hpp"
#include "ores.iam.api/domain/seed_profile_step.hpp"
#include "ores.iam.api/messaging/tenant_provisioning_protocol.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.refdata.api/domain/party.hpp"
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace ores::http::routes::iam {

/**
 * @brief The routes that start a tenant or a party: the starting points a
 * tenant is provisioned from, and the two journeys that provision.
 *
 * Each journey answers the run it started rather than waiting for it, because
 * the steps it orders take minutes. A refusal the service states is an answer
 * with success false, as the BFF sent it, and not an error.
 */
class provisioning_routes {
public:
    /**
     * @brief One starting point and the two child reads that complete it.
     */
    struct seed_profile_reads final {
        ores::iam::domain::seed_profile profile;
        std::vector<ores::iam::domain::seed_profile_step> steps;
        std::vector<ores::iam::domain::seed_profile_parameter> parameters;
    };

    /**
     * @brief The starting points as the journey's cards read them, ordered by
     * the order each declares and then by code.
     *
     * @throws std::runtime_error when a JSON list column is not a list of texts.
     */
    [[nodiscard]] static std::string profiles_json(std::vector<seed_profile_reads> reads);

    /**
     * @brief A JSON list column read as its texts. An empty column is no texts,
     * because a null jsonb column reaches the wire as an empty string.
     */
    [[nodiscard]] static std::optional<std::vector<std::string>>
    texts_of(const std::string& column);

    /**
     * @brief The provision command a browser's body states, or nothing when a
     * required field is missing or empty.
     *
     * Each parameter travels as one name=value entry, which is the command's
     * form.
     */
    [[nodiscard]] static std::optional<ores::iam::messaging::provision_tenant_command>
    read_tenant_command(const std::string& body);

    /**
     * @brief The party a new party is placed under: the one whose parent is
     * unset and that is not the system party, or nothing when the tenant has
     * none yet.
     */
    [[nodiscard]] static std::optional<boost::uuids::uuid>
    root_of(const std::vector<ores::refdata::domain::party>& parties);

    /**
     * @brief Register the starting points read and the two provisioning journeys.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
