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
#ifndef ORES_HTTP_ROUTES_IAM_PROFILE_ROUTES_HPP
#define ORES_HTTP_ROUTES_IAM_PROFILE_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http/routes/iam/access_screen_shapes.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>
#include <optional>
#include <string>

namespace ores::http::routes::iam {

/**
 * @brief The profile and contact screens' reads and writes, at the addresses
 * and in the shapes ores.web reads.
 *
 * A person's own profile and contact record, and an administrator's view of
 * one account's. Each forwards the caller's own token, so IAM's permission
 * check governs; the own routes name no account, so they cannot name another.
 */
class profile_routes final {
public:
    /**
     * @brief Register the routes.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);

    /**
     * @brief A profile write, or nothing when a field is not text.
     */
    [[nodiscard]] static std::optional<profile_body> read_profile(const std::string& body);

    /**
     * @brief A contact write, or nothing when a field is not text or the version
     * is not a count.
     */
    [[nodiscard]] static std::optional<contact_body> read_contact(const std::string& body);

    /**
     * @brief Whether a write claims a version the record has left: the screen
     * stated one, and it is not the version just read.
     */
    [[nodiscard]] static bool moved(int current, const std::optional<std::string>& claimed);
};

}

#endif
