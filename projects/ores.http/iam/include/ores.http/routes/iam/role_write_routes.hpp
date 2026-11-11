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
#ifndef ORES_HTTP_ROUTES_IAM_ROLE_WRITE_ROUTES_HPP
#define ORES_HTTP_ROUTES_IAM_ROLE_WRITE_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http/routes/iam/access_screen_shapes.hpp"
#include "ores.iam.api/domain/permission.hpp"
#include "ores.iam.api/domain/role.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace ores::http::routes::iam {

/**
 * @brief The access screens' role and lock writes, and the catalogues they pick
 * from, at the addresses and in the shapes ores.web reads.
 *
 * A role given to an account or taken away, an account locked or unlocked, a
 * role made, edited or removed, and the permissions a role grants written whole
 * or changed by what to add and remove. Each forwards the caller's own token,
 * so IAM's permission check governs, and states a refusal the way the BFF did.
 */
class role_write_routes final {
public:
    /**
     * @brief One role of the tenant's catalogue with the permissions it grants.
     */
    struct role_summary final {
        ores::iam::domain::role role;
        std::vector<std::string> permission_codes;
    };

    /**
     * @brief Register the routes.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);

    /**
     * @brief A role to give, or nothing when the body is not one: a role
     * identifier, a reason of 1 to 100 characters and a note of at most 2000.
     */
    [[nodiscard]] static std::optional<give_role_body> read_give_role(const std::string& body);

    /**
     * @brief A role to make, or nothing when it is not one: a name of 1 to 100
     * characters once trimmed and a description of at most 1000.
     */
    [[nodiscard]] static std::optional<role_body> read_new_role(const std::string& body);

    /**
     * @brief A role to edit, or nothing when it is not one: a new role's fields
     * and the version it was read at and its two flags.
     */
    [[nodiscard]] static std::optional<role_body> read_edited_role(const std::string& body);

    /**
     * @brief The permissions a role grants, or nothing when they are not stated:
     * at most 2000 codes of 1 to 200 characters and a note of at most 2000.
     */
    [[nodiscard]] static std::optional<permission_bundle_body> read_bundle(const std::string& body);

    /**
     * @brief The permissions to add and remove, or nothing when they are not
     * stated: at most 2000 codes each and a note of 1 to 2000 characters.
     */
    [[nodiscard]] static std::optional<permission_changes_body>
    read_changes(const std::string& body);

    /**
     * @brief What a role grants once the changes apply: the held codes without
     * those removed, then each added code it did not hold.
     */
    [[nodiscard]] static std::vector<std::string>
    apply_changes(std::vector<std::string> held, const permission_changes_body& changes);

    /**
     * @brief The tenant's roles as the screen reads them.
     */
    [[nodiscard]] static std::string roles_json(const std::vector<role_summary>& roles);

    /**
     * @brief The permission catalogue as the screen reads it, in code order.
     */
    [[nodiscard]] static std::string
    permissions_json(std::vector<ores::iam::domain::permission> permissions);
};

}

#endif
