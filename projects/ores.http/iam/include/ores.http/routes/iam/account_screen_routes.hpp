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
#ifndef ORES_HTTP_ROUTES_IAM_ACCOUNT_SCREEN_ROUTES_HPP
#define ORES_HTTP_ROUTES_IAM_ACCOUNT_SCREEN_ROUTES_HPP

#include "ores.assets.api/domain/image.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.iam.api/messaging/account_protocol.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>
#include <optional>

namespace ores::http::routes::iam {

/**
 * @brief The people screens' account reads and the default party, at the
 * addresses and in the shapes ores.web reads.
 *
 * A page of the tenant's accounts, one account by username, and the party
 * quick sign-in uses. Each forwards the caller's own token, so IAM's
 * permission check governs; one account named by the caller's own username is
 * read by the self read, which needs no account permission.
 */
class account_screen_routes final {
public:
    /**
     * @brief Register the routes.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);

    /**
     * @brief The account page a query names, or nothing when it is not one: an
     * offset, a limit of 1 to 1000, a search of at most 256 characters and an
     * order by username or full name.
     */
    [[nodiscard]] static std::optional<ores::iam::messaging::list_accounts_request>
    read_page_query(const ores::http::domain::http_request& req);

    /**
     * @brief A page of accounts, as the screen reads it.
     */
    [[nodiscard]] static std::string
    page_json(const ores::iam::messaging::list_accounts_response& reply);

    /**
     * @brief The reporting shape, as the screen reads it: an empty reference
     * reads as null.
     */
    [[nodiscard]] static std::string
    tree_json(const ores::iam::messaging::get_reporting_tree_response& reply);

    /**
     * @brief The parties the caller works in and the party quick sign-in uses,
     * as the member's own screen reads them.
     */
    [[nodiscard]] static std::string
    my_parties_json(const ores::iam::messaging::get_my_parties_response& reply);

    /**
     * @brief One picture as the browser shows it: its bytes, kept privately for
     * five minutes and never run as a document.
     */
    [[nodiscard]] static ores::http::domain::http_response
    picture_of(const ores::assets::domain::image& image);
};

}

#endif
