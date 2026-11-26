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
#ifndef ORES_HTTP_ROUTES_VAULT_VAULT_ROUTES_HPP
#define ORES_HTTP_ROUTES_VAULT_VAULT_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <boost/asio/awaitable.hpp>
#include <memory>
#include <vector>

namespace ores::http::routes::vault {

/**
 * @brief The two routes a person uses against the vault.
 *
 * Neither is generated, and neither forwards a body unchanged. A write seals
 * the value to the vault's transport key before it leaves this process, so the
 * bus carries ciphertext and the person's plaintext never crosses it. A reveal
 * resolves as this process and opens the answer with this process's own key,
 * so the vault seals to a consumer that holds a key rather than to a person who
 * does not.
 */
class vault_routes {
private:
    inline static std::string_view logger_name = "ores.http.routes.vault.vault_routes";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /// The key pair this process holds as a consumer of the vault.
    struct gateway_keys {
        std::vector<unsigned char> private_key;
        std::vector<unsigned char> public_key;
    };

    /**
     * @brief Register the vault routes.
     *
     * @param router The gateway's router.
     * @param registry The OpenAPI registry the routes are published in.
     * @param session The client every route forwards on.
     * @param keys The gateway's own key pair, which the reveal opens with.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session,
                                std::shared_ptr<const gateway_keys> keys);

    /**
     * @brief Reads the gateway's key pair from the unit's credentials.
     *
     * @return The pair, with the public half derived from the private one.
     *
     * @throws std::runtime_error When the unit holds no such credential, which
     * fails the gateway's start rather than leaving a route that cannot answer.
     */
    [[nodiscard]] static std::shared_ptr<const gateway_keys> load_keys();

    /// POST /api/v1/vault/secrets — store a value, sealed in this process.
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_write(const ores::http::domain::http_request& req,
                 ores::nats::service::nats_client& session,
                 const gateway_keys& keys);

    /// GET /api/v1/vault/secrets/{name}/value — reveal a value, over the session.
    static boost::asio::awaitable<ores::http::domain::http_response>
    handle_reveal(const ores::http::domain::http_request& req,
                  ores::nats::service::nats_client& session,
                  const gateway_keys& keys);
};

}

#endif
