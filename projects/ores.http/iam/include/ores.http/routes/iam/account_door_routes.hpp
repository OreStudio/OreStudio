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
#ifndef ORES_HTTP_ROUTES_IAM_ACCOUNT_DOOR_ROUTES_HPP
#define ORES_HTTP_ROUTES_IAM_ACCOUNT_DOOR_ROUTES_HPP

#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.iam.api/messaging/password_policy_protocol.hpp"
#include "ores.iam.api/messaging/signup_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>
#include <string>
#include <string_view>

namespace ores::http::routes::iam {

/**
 * @brief The routes that make or change a credential: a registration, the rules
 * a password must keep, and the signed-in account's own password change.
 *
 * A registration and the rules answer somebody with no session. A registration
 * shares the sign-in budget, because both are writes a stranger can drive
 * against the same accounts.
 */
class account_door_routes final {
private:
    inline static std::string_view logger_name = "ores.http.routes.iam.account_door_routes";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief A refused registration, its status and code chosen by the
     * service's own code, as the BFF chose them, so the door can say what an
     * administrator must fix. A code this build does not know is a refusal.
     */
    [[nodiscard]] static ores::http::net::record_routes::refusal
    signup_refusal(const std::string& code, const std::string& message);

    /**
     * @brief A registration that succeeded, as the door reads it.
     */
    [[nodiscard]] static std::string
    signup_json(const ores::iam::messaging::signup_response& reply);

    /**
     * @brief The password rules, as the sign-in screen states them.
     */
    [[nodiscard]] static std::string
    policy_json(const ores::iam::messaging::get_password_policy_response& reply);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
