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
#ifndef ORES_HTTP_ROUTES_IAM_AUDIT_ROUTES_HPP
#define ORES_HTTP_ROUTES_IAM_AUDIT_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.iam.api/messaging/auth_event_operations_protocol.hpp"
#include "ores.iam.api/messaging/session_statistics_operations_protocol.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <chrono>
#include <cstdint>
#include <initializer_list>
#include <memory>
#include <optional>
#include <string>
#include <string_view>

namespace ores::http::routes::iam {

/**
 * @brief The audit screens' reads and the end of a session, at the addresses
 * and in the shapes ores.web reads.
 *
 * The sign-in events and the session statistics of a period, the country an
 * address is in, and an administrator ending one session. Each forwards the
 * caller's own token, so IAM's permission check governs.
 */
class audit_routes final {
public:
    /**
     * @brief The account, period and page an audit read names.
     */
    struct audit_query final {
        std::string account_id;
        std::string event_type;
        std::optional<std::chrono::system_clock::time_point> from;
        std::optional<std::chrono::system_clock::time_point> to;
        std::uint32_t offset = 0;
        std::uint32_t limit = 100;
    };

    /**
     * @brief Register the routes.
     */
    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);

    /**
     * @brief The audit query a request names at @p now, or nothing when it is
     * not one: only the named keys, an account of at most 64 characters, a
     * period of hour, day, week or all, and a page of 1 to 500. A period other
     * than all reaches back from now.
     */
    [[nodiscard]] static std::optional<audit_query>
    read_query(const ores::http::domain::http_request& req,
               std::initializer_list<std::string_view> keys,
               std::chrono::system_clock::time_point now);

    /**
     * @brief The sign-in events, as the screen reads them.
     */
    [[nodiscard]] static std::string
    events_json(const ores::iam::messaging::list_auth_events_response& reply);

    /**
     * @brief The session statistics, as the screen reads them.
     */
    [[nodiscard]] static std::string
    statistics_json(const ores::iam::messaging::get_session_statistics_response& reply);
};

}

#endif
