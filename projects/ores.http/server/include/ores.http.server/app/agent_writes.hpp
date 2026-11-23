/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_HTTP_SERVER_APP_AGENT_WRITES_HPP
#define ORES_HTTP_SERVER_APP_AGENT_WRITES_HPP

#include "ores.database/domain/context.hpp"
#include "ores.http.api/domain/bearer_exchange.hpp"
#include "ores.http.api/net/http_server.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <string>

namespace ores::http::server::app {

/**
 * @brief Holds an LLM agent's writes for its person, and replays them once
 * approved.
 *
 * A write an agent makes raises an iam.write_by_agent approval request, with the
 * agent's own token, and is kept as the call itself. When its person approves
 * the request the gateway replays the call through its own routes, as the
 * person with the agent as actor, under a token IAM issues for that request
 * alone. The replay meets whatever control the person's own write would, so a
 * gated entity raises its own request and a trade enters authorisation.
 */
class agent_writes final {
public:
    static constexpr std::string_view kind_code = "iam.write_by_agent";

    /**
     * @brief The hold the gateway puts in front of every write an agent makes.
     *
     * A write is exempt, and runs as any other, when an approval policy row
     * names agent_write, the method and the route with the part none.
     */
    [[nodiscard]] static ores::http::domain::exchanged_write_holder
    holder(ores::database::context ctx, ores::nats::service::nats_client& service);

    /**
     * @brief Replays the write an approved agent write request holds, if the
     * event is one; returns at once and does the work on a thread of its own.
     */
    static void on_request_event(const ores::nats::message& msg,
                                 ores::database::context ctx,
                                 ores::nats::service::nats_client& service,
                                 ores::http::net::http_server& server);
};

}

#endif
