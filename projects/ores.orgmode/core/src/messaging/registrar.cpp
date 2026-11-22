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
#include "ores.orgmode.core/messaging/registrar.hpp"
#include "ores.orgmode.api/messaging/publish_operation_protocol.hpp"
#include "ores.orgmode.core/messaging/knowledge_page_registrar.hpp"
#include "ores.orgmode.core/messaging/publish_from_dq_handler.hpp"
#include "ores.service/messaging/step_command_stream.hpp"
#include <memory>
#include <utility>

namespace ores::orgmode::messaging {

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             const ores::database::context& ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    auto subs = register_knowledge_page_handlers(nats, ctx, std::move(verifier));

    auto pdq = std::make_shared<publish_from_dq_handler>(nats, ctx);
    ores::service::messaging::step_routes routes;
    routes.on(publish_knowledge_pages_from_dq_request::nats_subject,
              [pdq](ores::nats::delivery delivery) { pdq->handle(std::move(delivery)); });
    subs.push_back(
        ores::service::messaging::serve_step_commands(nats, "ores.orgmode.service", routes));

    return subs;
}

}
