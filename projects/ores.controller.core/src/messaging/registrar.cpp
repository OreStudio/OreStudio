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
#include "ores.controller.core/messaging/registrar.hpp"

#include "ores.controller.core/service/process_supervisor.hpp"
#include "ores.controller.api/messaging/service_definition_protocol.hpp"
#include "ores.controller.api/messaging/service_instance_protocol.hpp"
#include "ores.controller.api/messaging/service_event_protocol.hpp"
#include "ores.controller.core/messaging/service_definition_handler.hpp"
#include "ores.controller.core/messaging/service_instance_handler.hpp"
#include "ores.controller.core/messaging/service_event_handler.hpp"

namespace ores::controller::messaging {

using namespace ores::controller::api::messaging;

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier,
    service::process_supervisor* supervisor) {

    std::vector<ores::nats::service::subscription> subs;
    constexpr auto queue = "ores.controller.service";

    // Service definitions
    subs.push_back(nats.queue_subscribe(
        std::string(list_service_definitions_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            service_definition_handler h(nats, ctx, verifier);
            h.list(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_service_definition_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            service_definition_handler h(nats, ctx, verifier);
            h.save(std::move(msg));
        }));

    // Service instances
    subs.push_back(nats.queue_subscribe(
        std::string(list_service_instances_request::nats_subject), queue,
        [&nats, ctx, verifier, supervisor](ores::nats::message msg) mutable {
            service_instance_handler h(nats, ctx, verifier, supervisor);
            h.list(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(start_service_request::nats_subject), queue,
        [&nats, ctx, verifier, supervisor](ores::nats::message msg) mutable {
            service_instance_handler h(nats, ctx, verifier, supervisor);
            h.start(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(stop_service_request::nats_subject), queue,
        [&nats, ctx, verifier, supervisor](ores::nats::message msg) mutable {
            service_instance_handler h(nats, ctx, verifier, supervisor);
            h.stop(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(restart_service_request::nats_subject), queue,
        [&nats, ctx, verifier, supervisor](ores::nats::message msg) mutable {
            service_instance_handler h(nats, ctx, verifier, supervisor);
            h.restart(std::move(msg));
        }));

    // Service events
    subs.push_back(nats.queue_subscribe(
        std::string(list_service_events_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            service_event_handler h(nats, ctx, verifier);
            h.list(std::move(msg));
        }));

    return subs;
}

}
