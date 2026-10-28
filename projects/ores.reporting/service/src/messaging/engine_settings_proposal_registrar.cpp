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
#include "ores.reporting.service/messaging/engine_settings_proposal_registrar.hpp"
#include "ores.reporting.api/messaging/engine_settings_proposal_operations_protocol.hpp"
#include "ores.reporting.api/workflow/engine_settings_apply_workflow.hpp"
#include "ores.reporting.service/messaging/engine_settings_apply_step_handler.hpp"
#include "ores.reporting.service/messaging/engine_settings_proposal_handler.hpp"
#include "ores.service/messaging/step_command_stream.hpp"
#include <memory>
#include <string_view>
#include <utility>

namespace ores::reporting::messaging {

namespace {

constexpr std::string_view queue_group = "ores.reporting.service";

}

std::vector<ores::nats::service::subscription>
engine_settings_proposal_registrar::register_handlers(
    ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    // The apply workflow's commands come from the engine, which has proved
    // them, so the step handler takes no verifier. These are workflow step
    // commands, which the engine commits to a stream, so this service declares
    // the coverage and reads them durably.
    ores::service::messaging::step_routes routes{"apply"};

    auto steps = std::make_shared<engine_settings_apply_step_handler>(nats, ctx);
    routes.on(workflow::recheck_engine_settings_changes_subject,
              [steps](ores::nats::message msg) { steps->recheck(std::move(msg)); });
    routes.on(workflow::apply_engine_settings_changes_subject,
              [steps](ores::nats::message msg) { steps->apply(std::move(msg)); });

    subs.push_back(ores::service::messaging::serve_step_commands(nats, queue_group, routes));

    auto h = std::make_shared<engine_settings_proposal_handler>(
        nats, std::move(ctx), std::move(verifier));
    subs.push_back(nats.queue_subscribe(
        raise_engine_settings_change_request::nats_subject,
        queue_group,
        [h](ores::nats::message msg) { h->raise(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_engine_settings_changes_request::nats_subject,
        queue_group,
        [h](ores::nats::message msg) { h->get_changes(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        recheck_engine_settings_changes_request::nats_subject,
        queue_group,
        [h](ores::nats::message msg) { h->recheck(std::move(msg)); }));
    return subs;
}

}
