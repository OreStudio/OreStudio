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
#include "ores.workflow/messaging/registrar.hpp"

#include <memory>
#include "ores.logging/make_logger.hpp"
#include "ores.workflow.api/messaging/workflow_events.hpp"
#include "ores.workflow/messaging/workflow_handler.hpp"
#include "ores.workflow/messaging/workflow_query_handler.hpp"
#include "ores.workflow.api/messaging/workflow_protocol.hpp"
#include "ores.workflow.api/messaging/workflow_query_protocol.hpp"
#include "ores.workflow/service/fsm_state_map.hpp"
#include "ores.workflow/service/workflow_engine.hpp"
#include "ores.workflow/service/workflow_registry.hpp"
#include "ores.workflow/service/provision_parties_definitions.hpp"
#include "ores.workflow/service/ore_import_definitions.hpp"
#include "ores.workflow/service/report_execution_definitions.hpp"

namespace ores::workflow::messaging {

namespace {

using namespace ores::logging;
inline static std::string_view logger_name = "ores.workflow.messaging.registrar";
static auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

} // namespace

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    ores::security::jwt::jwt_authenticator signer,
    ores::nats::service::nats_client outbound_nats) {

    std::vector<ores::nats::service::subscription> subs;
    constexpr auto qg = "ores.workflow.service";

    // ----------------------------------------------------------------
    // Load FSM state maps once at startup (one NATS round-trip each).
    // ----------------------------------------------------------------
    const auto instance_states =
        service::load_fsm_states(outbound_nats, "workflow_instance");
    const auto step_states =
        service::load_fsm_states(outbound_nats, "workflow_step");

    // ----------------------------------------------------------------
    // Build workflow registry (one entry per known workflow type).
    // ----------------------------------------------------------------
    auto registry = std::make_shared<service::workflow_registry>();
    service::register_provision_parties_workflow(*registry);
    service::register_ore_import_workflow(*registry);
    service::register_report_execution_workflow(*registry);

    // ----------------------------------------------------------------
    // Create the engine (shared across all engine subscriptions).
    // ----------------------------------------------------------------
    auto engine = std::make_shared<service::workflow_engine>(
        nats, ctx, *registry, instance_states, step_states);

    // ----------------------------------------------------------------
    // Engine subscriptions
    // ----------------------------------------------------------------
    subs.push_back(nats.queue_subscribe(
        messaging::step_completed_event::nats_subject, qg,
        [engine](ores::nats::message msg) {
            engine->on_step_completed(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        messaging::start_workflow_message::nats_subject, qg,
        [engine](ores::nats::message msg) {
            engine->on_start_workflow(std::move(msg));
        }));

    // ----------------------------------------------------------------
    // Query handler (list instances, get steps).
    // ctx and signer are copied here; they are moved into wh below.
    // ----------------------------------------------------------------
    auto qh = std::make_shared<workflow_query_handler>(
        nats, ctx, signer, instance_states, step_states);

    subs.push_back(nats.queue_subscribe(
        list_workflow_instances_request::nats_subject, qg,
        [qh](ores::nats::message msg) {
            qh->list_instances(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        get_workflow_steps_request::nats_subject, qg,
        [qh](ores::nats::message msg) {
            qh->get_steps(std::move(msg));
        }));

    // ----------------------------------------------------------------
    // Provision parties request/reply handler.
    // Validates JWT, pre-generates party UUIDs, and dispatches one
    // start_workflow_message per party (fire-and-forget).
    // ----------------------------------------------------------------
    auto wh = std::make_shared<workflow_handler>(
        nats, std::move(ctx), std::move(signer));

    subs.push_back(nats.queue_subscribe(
        provision_parties_request::nats_subject, qg,
        [wh](ores::nats::message msg) {
            wh->provision_parties(std::move(msg));
        }));

    // ----------------------------------------------------------------
    // Startup recovery: re-dispatch any in-progress workflow steps.
    // ----------------------------------------------------------------
    try {
        engine->recover_in_progress();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "Workflow recovery failed: " << e.what();
    }

    BOOST_LOG_SEV(lg(), debug) << "Registered " << subs.size()
                               << " workflow message handlers.";
    return subs;
}

}
