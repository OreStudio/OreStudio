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
#include "ores.reporting.core/messaging/registrar.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.reporting.api/messaging/concurrency_policy_protocol.hpp"
#include "ores.reporting.api/messaging/report_definition_protocol.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.api/messaging/report_type_protocol.hpp"
#include "ores.reporting.core/messaging/concurrency_policy_handler.hpp"
#include "ores.reporting.core/messaging/publish_from_dq_handler.hpp"
#include "ores.reporting.core/messaging/report_definition_handler.hpp"
#include "ores.reporting.core/messaging/report_definition_template_handler.hpp"
#include "ores.reporting.core/messaging/report_execution_handler.hpp"
#include "ores.reporting.core/messaging/report_instance_handler.hpp"
#include "ores.reporting.core/messaging/report_instance_trigger_handler.hpp"
#include "ores.reporting.core/messaging/report_scheduling_handler.hpp"
#include "ores.reporting.core/messaging/report_type_handler.hpp"
#include "ores.workflow.core/service/fsm_state_map.hpp"
#include <memory>
#include <optional>
#include <string>
#include <string_view>

namespace ores::reporting::messaging {

namespace {
constexpr std::string_view queue_group = "ores.reporting.service";
}

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier,
                             ores::nats::service::nats_client& svc_nats,
                             std::string http_base_url) {
    std::vector<ores::nats::service::subscription> subs;
    const auto group = std::string(queue_group);

    // ----------------------------------------------------------------
    // Load FSM state maps (one store read each).
    // ----------------------------------------------------------------
    const auto instance_states =
        ores::workflow::service::load_fsm_states(ctx, "report_instance_lifecycle");

    // ----------------------------------------------------------------
    // Report definition templates
    // ----------------------------------------------------------------
    auto rdth = std::make_shared<report_definition_template_handler>(nats, ctx, verifier);
    subs.push_back(
        nats.queue_subscribe(get_report_definition_templates_request::nats_subject,
                             group,
                             [rdth](ores::nats::message msg) { rdth->list(std::move(msg)); }));

    // ----------------------------------------------------------------
    // Report types
    // ----------------------------------------------------------------
    auto rth = std::make_shared<report_type_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        list_report_types_request::nats_subject, group, [rth](ores::nats::message msg) {
            rth->list_report_types(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        put_report_type_request::nats_subject, group, [rth](ores::nats::message msg) {
            rth->put_report_type(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_report_type_request::nats_subject, group, [rth](ores::nats::message msg) {
            rth->delete_report_type(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        list_report_type_versions_request::nats_subject, group, [rth](ores::nats::message msg) {
            rth->list_report_type_versions(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_report_type_request::nats_subject, group, [rth](ores::nats::message msg) {
            rth->get_report_type(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_many_report_types_request::nats_subject, group, [rth](ores::nats::message msg) {
            rth->get_many_report_types(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        put_many_report_types_request::nats_subject, group, [rth](ores::nats::message msg) {
            rth->put_many_report_types(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_many_report_types_request::nats_subject, group, [rth](ores::nats::message msg) {
            rth->delete_many_report_types(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_report_type_version_request::nats_subject, group, [rth](ores::nats::message msg) {
            rth->get_report_type_version(std::move(msg));
        }));

    // ----------------------------------------------------------------
    // Report definitions (generated CRUD handler)
    // ----------------------------------------------------------------
    auto rdh = std::make_shared<report_definition_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        list_report_definitions_request::nats_subject, group, [rdh](ores::nats::message msg) {
            rdh->list_report_definitions(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        put_report_definition_request::nats_subject, group, [rdh](ores::nats::message msg) {
            rdh->put_report_definition(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_report_definition_request::nats_subject, group, [rdh](ores::nats::message msg) {
            rdh->delete_report_definition(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        list_report_definition_versions_request::nats_subject,
        group,
        [rdh](ores::nats::message msg) { rdh->list_report_definition_versions(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_report_definition_request::nats_subject, group, [rdh](ores::nats::message msg) {
            rdh->get_report_definition(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_many_report_definitions_request::nats_subject, group, [rdh](ores::nats::message msg) {
            rdh->get_many_report_definitions(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        put_many_report_definitions_request::nats_subject, group, [rdh](ores::nats::message msg) {
            rdh->put_many_report_definitions(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_many_report_definitions_request::nats_subject,
        group,
        [rdh](ores::nats::message msg) { rdh->delete_many_report_definitions(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_report_definition_version_request::nats_subject, group, [rdh](ores::nats::message msg) {
            rdh->get_report_definition_version(std::move(msg));
        }));

    // ----------------------------------------------------------------
    // Report definition scheduling (hand-crafted handler — not codegen)
    // ----------------------------------------------------------------
    {
        auto rsh = std::make_shared<report_scheduling_handler>(nats, ctx, verifier, svc_nats);
        subs.push_back(nats.queue_subscribe(
            schedule_report_definitions_request::nats_subject,
            group,
            [rsh](ores::nats::message msg) { rsh->schedule(std::move(msg)); }));
        subs.push_back(nats.queue_subscribe(
            unschedule_report_definitions_request::nats_subject,
            group,
            [rsh](ores::nats::message msg) { rsh->unschedule(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Report instances (generated CRUD handler)
    // ----------------------------------------------------------------
    auto rih = std::make_shared<report_instance_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        list_report_instances_request::nats_subject, group, [rih](ores::nats::message msg) {
            rih->list_report_instances(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        put_report_instance_request::nats_subject, group, [rih](ores::nats::message msg) {
            rih->put_report_instance(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_report_instance_request::nats_subject, group, [rih](ores::nats::message msg) {
            rih->delete_report_instance(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        list_report_instance_versions_request::nats_subject, group, [rih](ores::nats::message msg) {
            rih->list_report_instance_versions(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_report_instance_request::nats_subject, group, [rih](ores::nats::message msg) {
            rih->get_report_instance(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_many_report_instances_request::nats_subject, group, [rih](ores::nats::message msg) {
            rih->get_many_report_instances(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        put_many_report_instances_request::nats_subject, group, [rih](ores::nats::message msg) {
            rih->put_many_report_instances(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_many_report_instances_request::nats_subject, group, [rih](ores::nats::message msg) {
            rih->delete_many_report_instances(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_report_instance_version_request::nats_subject, group, [rih](ores::nats::message msg) {
            rih->get_report_instance_version(std::move(msg));
        }));

    // ----------------------------------------------------------------
    // Report instance trigger (hand-crafted handler — not codegen)
    // ----------------------------------------------------------------
    {
        auto rith =
            std::make_shared<report_instance_trigger_handler>(nats, ctx, verifier, instance_states);
        subs.push_back(nats.queue_subscribe(
            trigger_report_instance_request::nats_subject, group, [rith](ores::nats::message msg) {
                rith->trigger(std::move(msg));
            }));
    }

    // ----------------------------------------------------------------
    // Concurrency policies
    // ----------------------------------------------------------------
    auto cph = std::make_shared<concurrency_policy_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        list_concurrency_policies_request::nats_subject, group, [cph](ores::nats::message msg) {
            cph->list_concurrency_policies(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        put_concurrency_policy_request::nats_subject, group, [cph](ores::nats::message msg) {
            cph->put_concurrency_policy(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_concurrency_policy_request::nats_subject, group, [cph](ores::nats::message msg) {
            cph->delete_concurrency_policy(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        list_concurrency_policy_versions_request::nats_subject,
        group,
        [cph](ores::nats::message msg) { cph->list_concurrency_policy_versions(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_concurrency_policy_request::nats_subject, group, [cph](ores::nats::message msg) {
            cph->get_concurrency_policy(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_many_concurrency_policies_request::nats_subject, group, [cph](ores::nats::message msg) {
            cph->get_many_concurrency_policies(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        put_many_concurrency_policies_request::nats_subject, group, [cph](ores::nats::message msg) {
            cph->put_many_concurrency_policies(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_many_concurrency_policies_request::nats_subject,
        group,
        [cph](ores::nats::message msg) { cph->delete_many_concurrency_policies(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_concurrency_policy_version_request::nats_subject,
        group,
        [cph](ores::nats::message msg) { cph->get_concurrency_policy_version(std::move(msg)); }));

    // ----------------------------------------------------------------
    // Report execution workflow step handlers
    // ----------------------------------------------------------------
    auto reh = std::make_shared<report_execution_handler>(
        nats, ctx, svc_nats, instance_states, std::move(http_base_url));

    subs.push_back(nats.queue_subscribe(
        std::string(gather_trades_request::nats_subject), group, [reh](ores::nats::message msg) {
            reh->gather_trades(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(gather_market_data_request::nats_subject),
        group,
        [reh](ores::nats::message msg) { reh->gather_market_data(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        std::string(assemble_bundle_request::nats_subject), group, [reh](ores::nats::message msg) {
            reh->assemble_bundle(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(collect_compute_results_request::nats_subject),
        group,
        [reh](ores::nats::message msg) { reh->collect_results(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        std::string(finalise_report_request::nats_subject), group, [reh](ores::nats::message msg) {
            reh->finalise(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(fail_report_request::nats_subject), group, [reh](ores::nats::message msg) {
            reh->fail(std::move(msg));
        }));

    // ----------------------------------------------------------------
    // Publish-from-DQ workflow step handler
    // ----------------------------------------------------------------
    {
        auto pdq = std::make_shared<publish_from_dq_handler>(nats, ctx);
        subs.push_back(
            nats.queue_subscribe(publish_report_definitions_from_dq_request::nats_subject,
                                 group,
                                 [pdq](ores::nats::message msg) { pdq->handle(std::move(msg)); }));
    }

    return subs;
}

} // namespace ores::reporting::messaging
