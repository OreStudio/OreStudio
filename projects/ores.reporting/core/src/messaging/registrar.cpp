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
#include "ores.reporting.api/messaging/report_error_event_protocol.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.api/messaging/report_type_protocol.hpp"
#include "ores.reporting.api/messaging/run_document_protocol.hpp"
#include "ores.reporting.core/messaging/analytic_type_registrar.hpp"
#include "ores.reporting.core/messaging/concurrency_policy_handler.hpp"
#include "ores.reporting.core/messaging/configuration_analytic_registrar.hpp"
#include "ores.reporting.core/messaging/configuration_registrar.hpp"
#include "ores.reporting.core/messaging/configuration_type_registrar.hpp"
#include "ores.reporting.core/messaging/cut_family_handler.hpp"
#include "ores.reporting.core/messaging/cut_resolution_rule_handler.hpp"
#include "ores.reporting.core/messaging/cut_kind_handler.hpp"
#include "ores.reporting.core/messaging/cut_family_registrar.hpp"
#include "ores.reporting.core/messaging/cut_resolution_rule_registrar.hpp"
#include "ores.reporting.core/messaging/cut_kind_registrar.hpp"
#include "ores.reporting.core/messaging/engine_settings_building_registrar.hpp"
#include "ores.reporting.core/messaging/engine_settings_change_registrar.hpp"
#include "ores.reporting.core/messaging/engine_settings_execution_registrar.hpp"
#include "ores.reporting.core/messaging/engine_settings_fixings_registrar.hpp"
#include "ores.reporting.core/messaging/engine_settings_group_registrar.hpp"
#include "ores.reporting.core/messaging/engine_settings_group_setting_registrar.hpp"
#include "ores.reporting.core/messaging/engine_settings_registrar.hpp"
#include "ores.reporting.core/messaging/file_type_registrar.hpp"
#include "ores.reporting.core/messaging/ore_parameter_registrar.hpp"
#include "ores.reporting.core/messaging/ore_parameter_setting_value_set_registrar.hpp"
#include "ores.reporting.core/messaging/ore_parameter_value_domain_registrar.hpp"
#include "ores.reporting.core/messaging/ore_parameter_value_set_catalogue_registrar.hpp"
#include "ores.reporting.core/messaging/ore_parameter_value_set_registrar.hpp"
#include "ores.reporting.core/messaging/ore_parameter_value_set_value_registrar.hpp"
#include "ores.reporting.core/messaging/publish_from_dq_handler.hpp"
#include "ores.reporting.core/messaging/report_configuration_registrar.hpp"
#include "ores.reporting.core/messaging/report_definition_handler.hpp"
#include "ores.reporting.core/messaging/report_definition_template_handler.hpp"
#include "ores.reporting.core/messaging/report_error_event_handler.hpp"
#include "ores.reporting.core/messaging/report_error_event_registrar.hpp"
#include "ores.reporting.core/messaging/report_error_scope_registrar.hpp"
#include "ores.reporting.core/messaging/report_execution_handler.hpp"
#include "ores.reporting.core/messaging/report_instance_handler.hpp"
#include "ores.reporting.core/messaging/report_instance_trigger_handler.hpp"
#include "ores.reporting.core/messaging/report_result_row_registrar.hpp"
#include "ores.reporting.core/messaging/report_run_health_handler.hpp"
#include "ores.reporting.core/messaging/report_run_pricing_handler.hpp"
#include "ores.reporting.core/messaging/report_scheduling_handler.hpp"
#include "ores.reporting.core/messaging/report_type_handler.hpp"
#include "ores.reporting.core/messaging/run_document_handler.hpp"
#include "ores.service/messaging/step_command_stream.hpp"
#include "ores.workflow.core/service/fsm_state_map.hpp"
#include <iterator>
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
    // Cut families (generated CRUD handler)
    // ----------------------------------------------------------------
    auto cfh = std::make_shared<cut_family_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        list_cut_families_request::nats_subject, group, [cfh](ores::nats::message msg) {
            cfh->list_cut_families(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_cut_family_request::nats_subject, group, [cfh](ores::nats::message msg) {
            cfh->get_cut_family(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_many_cut_families_request::nats_subject, group, [cfh](ores::nats::message msg) {
            cfh->get_many_cut_families(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        put_cut_family_request::nats_subject, group, [cfh](ores::nats::message msg) {
            cfh->put_cut_family(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        put_many_cut_families_request::nats_subject, group, [cfh](ores::nats::message msg) {
            cfh->put_many_cut_families(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_cut_family_request::nats_subject, group, [cfh](ores::nats::message msg) {
            cfh->delete_cut_family(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_many_cut_families_request::nats_subject, group, [cfh](ores::nats::message msg) {
            cfh->delete_many_cut_families(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        list_cut_family_versions_request::nats_subject, group, [cfh](ores::nats::message msg) {
            cfh->list_cut_family_versions(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_cut_family_version_request::nats_subject, group, [cfh](ores::nats::message msg) {
            cfh->get_cut_family_version(std::move(msg));
        }));

    // ----------------------------------------------------------------
    // Cut resolution rules (generated CRUD handler)
    // ----------------------------------------------------------------
    auto crrh = std::make_shared<cut_resolution_rule_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        list_cut_resolution_rules_request::nats_subject, group, [crrh](ores::nats::message msg) {
            crrh->list_cut_resolution_rules(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_cut_resolution_rule_request::nats_subject, group, [crrh](ores::nats::message msg) {
            crrh->get_cut_resolution_rule(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_many_cut_resolution_rules_request::nats_subject, group, [crrh](ores::nats::message msg) {
            crrh->get_many_cut_resolution_rules(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        put_cut_resolution_rule_request::nats_subject, group, [crrh](ores::nats::message msg) {
            crrh->put_cut_resolution_rule(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        put_many_cut_resolution_rules_request::nats_subject, group, [crrh](ores::nats::message msg) {
            crrh->put_many_cut_resolution_rules(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_cut_resolution_rule_request::nats_subject, group, [crrh](ores::nats::message msg) {
            crrh->delete_cut_resolution_rule(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_many_cut_resolution_rules_request::nats_subject, group, [crrh](ores::nats::message msg) {
            crrh->delete_many_cut_resolution_rules(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        list_cut_resolution_rule_versions_request::nats_subject, group, [crrh](ores::nats::message msg) {
            crrh->list_cut_resolution_rule_versions(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_cut_resolution_rule_version_request::nats_subject, group, [crrh](ores::nats::message msg) {
            crrh->get_cut_resolution_rule_version(std::move(msg));
        }));

    // ----------------------------------------------------------------
    // Cut kinds (generated CRUD handler)
    // ----------------------------------------------------------------
    auto ckh = std::make_shared<cut_kind_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        list_cut_kinds_request::nats_subject, group, [ckh](ores::nats::message msg) {
            ckh->list_cut_kinds(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_cut_kind_request::nats_subject, group, [ckh](ores::nats::message msg) {
            ckh->get_cut_kind(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_many_cut_kinds_request::nats_subject, group, [ckh](ores::nats::message msg) {
            ckh->get_many_cut_kinds(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        put_cut_kind_request::nats_subject, group, [ckh](ores::nats::message msg) {
            ckh->put_cut_kind(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        put_many_cut_kinds_request::nats_subject, group, [ckh](ores::nats::message msg) {
            ckh->put_many_cut_kinds(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_cut_kind_request::nats_subject, group, [ckh](ores::nats::message msg) {
            ckh->delete_cut_kind(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_many_cut_kinds_request::nats_subject, group, [ckh](ores::nats::message msg) {
            ckh->delete_many_cut_kinds(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        list_cut_kind_versions_request::nats_subject, group, [ckh](ores::nats::message msg) {
            ckh->list_cut_kind_versions(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_cut_kind_version_request::nats_subject, group, [ckh](ores::nats::message msg) {
            ckh->get_cut_kind_version(std::move(msg));
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
        subs.push_back(nats.queue_subscribe(
            get_next_fire_request::nats_subject, group, [rsh](ores::nats::message msg) {
                rsh->get_next_fire(std::move(msg));
            }));
    }

    // ----------------------------------------------------------------
    // Report instances (generated CRUD handler)
    // ----------------------------------------------------------------
    auto rdoc = std::make_shared<run_document_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        save_run_document_request::nats_subject, group, [rdoc](ores::nats::message msg) {
            rdoc->save_run_document(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        bind_configuration_request::nats_subject, group, [rdoc](ores::nats::message msg) {
            rdoc->bind_configuration(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_run_document_request::nats_subject, group, [rdoc](ores::nats::message msg) {
            rdoc->get_run_document(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_run_document_request::nats_subject, group, [rdoc](ores::nats::message msg) {
            rdoc->delete_run_document(std::move(msg));
        }));

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
    subs.push_back(
        nats.queue_subscribe(get_latest_report_instances_by_definition_request::nats_subject,
                             group,
                             [rih](ores::nats::message msg) {
                                 rih->get_latest_report_instances_by_definition(std::move(msg));
                             }));
    subs.push_back(nats.queue_subscribe(
        get_report_instance_health_request::nats_subject, group, [rih](ores::nats::message msg) {
            rih->get_report_instance_health(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(count_report_definitions_by_health_request::nats_subject,
                                        group,
                                        [rih](ores::nats::message msg) {
                                            rih->count_report_definitions_by_health(std::move(msg));
                                        }));

    // ----------------------------------------------------------------
    // Report error events and the scopes they name. Both reads are the
    // generated registrars', so the handler's whole surface is served.
    // ----------------------------------------------------------------
    {
        auto error_subs = register_report_error_event_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(error_subs.begin()),
                    std::make_move_iterator(error_subs.end()));
        auto scope_subs = register_report_error_scope_handlers(nats, ctx, verifier);
        subs.insert(subs.end(),
                    std::make_move_iterator(scope_subs.begin()),
                    std::make_move_iterator(scope_subs.end()));
    }

    // ----------------------------------------------------------------
    // Report instance trigger (hand-crafted handler — not codegen)
    // ----------------------------------------------------------------
    {
        auto rith = std::make_shared<report_instance_trigger_handler>(
            nats, ctx, verifier, instance_states, svc_nats);
        subs.push_back(nats.queue_subscribe(
            trigger_report_instance_request::nats_subject, group, [rith](ores::nats::message msg) {
                rith->trigger(std::move(msg));
            }));
    }

    // ----------------------------------------------------------------
    // A run's health as the shell asks it (hand-crafted handler — not
    // codegen). It answers from the read the web already asks.
    // ----------------------------------------------------------------
    {
        auto rrhh = std::make_shared<report_run_health_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_run_health_request::nats_subject, group, [rrhh](ores::nats::message msg) {
                rrhh->get_run_health(std::move(msg));
            }));
    }

    // ----------------------------------------------------------------
    // The date a run priced at, as the packaging step states it
    // (hand-crafted handler — not codegen). The step is the only place
    // that knows both the date the run asked for and the date the
    // gathered data carries, so the run records what it used.
    // ----------------------------------------------------------------
    {
        auto rrp = std::make_shared<report_run_pricing_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            record_run_pricing_request::nats_subject, group, [rrp](ores::nats::message msg) {
                rrp->record_run_pricing(std::move(msg));
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
    // Report execution workflow step handlers. One durable consumer
    // carries the whole family, and each handler answers its own
    // delivery.
    // ----------------------------------------------------------------
    auto reh = std::make_shared<report_execution_handler>(
        nats, ctx, svc_nats, instance_states, std::move(http_base_url));
    ores::service::messaging::step_routes execution_routes{"execution"};
    execution_routes
        .on(gather_trades_request::nats_subject,
            [reh](ores::nats::message msg) { reh->gather_trades(std::move(msg)); })
        .on(gather_market_data_request::nats_subject,
            [reh](ores::nats::message msg) { reh->gather_market_data(std::move(msg)); })
        .on(assemble_bundle_request::nats_subject,
            [reh](ores::nats::message msg) { reh->assemble_bundle(std::move(msg)); })
        .on(resolve_prepared_input_request::nats_subject,
            [reh](ores::nats::message msg) { reh->resolve_prepared_input(std::move(msg)); })
        .on(collect_compute_results_request::nats_subject,
            [reh](ores::nats::message msg) { reh->collect_results(std::move(msg)); })
        .on(post_process_results_request::nats_subject,
            [reh](ores::nats::message msg) { reh->post_process_results(std::move(msg)); })
        .on(ignore_compute_results_request::nats_subject,
            [reh](ores::nats::message msg) { reh->ignore_compute_results(std::move(msg)); })
        .on(finalise_report_request::nats_subject,
            [reh](ores::nats::message msg) { reh->finalise(std::move(msg)); })
        .on(fail_report_request::nats_subject,
            [reh](ores::nats::message msg) { reh->fail(std::move(msg)); });
    subs.push_back(
        ores::service::messaging::serve_step_commands(nats, queue_group, execution_routes));

    // ----------------------------------------------------------------
    // Publish-from-DQ workflow step handlers. One handler serves every
    // subject, and it derives the SQL function from the delivered
    // subject, so a new dataset needs a subject here and a row in
    // ores_dq_artefact_types_tbl, and nothing else.
    // ----------------------------------------------------------------
    {
        auto pdq = std::make_shared<publish_from_dq_handler>(nats, ctx);
        static constexpr std::string_view publish_subjects[]{
            publish_report_definitions_from_dq_request::nats_subject,
            publish_risk_report_configs_from_dq_request::nats_subject,
            publish_engine_settings_from_dq_request::nats_subject,
            publish_ore_parameter_value_domains_from_dq_request::nats_subject,
            publish_ore_parameters_from_dq_request::nats_subject,
        };
        ores::service::messaging::step_routes routes;
        for (const auto subject : publish_subjects)
            routes.on(subject, [pdq](ores::nats::message msg) { pdq->handle(std::move(msg)); });
        subs.push_back(ores::service::messaging::serve_step_commands(nats, queue_group, routes));
    }

    // ----------------------------------------------------------------
    // File types (generated CRUD handlers). The export asks for them to name the files of
    // a run.
    // ----------------------------------------------------------------
    for (auto& sub : register_file_type_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));

    // ----------------------------------------------------------------
    // Configurations (generated CRUD handlers). A report page reads the
    // configurations a report binds, their kinds and their analytics.
    // ----------------------------------------------------------------
    for (auto& sub : register_configuration_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));
    for (auto& sub : register_report_configuration_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));
    for (auto& sub : register_configuration_type_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));
    for (auto& sub : register_configuration_analytic_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));
    for (auto& sub : register_analytic_type_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));

    // ----------------------------------------------------------------
    // ORE parameters and engine settings (generated CRUD handlers). The engine
    // settings screens read the documentation of each setting from the ORE
    // parameters, and read and write the settings of a configuration through
    // the central row and its three parts.
    // ----------------------------------------------------------------
    for (auto& sub : register_engine_settings_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));
    for (auto& sub : register_engine_settings_building_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));
    for (auto& sub : register_engine_settings_execution_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));
    for (auto& sub : register_engine_settings_fixings_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));
    for (auto& sub : register_engine_settings_group_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));
    for (auto& sub : register_engine_settings_group_setting_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));
    // A proposal's lines are read by the operations that propose them; the
    // entity's own read serves the shell and a future audit screen.
    for (auto& sub : register_engine_settings_change_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));
    for (auto& sub : register_ore_parameter_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));
    for (auto& sub : register_ore_parameter_value_domain_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));
    for (auto& sub : register_ore_parameter_value_set_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));
    for (auto& sub : register_ore_parameter_value_set_value_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));
    for (auto& sub : register_ore_parameter_value_set_catalogue_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));
    for (auto& sub : register_ore_parameter_setting_value_set_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));

    // ----------------------------------------------------------------
    // A run's result rows (generated CRUD handlers). The rows are a time
    // series the post-processor writes, and a screen asks for one run through
    // the filter. The handler checks the read permission and the service
    // honours the page and every filter member, so this path bounds the read.
    // ----------------------------------------------------------------
    for (auto& sub : register_report_result_row_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));

    return subs;
}

} // namespace ores::reporting::messaging
