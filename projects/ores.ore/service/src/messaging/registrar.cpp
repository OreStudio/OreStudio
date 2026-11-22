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
#include "ores.ore.service/messaging/registrar.hpp"
#include "ores.history.core/messaging/registrar.hpp"
#include "ores.history.core/service/dispatch_registry.hpp"
#include "ores.logging/logger.hpp"
#include "ores.ore.api/messaging/currency_config_protocol.hpp"
#include "ores.ore.api/messaging/ore_import_protocol.hpp"
#include "ores.ore.api/messaging/pricing_engines_protocol.hpp"
#include "ores.ore.api/messaging/run_configuration_protocol.hpp"
#include "ores.ore.core/messaging/ore_file_category_history_provider_registrar.hpp"
#include "ores.ore.core/messaging/ore_file_category_registrar.hpp"
#include "ores.ore.core/messaging/ore_file_history_provider_registrar.hpp"
#include "ores.ore.core/messaging/ore_file_registrar.hpp"
#include "ores.ore.core/messaging/ore_file_version_history_provider_registrar.hpp"
#include "ores.ore.core/messaging/ore_file_version_registrar.hpp"
#include "ores.ore.service/messaging/currency_config_handler.hpp"
#include "ores.ore.service/messaging/ore_import_execute_handler.hpp"
#include "ores.ore.service/messaging/ore_import_handler.hpp"
#include "ores.ore.service/messaging/pricing_engines_handler.hpp"
#include "ores.ore.service/messaging/report_package_handler.hpp"
#include "ores.ore.service/messaging/run_configuration_handler.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.service/messaging/step_command_stream.hpp"
#include <memory>

namespace ores::ore::service::messaging {

namespace {

using namespace ores::logging;
inline static std::string_view logger_name = "ores.ore.service.messaging.registrar";
static auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

// The history providers live for the whole process, as the subscription that
// serves them holds the registry by reference.
ores::history::service::dispatch_registry& history_registry() {
    static ores::history::service::dispatch_registry instance;
    return instance;
}

}

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             const ores::database::context& ctx,
                             ores::security::jwt::jwt_authenticator signer,
                             ores::nats::service::nats_client outbound_nats,
                             std::string http_base_url,
                             std::string work_dir) {

    std::vector<ores::nats::service::subscription> subs;
    constexpr auto qg = "ores.ore.service";

    // ----------------------------------------------------------------
    // Inbound client handler: validates JWT, dispatches workflow start.
    // ----------------------------------------------------------------
    auto h = std::make_shared<ore_import_handler>(nats, ctx, signer);

    subs.push_back(nats.queue_subscribe(
        ores::ore::messaging::ore_import_request::nats_subject, qg, [h](ores::nats::message msg) {
            h->ore_import(std::move(msg));
        }));

    auto cch = std::make_shared<currency_config_handler>(nats, ctx, signer, outbound_nats);
    subs.push_back(nats.queue_subscribe(
        ores::ore::messaging::export_currency_config_request::nats_subject,
        qg,
        [cch](ores::nats::message msg) { cch->export_currency_config(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        ores::ore::messaging::preview_currency_config_import_request::nats_subject,
        qg,
        [cch](ores::nats::message msg) { cch->preview_currency_config_import(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        ores::ore::messaging::import_currency_config_request::nats_subject,
        qg,
        [cch](ores::nats::message msg) { cch->import_currency_config(std::move(msg)); }));

    auto peh = std::make_shared<pricing_engines_handler>(nats, ctx, signer, outbound_nats);
    subs.push_back(nats.queue_subscribe(
        ores::ore::messaging::export_pricing_engines_request::nats_subject,
        qg,
        [peh](ores::nats::message msg) { peh->export_pricing_engines(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        ores::ore::messaging::preview_pricing_engines_import_request::nats_subject,
        qg,
        [peh](ores::nats::message msg) { peh->preview_pricing_engines_import(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        ores::ore::messaging::import_pricing_engines_request::nats_subject,
        qg,
        [peh](ores::nats::message msg) { peh->import_pricing_engines(std::move(msg)); }));

    // ----------------------------------------------------------------
    // The description of ORE (generated CRUD handlers): the files of an ORE
    // run, their categories and their contents per release, and their history.
    // ----------------------------------------------------------------
    for (auto& sub : ore::messaging::register_ore_file_category_handlers(nats, ctx, signer))
        subs.push_back(std::move(sub));
    for (auto& sub : ore::messaging::register_ore_file_handlers(nats, ctx, signer))
        subs.push_back(std::move(sub));
    for (auto& sub : ore::messaging::register_ore_file_version_handlers(nats, ctx, signer))
        subs.push_back(std::move(sub));
    {
        auto& hist_registry = history_registry();
        ore::messaging::register_ore_file_category_history_provider(hist_registry);
        ore::messaging::register_ore_file_history_provider(hist_registry);
        ore::messaging::register_ore_file_version_history_provider(hist_registry);
        subs.push_back(ores::history::messaging::register_history_handlers(
            nats, hist_registry, "ore", qg, ctx, signer));
    }

    auto rch =
        std::make_shared<run_configuration_handler>(nats, ctx, std::move(signer), outbound_nats);
    subs.push_back(nats.queue_subscribe(
        ores::ore::messaging::import_run_configuration_request::nats_subject,
        qg,
        [rch](ores::nats::message msg) { rch->import_run_configuration(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        ores::ore::messaging::export_run_configuration_request::nats_subject,
        qg,
        [rch](ores::nats::message msg) { rch->export_run_configuration(std::move(msg)); }));

    // ----------------------------------------------------------------
    // Engine-dispatched step handlers: execute and rollback. The report
    // package handler shares the service token client, so its storage calls
    // authenticate as the service.
    // ----------------------------------------------------------------
    auto rph = std::make_shared<report_package_handler>(nats, ctx, http_base_url, outbound_nats);
    auto rci = std::make_shared<run_configuration_import_handler>(nats, outbound_nats);
    auto eh = std::make_shared<ore_import_execute_handler>(
        // Not std::move: the report package handler below takes the same
        // base URL, and moving it here leaves that one empty, so every
        // prepare_ore_package call builds a relative storage URL and fails.
        nats,
        std::move(outbound_nats),
        http_base_url,
        std::move(work_dir));

    // One durable consumer carries all five, and each handler answers its own
    // delivery.
    ores::service::messaging::step_routes routes;
    routes
        .on(ores::ore::messaging::run_configuration_import_execute_request::nats_subject,
            [rci](ores::nats::delivery delivery) { rci->execute(std::move(delivery)); })
        .on(ores::ore::messaging::run_configuration_import_rollback_request::nats_subject,
            [rci](ores::nats::delivery delivery) { rci->rollback(std::move(delivery)); })
        .on(ores::ore::messaging::ore_import_execute_request::nats_subject,
            [eh](ores::nats::delivery delivery) { eh->execute(std::move(delivery)); })
        .on(ores::ore::messaging::ore_import_rollback_request::nats_subject,
            [eh](ores::nats::delivery delivery) { eh->rollback(std::move(delivery)); })
        .on(ores::reporting::messaging::prepare_ore_package_request::nats_subject,
            [rph](ores::nats::delivery delivery) { rph->prepare_package(std::move(delivery)); });
    subs.push_back(ores::service::messaging::serve_step_commands(nats, qg, routes));

    ORES_LOG_SEV(lg(), info) << "Registered " << subs.size() << " ORE service message handlers.";
    return subs;
}

}
