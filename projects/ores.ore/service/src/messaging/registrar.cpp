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
#include "ores.service/messaging/step_command_stream.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.ore.api/messaging/ore_import_protocol.hpp"
#include "ores.ore.api/messaging/run_configuration_protocol.hpp"
#include "ores.ore.service/messaging/ore_import_execute_handler.hpp"
#include "ores.ore.service/messaging/ore_import_handler.hpp"
#include "ores.ore.service/messaging/report_package_handler.hpp"
#include "ores.ore.service/messaging/run_configuration_handler.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include <array>
#include <memory>

namespace ores::ore::service::messaging {

namespace {

using namespace ores::logging;
inline static std::string_view logger_name = "ores.ore.service.messaging.registrar";
static auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

}

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             ores::database::context ctx,
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
    static constexpr std::array step_subjects{
        ores::ore::messaging::run_configuration_import_execute_request::nats_subject,
        ores::ore::messaging::run_configuration_import_rollback_request::nats_subject,
        ores::ore::messaging::ore_import_execute_request::nats_subject,
        ores::ore::messaging::ore_import_rollback_request::nats_subject,
        ores::reporting::messaging::prepare_ore_package_request::nats_subject};
    const auto step_stream = ores::service::messaging::ensure_step_stream(nats, qg, step_subjects);
    subs.push_back(ores::service::messaging::subscribe_step_command(
        nats,
        std::string(ores::ore::messaging::run_configuration_import_execute_request::nats_subject),
        qg,
        step_stream,
        [rci](ores::nats::message msg) {
            ores::nats::ack_guard ack(msg);
            rci->execute(std::move(msg));
            ack.accept();
        }));
    subs.push_back(ores::service::messaging::subscribe_step_command(
        nats,
        std::string(ores::ore::messaging::run_configuration_import_rollback_request::nats_subject),
        qg,
        step_stream,
        [rci](ores::nats::message msg) {
            ores::nats::ack_guard ack(msg);
            rci->rollback(std::move(msg));
            ack.accept();
        }));
    auto eh = std::make_shared<ore_import_execute_handler>(
        // Not std::move: the report package handler below takes the same
        // base URL, and moving it here leaves that one empty, so every
        // prepare_ore_package call builds a relative storage URL and fails.
        nats,
        std::move(outbound_nats),
        http_base_url,
        std::move(work_dir));

    subs.push_back(ores::service::messaging::subscribe_step_command(
        nats,
        std::string(ores::ore::messaging::ore_import_execute_request::nats_subject),
        qg,
        step_stream,
        [eh](ores::nats::message msg) {
            ores::nats::ack_guard ack(msg);
            eh->execute(std::move(msg));
            ack.accept();
        }));

    subs.push_back(ores::service::messaging::subscribe_step_command(
        nats,
        std::string(ores::ore::messaging::ore_import_rollback_request::nats_subject),
        qg,
        step_stream,
        [eh](ores::nats::message msg) {
            ores::nats::ack_guard ack(msg);
            eh->rollback(std::move(msg));
            ack.accept();
        }));

    // ----------------------------------------------------------------
    // Report package preparation (workflow step for report execution).
    // ----------------------------------------------------------------
    subs.push_back(ores::service::messaging::subscribe_step_command(
        nats,
        std::string(ores::reporting::messaging::prepare_ore_package_request::nats_subject),
        qg,
        step_stream,
        [rph](ores::nats::message msg) {
            ores::nats::ack_guard ack(msg);
            rph->prepare_package(std::move(msg));
            ack.accept();
        }));

    BOOST_LOG_SEV(lg(), info) << "Registered " << subs.size() << " ORE service message handlers.";
    return subs;
}

}
