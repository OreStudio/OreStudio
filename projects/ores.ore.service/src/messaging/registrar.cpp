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

#include <memory>
#include "ores.logging/make_logger.hpp"
#include "ores.ore.api/messaging/ore_import_protocol.hpp"
#include "ores.ore.api/messaging/ore_import_engine_protocol.hpp"
#include "ores.ore.service/messaging/ore_import_handler.hpp"
#include "ores.ore.service/messaging/ore_import_execute_handler.hpp"

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
    auto h = std::make_shared<ore_import_handler>(
        nats, ctx, std::move(signer));

    subs.push_back(nats.queue_subscribe(
        ores::ore::messaging::ore_import_request::nats_subject, qg,
        [h](ores::nats::message msg) {
            h->ore_import(std::move(msg));
        }));

    // ----------------------------------------------------------------
    // Engine-dispatched step handlers: execute and rollback.
    // ----------------------------------------------------------------
    auto eh = std::make_shared<ore_import_execute_handler>(
        nats, std::move(outbound_nats),
        std::move(http_base_url), std::move(work_dir));

    subs.push_back(nats.queue_subscribe(
        std::string(ores::ore::messaging::ore_import_execute_request::nats_subject), qg,
        [eh](ores::nats::message msg) {
            eh->execute(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(ores::ore::messaging::ore_import_rollback_request::nats_subject), qg,
        [eh](ores::nats::message msg) {
            eh->rollback(std::move(msg));
        }));

    BOOST_LOG_SEV(lg(), info) << "Registered " << subs.size()
                              << " ORE service message handlers.";
    return subs;
}

}
