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
#include "ores.workflow/messaging/workflow_handler.hpp"
#include "ores.workflow/messaging/workflow_protocol.hpp"

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

    auto wh = std::make_shared<workflow_handler>(
        nats, std::move(ctx), std::move(signer), std::move(outbound_nats));

    subs.push_back(nats.queue_subscribe(
        provision_parties_request::nats_subject, qg,
        [wh](ores::nats::message msg) {
            wh->provision_parties(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        run_report_message::nats_subject, qg,
        [wh](ores::nats::message msg) {
            wh->run_report(std::move(msg));
        }));

    BOOST_LOG_SEV(lg(), debug) << "Registered " << subs.size()
                               << " workflow message handlers.";
    return subs;
}

}
