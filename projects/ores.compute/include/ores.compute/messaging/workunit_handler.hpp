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
#ifndef ORES_COMPUTE_MESSAGING_WORKUNIT_HANDLER_HPP
#define ORES_COMPUTE_MESSAGING_WORKUNIT_HANDLER_HPP

#include <optional>
#include <span>
#include <stdexcept>
#include <rfl/json.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.compute/messaging/workunit_protocol.hpp"
#include "ores.compute/messaging/work_protocol.hpp"
#include "ores.compute/service/workunit_service.hpp"
#include "ores.compute/service/result_service.hpp"

namespace ores::compute::messaging {

namespace {
inline auto& workunit_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.compute.messaging.workunit_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using namespace ores::logging;

class workunit_handler {
public:
    workunit_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(workunit_handler_lg(), debug)
            << "Handling " << msg.subject;
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::workunit_service svc(ctx);
        list_workunits_response resp;
        try {
            if (auto req = decode<list_workunits_request>(msg)) {
                resp.workunits = svc.list();
                resp.total_available_count =
                    static_cast<int>(resp.workunits.size());
            }
        } catch (...) {}
        reply(nats_, msg, resp);
        BOOST_LOG_SEV(workunit_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(workunit_handler_lg(), debug)
            << "Handling " << msg.subject;
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (auto req = decode<save_workunit_request>(msg)) {
            try {
                service::workunit_service wu_svc(ctx);
                stamp(req->workunit, ctx);
                wu_svc.save(req->workunit);

                // Dispatcher: create target_redundancy result rows (state=2 Unsent)
                // and publish each as a JetStream assignment event.
                service::result_service result_svc(ctx);
                const auto wu_id_str =
                    boost::uuids::to_string(req->workunit.id);
                const auto tenant_id_str = ctx.tenant_id().to_string();
                const auto redundancy = req->workunit.target_redundancy;

                for (int i = 0; i < redundancy; ++i) {
                    domain::result r;
                    r.id = boost::uuids::random_generator()();
                    r.workunit_id = req->workunit.id;
                    r.server_state = 2; // Unsent
                    r.change_reason_code = "system.dispatch";
                    r.change_commentary = "Created on workunit dispatch";
                    stamp(r, ctx);
                    result_svc.save(r);

                    const auto result_id_str = boost::uuids::to_string(r.id);
                    const auto event = work_assignment_event{
                        .result_id = result_id_str,
                        .workunit_id = wu_id_str};
                    const auto json = rfl::json::write(event);
                    const auto* p =
                        reinterpret_cast<const std::byte*>(json.data());
                    nats_.js_publish(
                        "compute.v1.work.assignments." + tenant_id_str,
                        std::span<const std::byte>(p, json.size()));
                    BOOST_LOG_SEV(workunit_handler_lg(), debug)
                        << "Dispatched result " << result_id_str
                        << " for workunit " << wu_id_str;
                }

                reply(nats_, msg, save_workunit_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, save_workunit_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(workunit_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(workunit_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::compute::messaging

#endif
