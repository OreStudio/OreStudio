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
#ifndef ORES_COMPUTE_MESSAGING_WORK_HANDLER_HPP
#define ORES_COMPUTE_MESSAGING_WORK_HANDLER_HPP

#include <optional>
#include <stdexcept>
#include <rfl/json.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.compute/messaging/work_protocol.hpp"
#include "ores.compute/service/host_service.hpp"

namespace ores::compute::messaging {

namespace {
inline auto& work_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.compute.messaging.work_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using namespace ores::logging;

class work_handler {
public:
    work_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void pull(ores::nats::message msg) {
        // Stub: full implementation requires JetStream (Phase 4).
        BOOST_LOG_SEV(work_handler_lg(), debug)
            << "Handling " << msg.subject;
        reply(nats_, msg, pull_work_response{
            .success = false,
            .result_id = "not_implemented"});
        BOOST_LOG_SEV(work_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void heartbeat(ores::nats::message msg) {
        BOOST_LOG_SEV(work_handler_lg(), debug)
            << "Handling " << msg.subject;
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (auto req = decode<heartbeat_message>(msg)) {
            try {
                service::host_service svc(ctx);
                auto existing = svc.find(req->host_id);
                if (existing) {
                    auto h = *existing;
                    h.last_rpc_time = std::chrono::system_clock::now();
                    stamp(h, ctx);
                    svc.save(h);
                } else {
                    BOOST_LOG_SEV(work_handler_lg(), warn)
                        << "Heartbeat for unknown host: " << req->host_id;
                }
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(work_handler_lg(), error)
                    << "Heartbeat error for host " << req->host_id
                    << ": " << e.what();
            }
        } else {
            BOOST_LOG_SEV(work_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(work_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void reap(ores::nats::message msg) {
        // Stub: full implementation in Phase 4.
        BOOST_LOG_SEV(work_handler_lg(), debug)
            << "Handling " << msg.subject
            << " (stub — full implementation in Phase 4)";
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::compute::messaging

#endif
