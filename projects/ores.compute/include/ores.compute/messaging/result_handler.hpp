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
#ifndef ORES_COMPUTE_MESSAGING_RESULT_HANDLER_HPP
#define ORES_COMPUTE_MESSAGING_RESULT_HANDLER_HPP

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
#include "ores.compute/messaging/result_protocol.hpp"
#include "ores.compute/service/result_service.hpp"

namespace ores::compute::messaging {

namespace {
inline auto& result_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.compute.messaging.result_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using namespace ores::logging;

class result_handler {
public:
    result_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(result_handler_lg(), debug)
            << "Handling " << msg.subject;
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::result_service svc(ctx);
        list_results_response resp;
        try {
            if (auto req = decode<list_results_request>(msg)) {
                resp.results = svc.list();
                resp.total_available_count =
                    static_cast<int>(resp.results.size());
            }
        } catch (...) {}
        reply(nats_, msg, resp);
        BOOST_LOG_SEV(result_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void submit(ores::nats::message msg) {
        BOOST_LOG_SEV(result_handler_lg(), debug)
            << "Handling " << msg.subject;
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (auto req = decode<submit_result_request>(msg)) {
            try {
                service::result_service svc(ctx);
                auto existing = svc.find(req->result_id);
                if (!existing) {
                    reply(nats_, msg, submit_result_response{
                        .success = false,
                        .message = "Result not found: " + req->result_id});
                    return;
                }
                auto r = *existing;
                r.server_state = 5;
                r.output_uri = req->output_uri;
                r.received_at = std::chrono::system_clock::now();
                r.outcome = req->outcome;
                stamp(r, ctx);
                svc.save(r);
                reply(nats_, msg,
                    submit_result_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, submit_result_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(result_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(result_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::compute::messaging

#endif
