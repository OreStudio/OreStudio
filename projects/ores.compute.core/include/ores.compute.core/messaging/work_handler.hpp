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
#include <chrono>
#include <rfl/json.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.compute.api/messaging/work_protocol.hpp"
#include "ores.compute.core/service/host_service.hpp"
#include "ores.dq.api/domain/change_reason.hpp"
#include "ores.compute.core/service/result_service.hpp"
#include "ores.compute.core/service/workunit_service.hpp"

namespace ores::compute::messaging {

namespace {
inline auto& work_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.compute.messaging.work_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::error_reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using ores::service::messaging::error_reply;
using namespace ores::logging;

class work_handler {
public:
    work_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void pull(ores::nats::message msg) {
        BOOST_LOG_SEV(work_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (auto req = decode<pull_work_request>(msg)) {
            try {
                service::result_service result_svc(ctx);
                auto unsent = result_svc.list_by_state(2); // Unsent
                if (unsent.empty()) {
                    reply(nats_, msg, pull_work_response{
                        .success = false,
                        .message = "No work available"});
                    BOOST_LOG_SEV(work_handler_lg(), debug)
                        << "No unsent results available";
                    return;
                }

                boost::uuids::uuid host_uuid;
                try {
                    host_uuid = boost::lexical_cast<boost::uuids::uuid>(
                        req->host_id);
                } catch (...) {
                    reply(nats_, msg, pull_work_response{
                        .success = false,
                        .message = "Invalid host_id: " + req->host_id});
                    return;
                }

                auto r = unsent.front();
                r.host_id = host_uuid;
                r.server_state = 4; // InProgress
                r.change_reason_code = ores::dq::domain::change_reasons::system_new_record;
                r.change_commentary = "Assigned to host on work.pull";
                stamp(r, ctx);
                result_svc.save(r);

                service::workunit_service wu_svc(ctx);
                const auto wu_id_str = boost::uuids::to_string(r.workunit_id);
                const auto wu_opt = wu_svc.find(wu_id_str);
                if (!wu_opt) {
                    reply(nats_, msg, pull_work_response{
                        .success = false,
                        .message = "Workunit not found: " + wu_id_str});
                    return;
                }

                reply(nats_, msg, pull_work_response{
                    .success = true,
                    .result_id = boost::uuids::to_string(r.id),
                    .workunit_id = wu_id_str,
                    .app_version_id = boost::uuids::to_string(
                        wu_opt->app_version_id),
                    .input_uri = wu_opt->input_uri,
                    .config_uri = wu_opt->config_uri});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(work_handler_lg(), error)
                    << "Pull error: " << e.what();
                reply(nats_, msg, pull_work_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(work_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(work_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void heartbeat(ores::nats::message msg) {
        BOOST_LOG_SEV(work_handler_lg(), debug)
            << "Handling " << msg.subject;
        // Heartbeats are unauthenticated fire-and-forget publishes from wrapper
        // nodes — use the service context directly (no JWT required).
        if (auto req = decode<heartbeat_message>(msg)) {
            try {
                service::host_service svc(ctx_);
                auto existing = svc.find(req->host_id);
                if (existing) {
                    auto h = *existing;
                    h.last_rpc_time = std::chrono::system_clock::now();
                    h.change_reason_code = ores::dq::domain::change_reasons::system_new_record;
                    stamp(h, ctx_);
                    svc.save(h);
                } else {
                    // Auto-register the host on first heartbeat.
                    BOOST_LOG_SEV(work_handler_lg(), info)
                        << "Auto-registering new host from heartbeat: " << req->host_id;
                    domain::host h;
                    try {
                        h.id = boost::lexical_cast<boost::uuids::uuid>(req->host_id);
                    } catch (...) {
                        BOOST_LOG_SEV(work_handler_lg(), warn)
                            << "Invalid host_id in heartbeat: " << req->host_id;
                        return;
                    }
                    h.external_id = req->host_id;
                    h.last_rpc_time = std::chrono::system_clock::now();
                    h.change_reason_code = ores::dq::domain::change_reasons::system_new_record;
                    h.change_commentary = "Auto-registered on first heartbeat";
                    stamp(h, ctx_);
                    svc.save(h);
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
        BOOST_LOG_SEV(work_handler_lg(), debug)
            << "Handling " << msg.subject;
        // Reaper runs with service context (no per-request JWT needed).
        static constexpr auto stale_threshold = std::chrono::minutes(5);
        try {
            service::result_service result_svc(ctx_);
            service::host_service host_svc(ctx_);
            auto in_progress = result_svc.list_by_state(4); // InProgress
            int reaped = 0;
            const auto now = std::chrono::system_clock::now();

            for (auto& r : in_progress) {
                if (r.host_id == boost::uuids::uuid{}) continue;
                const auto host_id_str = boost::uuids::to_string(r.host_id);
                const auto host_opt = host_svc.find(host_id_str);
                if (!host_opt) continue;

                const auto& last_seen = host_opt->last_rpc_time;
                if (last_seen == std::chrono::system_clock::time_point{})
                    continue;
                if (now - last_seen <= stale_threshold) continue;

                r.host_id = boost::uuids::uuid{};
                r.server_state = 2; // Unsent — back in the queue
                r.change_reason_code = ores::dq::domain::change_reasons::system_new_record;
                r.change_commentary = "Host went stale; result re-queued";
                stamp(r, ctx_);
                result_svc.save(r);
                ++reaped;
                BOOST_LOG_SEV(work_handler_lg(), info)
                    << "Reaped result " << boost::uuids::to_string(r.id)
                    << " from stale host " << host_id_str;
            }

            BOOST_LOG_SEV(work_handler_lg(), info)
                << "Reap complete. Reaped " << reaped << " results.";
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(work_handler_lg(), error)
                << "Reap error: " << e.what();
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::compute::messaging

#endif
