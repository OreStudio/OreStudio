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
#include <algorithm>
#include <rfl/json.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.compute.api/messaging/result_protocol.hpp"
#include "ores.compute.core/service/result_service.hpp"
#include "ores.dq.api/domain/change_reason.hpp"
#include "ores.compute.core/service/workunit_service.hpp"
#include "ores.compute.core/service/batch_service.hpp"

namespace ores::compute::messaging {

namespace {
inline auto& result_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.compute.messaging.result_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::error_reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using ores::service::messaging::error_reply;
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
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
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
        // submit_result is called by trusted wrapper nodes without a JWT;
        // use the service context directly (same pattern as heartbeat handler).
        const auto& ctx = ctx_;
        if (auto req = decode<submit_result_request>(msg)) {
            try {
                service::result_service result_svc(ctx);
                auto existing = result_svc.find(req->result_id);
                if (!existing) {
                    reply(nats_, msg, submit_result_response{
                        .success = false,
                        .message = "Result not found: " + req->result_id});
                    return;
                }
                auto r = *existing;
                r.server_state = 5; // Done
                r.output_uri = req->output_uri;
                r.received_at = std::chrono::system_clock::now();
                r.outcome = req->outcome;
                r.change_reason_code = ores::dq::domain::change_reasons::system_new_record;
                r.change_commentary = "Output received from wrapper";
                stamp(r, ctx);
                result_svc.save(r);

                // Validator: if we now have enough successful results,
                // set the canonical_result_id on the workunit.
                service::workunit_service wu_svc(ctx);
                const auto wu_id_str = boost::uuids::to_string(r.workunit_id);
                const auto wu_opt = wu_svc.find(wu_id_str);
                if (wu_opt && wu_opt->canonical_result_id == boost::uuids::uuid{}) {
                    const auto wu_results =
                        result_svc.list_by_workunit(wu_id_str);
                    const int done = static_cast<int>(
                        std::ranges::count_if(wu_results, [](const auto& res) {
                            return res.server_state == 5;
                        }));
                    if (done >= wu_opt->target_redundancy) {
                        auto wu = *wu_opt;
                        wu.canonical_result_id = r.id;
                        wu.change_reason_code = ores::dq::domain::change_reasons::system_new_record;
                        wu.change_commentary = "Canonical result accepted";
                        stamp(wu, ctx);
                        wu_svc.save(wu);
                        BOOST_LOG_SEV(result_handler_lg(), info)
                            << "Validator: canonical result set for workunit "
                            << wu_id_str;

                        // Assimilator: if all workunits in the batch have a
                        // canonical result, close the batch.
                        const auto batch_id_str =
                            boost::uuids::to_string(wu.batch_id);
                        const auto batch_wus =
                            wu_svc.list_by_batch(batch_id_str);
                        const bool all_done = std::ranges::all_of(
                            batch_wus, [](const auto& w) {
                                return w.canonical_result_id != boost::uuids::uuid{};
                            });
                        if (all_done) {
                            service::batch_service batch_svc(ctx);
                            const auto batch_opt =
                                batch_svc.find(batch_id_str);
                            if (batch_opt) {
                                auto batch = *batch_opt;
                                batch.status = "closed";
                                batch.change_reason_code = ores::dq::domain::change_reasons::system_new_record;
                                batch.change_commentary =
                                    "All workunits complete";
                                stamp(batch, ctx);
                                batch_svc.save(batch);
                                BOOST_LOG_SEV(result_handler_lg(), info)
                                    << "Assimilator: batch "
                                    << batch_id_str << " closed";
                            }
                        }
                    }
                }

                reply(nats_, msg, submit_result_response{.success = true});
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
