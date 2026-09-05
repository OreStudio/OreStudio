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
#ifndef ORES_COMPUTE_MESSAGING_RESULT_SUBMIT_HANDLER_HPP
#define ORES_COMPUTE_MESSAGING_RESULT_SUBMIT_HANDLER_HPP

#include "ores.compute.api/messaging/work_protocol.hpp"
#include "ores.compute.core/export.hpp"
#include "ores.compute.core/service/batch_service.hpp"
#include "ores.compute.core/service/result_service.hpp"
#include "ores.compute.core/service/workunit_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/change_reason_codes.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <optional>

namespace ores::compute::messaging {

namespace {
inline auto& result_submit_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.compute.messaging.result_submit_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using namespace ores::logging;

/**
 * @brief Receives finished jobs from trusted wrapper nodes.
 *
 * Wrapper nodes hold no user JWT, so the generated save_result flow (gated
 * on the request session) rejects them. Like the heartbeat, this channel is
 * trusted at the transport layer and uses the service context directly.
 *
 * This class must stay hand-written: the per-entity result handler is
 * regenerated from the entity model on every bind, and the original submit
 * path was dropped that way when the result entity was bound to profiles.
 * The terminal half of the result lifecycle lives here: mark the result
 * Done, accept the canonical result once the redundancy target is met, and
 * close the batch once every workunit has a canonical result.
 */
class ORES_COMPUTE_CORE_EXPORT result_submit_handler {
public:
    result_submit_handler(ores::nats::service::client& nats, ores::database::context ctx)
        : nats_(nats)
        , ctx_(std::move(ctx)) {}

    void submit(ores::nats::message msg) {
        BOOST_LOG_SEV(result_submit_handler_lg(), debug) << "Handling " << msg.subject;
        if (auto req = decode<submit_result_request>(msg)) {
            try {
                service::result_service result_svc(ctx_);
                auto existing = result_svc.get_result(req->result_id);
                if (!existing) {
                    reply(nats_,
                          msg,
                          submit_result_response{.success = false,
                                                 .message = "Result not found: " + req->result_id});
                    return;
                }

                auto r = *existing;
                r.server_state = 5; // Done
                r.output_uri = req->output_uri;
                r.received_at = std::chrono::system_clock::now();
                r.outcome = req->outcome;
                if (!req->host_id.empty()) {
                    try {
                        r.host_id = boost::lexical_cast<boost::uuids::uuid>(req->host_id);
                    } catch (const boost::bad_lexical_cast& e) {
                        BOOST_LOG_SEV(result_submit_handler_lg(), warn)
                            << "Invalid host_id in submit_result_request: " << req->host_id << " ("
                            << e.what() << ")";
                    }
                }
                r.error_message = req->error_message;
                r.change_reason_code = ores::dq::domain::change_reasons::system_new_record;
                r.change_commentary =
                    req->error_message.empty() ? "Output received from wrapper" : req->error_message;
                stamp(r, ctx_);
                result_svc.save_result(r);

                // Validator: once the redundancy target is met, accept the
                // canonical result on the workunit. The shell drain watch
                // treats a set canonical_result_id as the terminal state.
                service::workunit_service wu_svc(ctx_);
                const auto wu_id_str = boost::uuids::to_string(r.workunit_id);
                const auto wu_opt = wu_svc.get_workunit(wu_id_str);
                if (wu_opt && wu_opt->canonical_result_id == boost::uuids::uuid{}) {
                    const auto wu_results = result_svc.list_results_by_workunit_id(wu_id_str, 0, 1000);
                    const int done = static_cast<int>(std::ranges::count_if(
                        wu_results, [](const auto& res) { return res.server_state == 5; }));
                    if (done >= wu_opt->target_redundancy) {
                        auto wu = *wu_opt;
                        wu.canonical_result_id = r.id;
                        wu.change_reason_code = ores::dq::domain::change_reasons::system_new_record;
                        wu.change_commentary = "Canonical result accepted";
                        stamp(wu, ctx_);
                        wu_svc.save_workunit(wu);
                        BOOST_LOG_SEV(result_submit_handler_lg(), info)
                            << "Validator: canonical result set for workunit " << wu_id_str;

                        // Assimilator: close the batch once every workunit has
                        // a canonical result. The workflow bridge publishes
                        // step_completed_event when it sees the closed status.
                        const auto batch_id_str = boost::uuids::to_string(wu.batch_id);
                        const auto batch_wus = wu_svc.list_workunits_by_batch_id(batch_id_str, 0, 1000);
                        const bool all_done = std::ranges::all_of(batch_wus, [](const auto& w) {
                            return w.canonical_result_id != boost::uuids::uuid{};
                        });
                        if (all_done) {
                            service::batch_service batch_svc(ctx_);
                            const auto batch_opt = batch_svc.get_batch(batch_id_str);
                            if (batch_opt) {
                                auto batch = *batch_opt;
                                batch.status = "closed";
                                batch.change_reason_code =
                                    ores::dq::domain::change_reasons::system_new_record;
                                batch.change_commentary = "All workunits complete";
                                stamp(batch, ctx_);
                                batch_svc.save_batch(batch);
                                BOOST_LOG_SEV(result_submit_handler_lg(), info)
                                    << "Assimilator: batch " << batch_id_str << " closed";
                            }
                        }
                    }
                }

                reply(nats_, msg, submit_result_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(result_submit_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, submit_result_response{.success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(result_submit_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            reply(nats_,
                  msg,
                  submit_result_response{.success = false, .message = "Failed to decode request"});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
};

} // namespace ores::compute::messaging

#endif
