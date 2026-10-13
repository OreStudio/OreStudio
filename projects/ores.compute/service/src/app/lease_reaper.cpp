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
#include "ores.compute.service/app/lease_reaper.hpp"
#include "ores.compute.api/domain/result.hpp"
#include "ores.compute.api/messaging/work_protocol.hpp"
#include "ores.compute.core/repository/work_ledger_repository.hpp"
#include "ores.compute.core/service/result_service.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.dq.api/domain/change_reason_codes.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <boost/asio/steady_timer.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/system/system_error.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <string>

namespace ores::compute::service::app {

using namespace ores::logging;

lease_reaper::lease_reaper(std::uint32_t interval_seconds,
                           ores::nats::service::client& nats,
                           ores::database::context ctx)
    : interval_seconds_(interval_seconds)
    , nats_(nats)
    , ctx_(std::move(ctx)) {}

void lease_reaper::sweep() {
    repository::work_ledger_repository ledger;

    const auto returned = ledger.requeue_expired(ctx_);
    if (returned > 0) {
        BOOST_LOG_SEV(lg(), warn) << returned
                                  << " job(s) lost their lease and went back to the queue";
        // The jobs wait again, so idle nodes are told as for new work.
        const ores::compute::messaging::work_available_event bell{.queued =
                                                                      static_cast<int>(returned)};
        nats_.publish(std::string(ores::compute::messaging::work_available_event::nats_subject),
                      ores::nats::default_wire_codec().encode(bell));
    }

    end_exhausted();
}

void lease_reaper::end_exhausted() {
    repository::work_ledger_repository ledger;
    for (const auto& entry : ledger.exhausted(ctx_)) {
        const auto result_id = boost::uuids::to_string(entry.result_id);
        try {
            const auto tenant_ctx = ores::database::service::tenant_context::with_tenant(
                ctx_, boost::uuids::to_string(entry.work_tenant_id));
            ores::compute::service::result_service result_svc(tenant_ctx);
            const auto result = result_svc.get_result(entry.result_id);

            // Only a result still unsent or in progress is ended. A node that died between the
            // claim and the mark leaves it unsent. One that was submitted, or retired, after
            // the reaper read the entry keeps its state, and only the entry goes.
            if (result && (result->server_state == 2 || result->server_state == 4)) {
                auto ended = *result;
                // Done, with outcome NoReply.
                ended.server_state = 5;
                ended.outcome = 4;
                ended.received_at = std::chrono::system_clock::now();
                ended.error_message = "The lease ended with no reply after " +
                                      std::to_string(entry.attempts) + " attempts.";
                ended.change_reason_code = ores::dq::domain::change_reasons::system_new_record;
                ended.change_commentary = ended.error_message;
                ores::service::messaging::stamp(ended, tenant_ctx);
                result_svc.save_result(ended);
                BOOST_LOG_SEV(lg(), error)
                    << "Result " << result_id << " ended with no reply: " << ended.error_message;
            }
            ledger.remove_expired(ctx_, entry.result_id);
        } catch (const std::exception& e) {
            // The entry stays, and the next sweep tries again.
            BOOST_LOG_SEV(lg(), error)
                << "Could not end the expired job " << result_id << ": " << e.what();
        }
    }
}

boost::asio::awaitable<void> lease_reaper::run() {
    BOOST_LOG_SEV(lg(), info) << "Lease reaper started. Sweeping every " << interval_seconds_
                              << "s";

    auto executor = co_await boost::asio::this_coro::executor;
    boost::asio::steady_timer timer(executor);

    try {
        for (;;) {
            try {
                sweep();
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), warn) << "Lease reaper sweep failed: " << e.what();
            }

            timer.expires_after(std::chrono::seconds(interval_seconds_));
            co_await timer.async_wait(boost::asio::use_awaitable);
        }
    } catch (const boost::system::system_error& e) {
        if (e.code() != boost::asio::error::operation_aborted) {
            BOOST_LOG_SEV(lg(), warn) << "Lease reaper timer error: " << e.what();
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Lease reaper stopped.";
}

}
