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
#include "ores.compute.service/app/batch_workflow_bridge.hpp"

#include <format>
#include <span>
#include <rfl/json.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/system/system_error.hpp>
#include "ores.database/service/tenant_context.hpp"
#include "ores.compute.core/repository/workflow_batch_link_repository.hpp"
#include "ores.compute.core/service/batch_service.hpp"
#include "ores.workflow.api/messaging/workflow_events.hpp"

namespace ores::compute::service::app {

using namespace ores::logging;

batch_workflow_bridge::batch_workflow_bridge(
    std::uint32_t interval_seconds,
    ores::nats::service::client& nats,
    ores::database::context ctx)
    : interval_seconds_(interval_seconds)
    , nats_(nats)
    , ctx_(std::move(ctx)) {}

void batch_workflow_bridge::poll_once() {
    repository::workflow_batch_link_repository link_repo;
    const auto links = link_repo.find_all(ctx_);

    if (links.empty()) return;

    BOOST_LOG_SEV(lg(), debug)
        << "Batch workflow bridge: checking " << links.size() << " pending link(s)";

    for (const auto& link : links) {
        const auto batch_id = link.batch_id.value();
        try {
            // Load batch under the appropriate tenant context.
            const auto tenant_ctx =
                ores::database::service::tenant_context::with_tenant(
                    ctx_, link.tenant_id);

            ores::compute::service::batch_service batch_svc(std::move(tenant_ctx));
            const auto batch = batch_svc.find(batch_id);

            if (!batch) {
                BOOST_LOG_SEV(lg(), warn)
                    << "Batch not found, removing stale link: " << batch_id;
                link_repo.remove(ctx_, batch_id);
                continue;
            }

            if (batch->status != "closed") {
                BOOST_LOG_SEV(lg(), debug)
                    << "Batch " << batch_id
                    << " still in progress (status=" << batch->status << ")";
                continue;
            }

            // Batch closed: publish step_completed_event.
            ores::workflow::messaging::step_completed_event evt;
            evt.workflow_instance_id = link.workflow_instance_id;
            evt.step_id              = link.workflow_step_id;
            evt.success              = true;
            evt.result_json          = std::format(
                R"({{"success":true,"batch_id":"{}","message":"Compute batch completed"}})",
                batch_id);

            const auto json = rfl::json::write(evt);
            const auto data = std::as_bytes(std::span{json.data(), json.size()});
            nats_.publish(
                std::string(ores::workflow::messaging::step_completed_event::nats_subject),
                data);

            BOOST_LOG_SEV(lg(), info)
                << "Fired step_completed for batch " << batch_id
                << " (step=" << link.workflow_step_id
                << " instance=" << link.workflow_instance_id << ")";

            // Remove the link — idempotency key (step_id) guards against
            // duplicates should the service restart between publish and remove.
            link_repo.remove(ctx_, batch_id);

        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Error processing batch link " << batch_id
                << ": " << e.what();
        }
    }
}

boost::asio::awaitable<void> batch_workflow_bridge::run() {
    BOOST_LOG_SEV(lg(), info)
        << "Batch workflow bridge started. Polling every "
        << interval_seconds_ << "s";

    auto executor = co_await boost::asio::this_coro::executor;
    boost::asio::steady_timer timer(executor);

    try {
        for (;;) {
            try {
                poll_once();
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), warn)
                    << "Batch workflow bridge poll failed: " << e.what();
            }

            timer.expires_after(std::chrono::seconds(interval_seconds_));
            co_await timer.async_wait(boost::asio::use_awaitable);
        }
    } catch (const boost::system::system_error& e) {
        if (e.code() != boost::asio::error::operation_aborted) {
            BOOST_LOG_SEV(lg(), warn)
                << "Batch workflow bridge timer error: " << e.what();
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Batch workflow bridge stopped.";
}

}
