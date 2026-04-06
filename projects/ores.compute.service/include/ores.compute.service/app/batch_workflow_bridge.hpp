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
#ifndef ORES_COMPUTE_SERVICE_APP_BATCH_WORKFLOW_BRIDGE_HPP
#define ORES_COMPUTE_SERVICE_APP_BATCH_WORKFLOW_BRIDGE_HPP

#include <cstdint>
#include <boost/asio/awaitable.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.nats/service/client.hpp"

namespace ores::compute::service::app {

/**
 * @brief Async bridge: fires workflow step_completed_event when a batch
 *        reaches a terminal state.
 *
 * When report_submit_handler defers step completion it writes a row to
 * ores_compute_workflow_batch_links_tbl containing the batch_id and the
 * workflow step context (step_id, instance_id, tenant_id).
 *
 * This poller runs every @p interval_seconds seconds.  For each pending
 * link it loads the batch; if the batch is "closed" it publishes a
 * step_completed_event to workflow.v1.events.step-completed and removes
 * the link row, allowing the workflow engine to advance to the next step.
 *
 * Runs as an async coroutine co-spawned alongside compute_grid_poller.
 */
class batch_workflow_bridge {
private:
    inline static std::string_view logger_name =
        "ores.compute.service.app.batch_workflow_bridge";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    batch_workflow_bridge(std::uint32_t interval_seconds,
                          ores::nats::service::client& nats,
                          ores::database::context ctx);

    boost::asio::awaitable<void> run();

private:
    void poll_once();

    std::uint32_t interval_seconds_;
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
};

}

#endif
