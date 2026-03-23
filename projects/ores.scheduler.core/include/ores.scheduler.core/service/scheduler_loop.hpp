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
#pragma once

#include <atomic>
#include <chrono>
#include <map>
#include <memory>
#include <vector>
#include <boost/asio.hpp>
#include <boost/uuid/uuid.hpp>
#include "ores.database/domain/context.hpp"
#include "ores.scheduler.api/domain/job_definition.hpp"
#include "ores.scheduler.core/service/action_handler.hpp"

namespace ores::scheduler::service {

/**
 * @brief In-process scheduler loop that fires jobs based on cron expressions.
 *
 * Loads all active job definitions at startup, then wakes at each minute
 * boundary to evaluate which jobs are due. Each firing creates a job_instance
 * record and delegates execution to the appropriate action_handler.
 *
 * Call reload() to signal the loop that the job table has changed; it will
 * reload definitions on the next tick.
 */
class scheduler_loop final {
public:
    scheduler_loop(database::context system_ctx,
        std::vector<std::unique_ptr<action_handler>> handlers);

    /**
     * @brief Run the scheduler loop.
     *
     * This coroutine runs indefinitely, waking at each minute boundary and
     * firing any due jobs. Returns only when the io_context is stopped.
     */
    boost::asio::awaitable<void> run(boost::asio::io_context& ioc);

    /**
     * @brief Signal the loop to reload job definitions on the next tick.
     */
    void reload();

private:
    boost::asio::awaitable<void> tick(boost::asio::io_context& ioc);
    boost::asio::awaitable<void> fire_job(const domain::job_definition& job);
    void load_jobs();

    database::context system_ctx_;
    std::vector<std::unique_ptr<action_handler>> handlers_;
    std::vector<domain::job_definition> jobs_;
    std::map<boost::uuids::uuid, std::chrono::system_clock::time_point> last_run_;
    std::atomic<bool> needs_reload_{false};
};

} // namespace ores::scheduler::service
