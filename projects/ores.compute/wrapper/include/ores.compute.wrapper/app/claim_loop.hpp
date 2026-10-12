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
#ifndef ORES_COMPUTE_WRAPPER_APP_CLAIM_LOOP_HPP
#define ORES_COMPUTE_WRAPPER_APP_CLAIM_LOOP_HPP

#include <cstdint>
#include "ores.compute.api/messaging/work_protocol.hpp"
#include "ores.nats/service/client.hpp"
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <functional>
#include <mutex>
#include <string>
#include <thread>

namespace ores::compute::wrapper::app {

/**
 * @brief Asks the compute service for work whenever the node is free.
 *
 * A node holds no job it is not running. When it is idle it sends a claim request,
 * and the service replies with the next job for the node's platform, or with none.
 * With a job the loop runs the handler to the end and asks again at once. With none
 * it waits for the doorbell, which the service rings when it queues work, or for the
 * poll interval, spread by random jitter so the nodes do not ask in step. A failed
 * request waits longer each time in a row.
 *
 * The loop runs on one thread and runs one job at a time.
 */
class claim_loop {
public:
    using handler = std::function<void(const ores::compute::messaging::work_assignment_event&)>;

    claim_loop(ores::nats::service::client& nats,
               std::string host_id,
               std::string platform,
               std::chrono::milliseconds poll_interval,
               handler on_job);
    ~claim_loop();
    claim_loop(const claim_loop&) = delete;
    claim_loop& operator=(const claim_loop&) = delete;

    /// Starts the thread. The loop asks for work at once.
    void start();

    /// Ends the loop after the job in flight and joins the thread.
    void stop();

    /// The doorbell: wakes an idle loop so it asks without waiting out the poll.
    void ring();

private:
    void run();
    /// Waits for the delay, the doorbell or a stop, whichever comes first.
    void wait_for(std::chrono::milliseconds delay);

    ores::nats::service::client& nats_;
    std::string host_id_;
    std::string platform_;
    std::chrono::milliseconds poll_interval_;
    handler on_job_;
    std::thread thread_;
    std::mutex mutex_;
    std::condition_variable wake_;
    bool rung_{false};
    std::atomic<bool> stopping_{false};
};

} // namespace ores::compute::wrapper::app

#endif
