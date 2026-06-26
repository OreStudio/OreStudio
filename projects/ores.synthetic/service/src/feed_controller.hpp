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
#ifndef ORES_SYNTHETIC_SERVICE_FEED_CONTROLLER_HPP
#define ORES_SYNTHETIC_SERVICE_FEED_CONTROLLER_HPP

#include "fx_spot_feed.hpp"
#include "process_factory.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <boost/uuid/uuid.hpp>
#include <memory>
#include <mutex>
#include <thread>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Shared mutable state for the start/stop NATS handlers.
 *
 * Owned by application::run() as a shared_ptr and passed to the
 * market_feed_config_handler lambdas in the registrar. Owns the running
 * fx_spot_feed and its tick thread.
 *
 * Threading: start() and stop_signal() are called from NATS I/O callbacks
 * and are protected by a mutex. shutdown() is called from the application
 * coroutine after the NATS I/O loop has stopped, so it does not race with
 * handler callbacks.
 *
 * PoC limitation: supports a single active feed. A second start() while
 * one is running returns false.
 */
class feed_controller {
public:
    feed_controller(ores::nats::service::client& nats,
                    boost::uuids::uuid series_id,
                    ores::utility::uuid::tenant_id tenant_id)
        : nats_(nats)
        , series_id_(series_id)
        , tenant_id_(std::move(tenant_id)) {}

    /**
     * @brief Start the feed from the supplied GMM parameters.
     *
     * Joins any previously stopped (but not yet joined) thread before
     * constructing the new feed. Returns false if a feed is already running.
     */
    bool start(const std::string& ore_key,
               std::vector<double> means,
               std::vector<double> stdevs,
               std::vector<double> weights,
               double initial_price,
               double ticks_per_hour) {
        std::lock_guard lock(mu_);
        if (running_.load(std::memory_order_relaxed))
            return false;
        // Join a previously stopped thread before replacing it.
        if (thread_.joinable())
            thread_.join();
        auto process = process_factory::make_gmm_process(
            std::move(means), std::move(stdevs), std::move(weights), initial_price);
        feed_ = std::make_shared<fx_spot_feed>(
            nats_, ore_key, std::move(process), ticks_per_hour, series_id_, tenant_id_);
        thread_ = std::thread([feed = feed_]() {
            feed->start([](const auto& /*tick*/) {});
        });
        running_.store(true, std::memory_order_relaxed);
        return true;
    }

    /**
     * @brief Signal the running feed to stop. Non-blocking.
     *
     * The feed's tick thread will exit after completing its current iteration.
     * The thread is NOT joined here; join happens in shutdown() or the next
     * start() call. Returns false if no feed is running.
     */
    bool stop_signal() {
        std::lock_guard lock(mu_);
        if (!running_.load(std::memory_order_relaxed))
            return false;
        feed_->stop();
        running_.store(false, std::memory_order_relaxed);
        return true;
    }

    /**
     * @brief Stop the feed and join its thread. Safe to call even if not running.
     *
     * Intended for orderly application shutdown. Must only be called after the
     * NATS I/O loop has stopped (no concurrent handler callbacks).
     */
    void shutdown() {
        {
            std::lock_guard lock(mu_);
            if (feed_)
                feed_->stop();
            running_.store(false, std::memory_order_relaxed);
        }
        if (thread_.joinable())
            thread_.join();
    }

    bool is_running() const noexcept {
        return running_.load(std::memory_order_relaxed);
    }

private:
    ores::nats::service::client& nats_;
    boost::uuids::uuid series_id_;
    ores::utility::uuid::tenant_id tenant_id_;

    std::mutex mu_;
    std::shared_ptr<fx_spot_feed> feed_;
    std::thread thread_;
    std::atomic<bool> running_{false};
};

}

#endif
