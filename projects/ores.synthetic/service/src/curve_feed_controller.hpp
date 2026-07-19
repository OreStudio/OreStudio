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
#ifndef ORES_SYNTHETIC_SERVICE_CURVE_FEED_CONTROLLER_HPP
#define ORES_SYNTHETIC_SERVICE_CURVE_FEED_CONTROLLER_HPP

#include "ir_curve_feed.hpp"
#include <map>
#include <memory>
#include <mutex>
#include <string>
#include <thread>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Owns the running ir_curve_feed producers; one tick thread per feed, keyed by
 * source_name -- same shape as feed_controller's own feeds_ map for FX.
 *
 * Supports both auto-start (add(), used at application startup for every enabled config) and
 * on-demand start/stop/list via ir_curve_feed_config_handler's NATS control-plane, mirroring
 * feed_controller/market_feed_config_handler's manual-control surface for FX.
 */
class curve_feed_controller final {
public:
    enum class start_result { started, already_running };

    /**
     * @brief Auto-start path: adds an already-constructed feed and starts its tick thread.
     * Caller is responsible for ensuring source_name uniqueness (add() does not check).
     */
    void add(std::shared_ptr<ir_curve_feed> feed) {
        std::lock_guard lock(mu_);
        const auto source_name = feed->source_name();
        auto* raw = feed.get();
        running_feed rf;
        rf.feed = feed;
        rf.thread = std::thread([raw] { raw->start(); });
        feeds_.emplace(source_name, std::move(rf));
    }

    /**
     * @brief On-demand path: starts a feed by source_name if not already running.
     */
    start_result start(std::shared_ptr<ir_curve_feed> feed) {
        std::lock_guard lock(mu_);
        const auto source_name = feed->source_name();
        if (feeds_.contains(source_name))
            return start_result::already_running;
        auto* raw = feed.get();
        running_feed rf;
        rf.feed = feed;
        rf.thread = std::thread([raw] { raw->start(); });
        feeds_.emplace(source_name, std::move(rf));
        return start_result::started;
    }

    /**
     * @brief Stops one feed by source_name, or all feeds if source_name is empty.
     * Returns the number of feeds stopped.
     */
    std::size_t stop(const std::string& source_name = {}) {
        std::lock_guard lock(mu_);
        if (source_name.empty()) {
            const auto n = feeds_.size();
            for (auto& [_, rf] : feeds_)
                join_and_clear(rf);
            feeds_.clear();
            return n;
        }
        auto it = feeds_.find(source_name);
        if (it == feeds_.end())
            return 0;
        join_and_clear(it->second);
        feeds_.erase(it);
        return 1;
    }

    /** @brief Snapshot of source_names for all currently running feeds. */
    std::vector<std::string> list() const {
        std::lock_guard lock(mu_);
        std::vector<std::string> names;
        names.reserve(feeds_.size());
        for (const auto& [name, _] : feeds_)
            names.push_back(name);
        return names;
    }

    std::size_t running_count() const {
        std::lock_guard lock(mu_);
        return feeds_.size();
    }

    void shutdown() {
        stop();
    }

    ~curve_feed_controller() {
        shutdown();
    }

private:
    struct running_feed {
        std::shared_ptr<ir_curve_feed> feed;
        std::thread thread;
    };

    static void join_and_clear(running_feed& rf) {
        if (rf.feed)
            rf.feed->stop();
        if (rf.thread.joinable())
            rf.thread.join();
    }

    mutable std::mutex mu_;
    std::map<std::string, running_feed> feeds_;
};

}

#endif
