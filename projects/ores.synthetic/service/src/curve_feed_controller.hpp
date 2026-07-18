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
#include <memory>
#include <thread>
#include <utility>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Owns the running ir_curve_feed producers; one tick thread per feed.
 *
 * Phase-1 scope is auto-start only (see the tick-batch-publishing task) — every enabled
 * ir_curve_generation_config is started once at application startup, mirroring
 * feed_controller's auto-started FX feeds. No NATS control-plane (start/stop/list) yet; that is
 * the same manual-control surface feed_controller/market_feed_config_handler already provide for
 * FX and can be extended to curve feeds later without changing this class's own shape.
 */
class curve_feed_controller final {
public:
    void add(std::shared_ptr<ir_curve_feed> feed) {
        auto* raw = feed.get();
        threads_.emplace_back(feed, std::thread([raw] { raw->start(); }));
    }

    std::size_t running_count() const {
        return threads_.size();
    }

    void shutdown() {
        for (auto& [feed, thread] : threads_)
            feed->stop();
        for (auto& [feed, thread] : threads_)
            if (thread.joinable())
                thread.join();
        threads_.clear();
    }

    ~curve_feed_controller() {
        shutdown();
    }

private:
    std::vector<std::pair<std::shared_ptr<ir_curve_feed>, std::thread>> threads_;
};

}

#endif
