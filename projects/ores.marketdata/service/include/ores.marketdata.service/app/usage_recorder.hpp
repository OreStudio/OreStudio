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
#ifndef ORES_MARKETDATA_SERVICE_APP_USAGE_RECORDER_HPP
#define ORES_MARKETDATA_SERVICE_APP_USAGE_RECORDER_HPP

#if defined(_MSC_VER)
#    pragma once
#endif

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.service/app/delivery_meter.hpp"
#include "ores.marketdata.service/app/demand_calculator.hpp"
#include "ores.marketdata.service/export.hpp"
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <memory>
#include <mutex>
#include <thread>

namespace ores::marketdata::service::app {

/**
 * @brief Writes the usage events of the platform: the intervals that open and close, and the
 * counts of deliveries.
 *
 * @c sync() compares the routes, the subscriptions and the demand now with the intervals that are
 * open, and opens and closes intervals to match. The supervisor calls it after each catalogue
 * read, so the intervals are by-products of control actions and cost nothing at tick time. A
 * thread writes the counts of the delivery meter every flush interval and once more at stop.
 * A count that cannot be written goes back into the meter, so a store that is down loses none.
 *
 * No event holds a price. The rate card of a source prices the events later.
 */
class ORES_MARKETDATA_SERVICE_EXPORT usage_recorder {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.marketdata.service.app.usage_recorder");
        return instance;
    }

public:
    static constexpr std::chrono::seconds flush_interval{30};

    usage_recorder(ores::database::context ctx, std::shared_ptr<delivery_meter> meter);
    ~usage_recorder();

    usage_recorder(const usage_recorder&) = delete;
    usage_recorder& operator=(const usage_recorder&) = delete;

    /// Starts the thread that writes the counts.
    void start();

    /// Opens and closes the subscription and demand intervals to match @p catalogue at @p now.
    void sync(const demand_catalogue& catalogue, std::chrono::system_clock::time_point now);

    /// Writes the counts of the meter. A failure is logged and the counts go back to the meter.
    void flush(std::chrono::system_clock::time_point now);

private:
    void flush_loop();

    ores::database::context ctx_;
    std::shared_ptr<delivery_meter> meter_;
    std::mutex sync_mu_;
    std::mutex wait_mu_;
    std::condition_variable wake_;
    std::atomic<bool> stop_{false};
    std::thread flusher_;
};

}

#endif
