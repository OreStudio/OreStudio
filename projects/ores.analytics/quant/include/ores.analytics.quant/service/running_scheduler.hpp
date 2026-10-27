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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_ANALYTICS_QUANT_SERVICE_RUNNING_SCHEDULER_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_RUNNING_SCHEDULER_HPP

#include "ores.analytics.quant/service/run_clock.hpp"
#include "ores.analytics.quant/service/tick_scheduler.hpp"
#include <cstdint>
#include <thread>

namespace ores::analytics::quant::service {

/**
 * @brief A scheduler on the steady clock with its loop running, for as long as
 * this object lives.
 *
 * Declare it before anything that adds series to it, so it outlives them.
 */
class running_scheduler final {
public:
    explicit running_scheduler(const domain::market_config& market = {},
                               std::uint64_t run_seed = 42)
        : scheduler(clock_, market, run_seed)
        , loop_([this] { scheduler.run(); }) {}

    running_scheduler(const running_scheduler&) = delete;
    running_scheduler& operator=(const running_scheduler&) = delete;

    ~running_scheduler() {
        scheduler.stop();
        loop_.join();
    }

private:
    steady_run_clock clock_;

public:
    tick_scheduler scheduler;

private:
    std::thread loop_;
};

}

#endif
