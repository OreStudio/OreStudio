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
#ifndef ORES_SYNTHETIC_API_FEEDS_ADAPTER_FAULTS_HPP
#define ORES_SYNTHETIC_API_FEEDS_ADAPTER_FAULTS_HPP

#include "ores.marketdata.api/domain/adapter_limits.hpp"
#include "ores.marketdata.api/messaging/adapter_operation_protocol.hpp"
#include <chrono>
#include <format>
#include <mutex>
#include <optional>
#include <vector>

namespace ores::synthetic::feed {

/**
 * @brief The faults a simulator author can inject, each off by default.
 *
 * Every fault goes through the adapter contract. The connection loss silences the heartbeat
 * and the demand reply, so the service makes the adapter stale. The delay gives every tick a
 * vendor time in the past, so a delay beyond the age limit makes the service drop the tick as
 * stale. The repeated tick sends the last tick again after the new one, so its vendor time is
 * older than the newest and the service drops it as out of order. The out of range value
 * makes every tick carry a value beyond the largest the service accepts. The not entitled
 * rejection answers a demand by rejecting every id as not_entitled.
 */
struct fault_plan final {
    bool connection_lost = false;
    std::chrono::milliseconds tick_delay{0};
    bool repeat_ticks = false;
    bool out_of_range = false;
    bool not_entitled = false;

    /// Whether any fault is on.
    [[nodiscard]] bool any() const {
        return connection_lost || tick_delay.count() > 0 || repeat_ticks || out_of_range ||
               not_entitled;
    }
};

/**
 * @brief The plan in force. The adapter and every generator read it, and a control message
 * replaces it.
 */
class fault_state final {
public:
    void set(const fault_plan& plan) {
        std::lock_guard lock(mu_);
        plan_ = plan;
    }

    [[nodiscard]] fault_plan get() const {
        std::lock_guard lock(mu_);
        return plan_;
    }

private:
    mutable std::mutex mu_;
    fault_plan plan_;
};

/**
 * @brief The ticks a generator publishes for the tick it made, under @p plan.
 *
 * @p previous is the last tick the generator made. It is replaced by @p tick, so a repeated
 * tick is the one before the tick that was just made.
 */
[[nodiscard]] inline std::vector<ores::marketdata::messaging::adapter_tick>
faulted_ticks(const fault_plan& plan,
              ores::marketdata::messaging::adapter_tick tick,
              std::optional<ores::marketdata::messaging::adapter_tick>& previous) {
    const auto made = tick;
    if (plan.out_of_range)
        tick.value =
            std::format("{}", ores::marketdata::domain::adapter_limits::max_abs_value * 10);
    if (plan.tick_delay.count() > 0)
        tick.vendor_time -=
            std::chrono::duration_cast<std::chrono::system_clock::duration>(plan.tick_delay);

    std::vector<ores::marketdata::messaging::adapter_tick> out{tick};
    if (plan.repeat_ticks && previous)
        out.push_back(*previous);
    previous = made;
    return out;
}

}

#endif
