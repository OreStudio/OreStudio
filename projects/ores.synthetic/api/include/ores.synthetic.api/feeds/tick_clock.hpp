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
#ifndef ORES_SYNTHETIC_API_FEEDS_TICK_CLOCK_HPP
#define ORES_SYNTHETIC_API_FEEDS_TICK_CLOCK_HPP

#include <atomic>
#include <chrono>
#include <cstdint>
#include <exception>
#include <functional>
#include <string>
#include <thread>

namespace ores::synthetic::feed {

/**
 * @brief The one fixed-mode tick clock every producer runs on: a ticks-per-hour
 * rate turned into a stop-responsive sleep loop that counts published batches.
 *
 * The period, the sleep sliced so stop() is observed promptly (a period can be
 * minutes long), the stop check and the catch-and-continue guard around one
 * tick live here once. A producer supplies only what one tick does: advance its
 * process and publish its datum(s).
 *
 * The guard covers the whole tick, including the process step: a throw from
 * either is logged through on_failed and the tick skipped, because a tick-loop
 * thread has no caller to propagate to and an uncaught throw would terminate
 * the service, taking every other feed and NATS handler with it.
 */
class tick_clock final {
public:
    explicit tick_clock(double ticks_per_hour) : ticks_per_hour_(ticks_per_hour) {}

    /// @brief Emits one tick and returns a short summary of it for the log line.
    using tick_body = std::function<std::string()>;
    using published_hook = std::function<void(std::uint64_t, const std::string&)>;
    using failure_hook = std::function<void(const std::exception&)>;

    /**
     * @brief Runs one tick per period until stop(). Blocks the calling thread.
     *
     * @p on_published receives the 1-based count of successful ticks; a throw
     * from @p body is passed to @p on_failed and the tick is skipped.
     */
    void run(const tick_body& body,
             const published_hook& on_published,
             const failure_hook& on_failed) {
        using namespace std::chrono;

        const auto period_us =
            duration_cast<microseconds>(hours(1)) / static_cast<long long>(ticks_per_hour_);

        constexpr auto slice = milliseconds(100);

        // stop_flag_ is false from the member initialiser and is deliberately
        // not reset here: a stop() that arrives between thread spawn and this
        // point would otherwise be clobbered, and the loop and its join() would
        // hang forever.
        while (!stop_flag_.load(std::memory_order_relaxed)) {
            auto remaining = duration_cast<microseconds>(period_us);
            while (remaining.count() > 0 && !stop_flag_.load(std::memory_order_relaxed)) {
                const auto nap =
                    remaining < slice ? remaining : duration_cast<microseconds>(slice);
                std::this_thread::sleep_for(nap);
                remaining -= nap;
            }

            if (stop_flag_.load(std::memory_order_relaxed))
                break;

            try {
                const auto summary = body();
                const auto n = publish_count_.fetch_add(1, std::memory_order_relaxed) + 1;
                on_published(n, summary);
            } catch (const std::exception& ex) {
                on_failed(ex);
            }
        }
    }

    void stop() {
        stop_flag_.store(true, std::memory_order_relaxed);
    }

    [[nodiscard]] std::uint64_t publish_count() const {
        return publish_count_.load(std::memory_order_relaxed);
    }

private:
    double ticks_per_hour_;
    std::atomic<bool> stop_flag_{false};
    std::atomic<std::uint64_t> publish_count_{0};
};

}
#endif
