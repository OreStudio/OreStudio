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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_UTILITY_CONCURRENCY_RETRY_STRATEGY_HPP
#define ORES_UTILITY_CONCURRENCY_RETRY_STRATEGY_HPP

#include <algorithm>
#include <chrono>

namespace ores::utility::concurrency {

/**
 * @brief Stateful exponential-backoff retry calculator.
 *
 * Tracks the number of consecutive failures and the time of the last
 * successful start, and computes the next wait interval as:
 *
 *   delay = min(initial_delay * 2^n, max_delay)
 *
 * where n is the current failure count.  If the elapsed time since the
 * last start exceeds reset_threshold, the failure counter is reset before
 * computing the delay — a long-running process that has an occasional
 * transient crash restarts quickly instead of incurring a large penalty.
 *
 * This class is a pure chrono utility: it has no knowledge of processes,
 * services, or any external system.  All logging and policy decisions
 * belong to the caller.
 */
class retry_strategy {
public:
    struct config {
        std::chrono::seconds initial_delay{5};
        std::chrono::seconds max_delay{300};      ///< 5-minute cap
        std::chrono::seconds reset_threshold{60}; ///< uptime that resets the counter
    };

    retry_strategy() = default;
    explicit retry_strategy(config cfg) : config_(cfg) {}

    /**
     * @brief Record a successful start.
     *
     * Must be called immediately after each successful launch so that
     * on_failure() can measure elapsed uptime accurately.
     */
    void on_start() {
        last_start_ = std::chrono::steady_clock::now();
    }

    /**
     * @brief Record a failure and compute the next backoff delay.
     *
     * If the elapsed time since on_start() exceeds config.reset_threshold,
     * the failure counter is reset to 0 before computing the delay.
     * The counter is then incremented and the delay is returned.
     *
     * @return Duration the caller should wait before the next attempt.
     */
    std::chrono::seconds on_failure() {
        const auto uptime = std::chrono::steady_clock::now() - last_start_;
        if (uptime >= config_.reset_threshold)
            count_ = 0;

        const auto delay = compute_delay();
        ++count_;
        return delay;
    }

    /**
     * @brief Returns whether the failure count has reached or exceeded @p max.
     *
     * Call before on_failure() to decide if the threshold has been crossed
     * on this failure, before the counter is incremented.
     */
    bool exceeded(int max) const { return count_ >= max; }

    /** @brief Returns the current consecutive failure count. */
    int failure_count() const { return count_; }

    /**
     * @brief Restore a previously saved failure count without resetting state.
     *
     * Used when a new entry must inherit an accumulated count from a prior
     * one (e.g. after a process is relaunched and the entry is replaced).
     */
    void set_failure_count(int n) { count_ = n; }

    /** @brief Reset the counter and start-time record to initial state. */
    void reset() {
        count_ = 0;
        last_start_ = {};
    }

private:
    std::chrono::seconds compute_delay() const {
        // Cap exponent at 6: initial(5) * 2^6 = 320s > max(300s), so the
        // min() clamp always applies from failure 6 onwards.
        const int exp = std::min(count_, 6);
        const auto delay = std::chrono::seconds(
            config_.initial_delay.count() * (1 << exp));
        return std::min(delay, config_.max_delay);
    }

    config config_{};
    int count_ = 0;
    std::chrono::steady_clock::time_point last_start_{};
};

} // namespace ores::utility::concurrency

#endif
