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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_TICK_SCHEDULER_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_TICK_SCHEDULER_HPP

#include "ores.analytics.quant/domain/i_arrival_process.hpp"
#include "ores.analytics.quant/domain/market_link.hpp"
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <functional>
#include <limits>
#include <map>
#include <mutex>
#include <optional>
#include <stdexcept>
#include <string>
#include <thread>
#include <vector>

namespace ores::analytics::quant::service {

/**
 * @brief One clock for a whole run: it decides which series ticks next.
 *
 * A feed with its own clock draws a gap from its own rate and sleeps, so it can
 * never see another feed's event. This scheduler holds every series' pending gap,
 * sleeps until the earliest, and calls only that series' body: the superposition
 * of independent streams is exactly the minimum of their gaps.
 *
 * Each series is re-armed from its current rate when it fires, and every series
 * is re-armed once per refresh period. The refresh is what carries the shared
 * factors: the market state and the heat change continuously, and a pending gap
 * drawn before the change would otherwise ignore it. A refresh bounds that
 * staleness to one period instead of leaving it unbounded.
 *
 * The body is the only thing a feed supplies; the scheduler owns the time, the
 * order and the stop.
 */
class tick_scheduler final {
public:
    /// What one series does when its turn comes, given the seconds it waited.
    using stream_body = std::function<std::string(double elapsed_seconds)>;
    using published_hook = std::function<void(std::uint64_t, const std::string&)>;
    using failure_hook = std::function<void(const std::exception&)>;

    /**
     * @param arrivals The series' process, borrowed and outliving the scheduler.
     * @param link The factors this series follows.
     * @param body What the series does when it ticks.
     */
    std::size_t add(ores::analytics::quant::domain::i_arrival_process& arrivals,
                    ores::analytics::quant::domain::market_link link,
                    stream_body body) {
        const auto guard = std::lock_guard(mutex_);
        const auto id = next_id_++;
        streams_.emplace(id, stream{&arrivals, link, std::move(body), 0.0, false});
        waiting_.notify_one();
        return id;
    }

    /// @brief Stop ticking a series. Safe while the scheduler runs.
    void remove(std::size_t id) {
        const auto guard = std::lock_guard(mutex_);
        streams_.erase(id);
    }

    [[nodiscard]] std::size_t size() const {
        const auto guard = std::lock_guard(mutex_);
        return streams_.size();
    }

    /**
     * @brief Advance to the next stream's tick, run its body, and report which
     * stream ticked and how long it waited, or nothing when there is no stream.
     *
     * Public so a test can drive the order without sleeping.
     */
    std::optional<std::pair<std::size_t, double>> step() {
        return tick_once(false, 0.0);
    }

    /**
     * @brief Re-arm every series from its current rate, so a change in the
     * shared factors reaches a gap that was already in flight.
     */
    void refresh() {
        const auto guard = std::lock_guard(mutex_);
        for (auto& [id, s] : streams_)
            arm(s);
    }

    /**
     * @brief Run until stop(), sleeping until each tick is due.
     *
     * @param refresh_period how often every series is re-armed. One period of the
     *        market's owner is the natural choice: beyond it the factors are
     *        stale anyway.
     */
    void run(std::chrono::milliseconds refresh_period = std::chrono::milliseconds(100)) {
        const double period = std::chrono::duration<double>(refresh_period).count();
        while (!stop_flag_.load(std::memory_order_relaxed)) {
            {
                // A run starts before its feeds do, so an empty registry means
                // "nothing to do yet", not "finished".
                std::unique_lock lock(mutex_);
                waiting_.wait_for(lock, refresh_period, [this] {
                    return !streams_.empty() || stop_flag_.load(std::memory_order_relaxed);
                });
                if (stop_flag_.load(std::memory_order_relaxed))
                    return;
                if (streams_.empty())
                    continue;
            }
            tick_once(true, period);
        }
    }

    void stop() {
        stop_flag_.store(true, std::memory_order_relaxed);
    }

    void set_hooks(published_hook on_published, failure_hook on_failed) {
        const auto guard = std::lock_guard(mutex_);
        on_published_ = std::move(on_published);
        on_failed_ = std::move(on_failed);
    }

    [[nodiscard]] std::uint64_t publish_count() const {
        return published_.load(std::memory_order_relaxed);
    }

private:
    std::optional<std::pair<std::size_t, double>> tick_once(bool should_sleep, double period) {
        std::size_t winner = 0;
        double earliest = std::numeric_limits<double>::infinity();
        stream_body body;

        {
            const auto guard = std::lock_guard(mutex_);
            if (streams_.empty())
                return std::nullopt;

            for (auto& [id, s] : streams_) {
                if (!s.armed)
                    arm(s);
                if (s.pending < earliest) {
                    earliest = s.pending;
                    winner = id;
                }
            }

            for (auto& [id, s] : streams_) {
                s.pending -= earliest;
                if (s.pending < 0.0)
                    s.pending = 0.0;
            }

            const auto it = streams_.find(winner);
            if (it == streams_.end())
                return std::nullopt;
            body = it->second.body;
        }

        if (should_sleep) {
            sleep_for(earliest);
            if (stop_flag_.load(std::memory_order_relaxed))
                return std::nullopt;
        }

        {
            // The series may have stopped while the scheduler slept, and a
            // stopped feed has destroyed the object its body points at.
            const auto guard = std::lock_guard(mutex_);
            if (streams_.find(winner) == streams_.end())
                return std::nullopt;
        }

        try {
            const auto summary = body(earliest);
            const auto n = published_.fetch_add(1, std::memory_order_relaxed) + 1;
            if (on_published_)
                on_published_(n, summary);
        } catch (const std::exception& ex) {
            if (on_failed_)
                on_failed_(ex);
        }

        {
            const auto guard = std::lock_guard(mutex_);
            const auto it = streams_.find(winner);
            if (it != streams_.end())
                arm(it->second);

            if (period > 0.0) {
                since_refresh_ += earliest;
                if (since_refresh_ >= period) {
                    for (auto& [id, s] : streams_)
                        arm(s);
                    since_refresh_ = 0.0;
                }
            }
        }
        dispatch_error();
        return std::pair{winner, earliest};
    }

    struct stream final {
        ores::analytics::quant::domain::i_arrival_process* arrivals;
        ores::analytics::quant::domain::market_link link;
        stream_body body;
        double pending;
        bool armed;
    };

    void arm(stream& s) {
        try {
            s.arrivals->set_baseline_multiplier(s.link.rate_multiplier());
            const double gap = s.arrivals->next_gap();
            s.pending = std::isfinite(gap) && gap > 0.0 ? gap : 0.0;
        } catch (const std::exception& ex) {
            // A process that cannot draw a gap is reported and set aside for a
            // second rather than re-armed at once, which would spin the loop.
            defer_error(ex);
            s.pending = 1.0;
        }
        s.armed = true;
    }

    void defer_error(const std::exception& ex) {
        pending_error_ = ex.what();
    }

    void dispatch_error() {
        if (!pending_error_)
            return;
        const auto message = *pending_error_;
        pending_error_.reset();
        if (on_failed_)
            on_failed_(std::runtime_error(message));
    }

    static void sleep_for(double seconds) {
        if (!(seconds > 0.0))
            return;
        std::this_thread::sleep_for(std::chrono::duration_cast<std::chrono::microseconds>(
            std::chrono::duration<double>(seconds)));
    }

    mutable std::mutex mutex_;
    std::map<std::size_t, stream> streams_;
    std::size_t next_id_ = 0;
    std::condition_variable waiting_;
    std::optional<std::string> pending_error_;
    std::atomic<bool> stop_flag_{false};
    std::atomic<std::uint64_t> published_{0};
    published_hook on_published_;
    failure_hook on_failed_;
    double since_refresh_ = 0.0;
};

}

#endif
