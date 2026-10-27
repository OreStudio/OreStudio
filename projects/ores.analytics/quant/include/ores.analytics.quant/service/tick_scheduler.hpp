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

#include "ores.analytics.quant/domain/arrival_law.hpp"
#include "ores.analytics.quant/domain/arrival_spec.hpp"
#include "ores.analytics.quant/domain/operational_clock.hpp"
#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/math/rng_stream.hpp"
#include "ores.analytics.quant/service/run_clock.hpp"
#include <queue>
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <cstdint>
#include <deque>
#include <exception>
#include <functional>
#include <map>
#include <mutex>
#include <optional>
#include <string>
#include <thread>
#include <vector>

namespace ores::analytics::quant::service {

/// What a series is told when its turn comes.
struct tick_context final {
    /// Wall seconds since this series last ticked, or since it was added.
    double elapsed_seconds;
    /// When the tick happened, from the run clock.
    std::chrono::system_clock::time_point observed_at;
};

/// What a scheduler needs to know about a series to schedule it.
struct series_spec final {
    /// The stable key of the series, from series_key(source name).
    std::uint64_t series;
    domain::arrival_spec arrival;
    /// How strongly the series follows the market, one being the average.
    double sensitivity = 1.0;
};

/// One tick the scheduler took.
struct fired_tick final {
    std::size_t id;
    /// Seconds since the run started.
    double at;
    /// The market's operational time.
    double tau;
};

/// The market as a reader sees it.
struct market_snapshot final {
    domain::market_state state;
    double multiplier;
    bool pinned;
    double heat;
    double pace;
    double tau;
    double jump;
    double decay;
    double loop_gain;
    double population_rate;
};

/// The heat at one instant, for a screen to draw.
struct heat_reading final {
    std::chrono::system_clock::time_point at;
    double heat;
};

/**
 * @brief The one loop of a run: it owns the market and decides which series
 * ticks next.
 *
 * Every series is scaled by one bracket, so all of them share one operational
 * time, tau, which the market's operational_clock keeps. A series with
 * sensitivity s whose law draws a gap g ticks when tau has moved by g / s. That
 * mark is fixed when the gap is drawn: a heat jump, a pin or an enrolment
 * changes how fast tau moves and moves no mark. So the series wait in one heap
 * keyed on their marks, a tick costs O(log n), and a change in the market
 * reaches every series at once.
 *
 * Only the loop changes the market or the heap. add, remove and pin from other
 * threads are commands the loop applies at its clock's now; remove waits until
 * the loop has applied it, so a feed may destroy what its body points at once
 * remove returns. Before run() starts, and on the loop's own thread, a command
 * applies at once.
 */
class ORES_ANALYTICS_QUANT_EXPORT tick_scheduler final {
public:
    using stream_body = std::function<std::string(const tick_context&)>;
    using published_hook = std::function<void(std::uint64_t, const std::string&)>;
    using failure_hook = std::function<void(const std::exception&)>;

    /// How often the loop reads the heat while nothing else wakes it.
    static constexpr std::chrono::milliseconds sample_period{100};

    /// How many heat readings the history keeps.
    static constexpr std::size_t history_capacity = 2048;

    /**
     * @param clock the run's time, borrowed and outliving the scheduler.
     * @param market how the market's pace moves.
     * @param run_seed the seed every stream of the run is keyed by.
     */
    tick_scheduler(i_run_clock& clock, const domain::market_config& market, std::uint64_t run_seed);

    /**
     * @brief Schedule a series. Its law is built here, so a bad spec throws in
     * the caller's thread.
     */
    std::size_t add(const series_spec& spec, stream_body body);

    /**
     * @brief Stop ticking a series. On return its body is not running and never will.
     *
     * Waits for the loop, which may be running a body, so do not call it while holding a lock
     * that a body takes.
     */
    void remove(std::size_t id);

    /// Pin the market to a state, or hand it back to the chain with nothing. On
    /// return the pin is in force.
    void pin(std::optional<domain::market_state> state);

    /// The market as the loop last left it, which is at most one sample period old.
    [[nodiscard]] market_snapshot snapshot() const;

    /// The heat readings taken since @p from, oldest first.
    [[nodiscard]] std::vector<heat_reading>
    readings_since(std::chrono::system_clock::time_point from) const;

    [[nodiscard]] std::size_t size() const;

    [[nodiscard]] std::uint64_t run_seed() const {
        return run_seed_;
    }

    /**
     * @brief Take the next tick now, without waiting for its time.
     *
     * For a test, which reads the time it was due from the result. Nothing when
     * no series is scheduled.
     */
    std::optional<fired_tick> step();

    /// Run until stop(), waiting on the clock for each tick.
    void run();

    void stop();

    void set_hooks(published_hook on_published, failure_hook on_failed);

    [[nodiscard]] std::uint64_t publish_count() const {
        return published_.load(std::memory_order_relaxed);
    }

private:
    struct series final {
        series_spec spec;
        domain::arrival_law law;
        math::rng_stream stream;
        stream_body body;
        double gap;
        double mark;
        double last_fired;
    };

    struct pending final {
        double mark;
        std::size_t id;
        bool operator>(const pending& other) const {
            return mark != other.mark ? mark > other.mark : id > other.id;
        }
    };

    using command = std::function<void()>;

    /// Apply @p c on the loop, now if the loop is not running or this is it.
    void post(command c, bool wait);
    void drain(std::unique_lock<std::mutex>& lock);

    [[nodiscard]] double seconds_at(i_run_clock::time_point t) const;
    [[nodiscard]] i_run_clock::time_point time_at(double seconds) const;
    void advance_to(double seconds);
    void schedule(std::size_t id, series& s);
    void sample(i_run_clock::time_point t);
    void rebuild_heap();

    /// The run time at which the next series is due, or nothing.
    [[nodiscard]] std::optional<double> next_due();

    /// Fire the series at the top of the heap. Releases the lock around its body.
    std::optional<fired_tick> fire(std::unique_lock<std::mutex>& lock);

    i_run_clock& clock_;
    i_run_clock::time_point origin_;
    std::uint64_t run_seed_;

    mutable std::mutex mutex_;
    domain::operational_clock market_;
    std::map<std::size_t, series> series_;
    std::priority_queue<pending, std::vector<pending>, std::greater<>> heap_;
    std::deque<heat_reading> history_;
    std::vector<command> commands_;
    std::condition_variable applied_;
    std::uint64_t applied_count_ = 0;
    std::uint64_t posted_count_ = 0;
    bool running_ = false;
    std::thread::id loop_thread_;
    std::size_t next_id_ = 0;
    /// Marks in the heap that belong to removed series.
    std::size_t stale_ = 0;

    std::atomic<bool> stop_flag_{false};
    std::atomic<std::uint64_t> published_{0};
    published_hook on_published_;
    failure_hook on_failed_;
};

}

#endif
