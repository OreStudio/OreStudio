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
#include "ores.analytics.quant/service/arrival_history.hpp"
#include "ores.analytics.quant/service/run_clock.hpp"
#include "ores.analytics.quant/service/scheduler_trace.hpp"
#include <queue>
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <cstdint>
#include <deque>
#include <exception>
#include <functional>
#include <limits>
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
    /// The simulated seconds since this series last ticked: the market's speed times
    /// the market time that passed. What a price step covers.
    double simulated_seconds = 0.0;
    /// How far the market tilts a price's shape toward its tails now; zero leaves it as
    /// configured.
    double tail_tilt = 0.0;
};

/// What a scheduler needs to know about a series to schedule it.
struct series_spec final {
    /// The stable key of the series, from series_key(source name).
    std::uint64_t series;
    domain::arrival_spec arrival;
    /// How strongly the series follows the market, one being the average.
    double sensitivity = 1.0;
    /// What a reader of the trace calls the series: its source name.
    std::string name;
    /// The group the history counts the series' ticks under, such as its asset class.
    std::string group;
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
    /// The simulated seconds one unit of market time spans.
    double speed;
    /// How often a calm market turns active, and an active one calm, per second.
    double wake_rate;
    double calm_rate;
    double active_multiplier;
    double tail_loading;
};

/**
 * @brief How the loop itself is doing.
 *
 * Lateness is the time the loop fires a tick less the time it was due. It counts the wake-up
 * jitter and any time spent on commands or on earlier ticks of the same batch, because that
 * is how late the tick reached its feed.
 */
struct scheduler_stats final {
    /// Ticks a second, taken at each heat sample and smoothed, so it is a sample period old
    /// at most and holds its last value while the loop is idle.
    double ticks_per_second;
    /// Entries in the heap, live and stale.
    std::size_t heap_size;
    /// Lateness smoothed over about the last ten ticks.
    double lateness_seconds;
    /// The latest tick since the last read of these numbers. Every read starts a new window,
    /// so two readers share it.
    double max_lateness_seconds;
    std::size_t series;
};

/// One running series and the state of its law now.
struct series_state final {
    std::string name;
    /// "periodic", "poisson" or "hawkes".
    std::string mode;
    /// The configured mean rate, a second.
    double rate;
    double sensitivity;
    /// The rate it ticks at now: its law's intensity, times its sensitivity and the pace.
    double effective_rate;
    /// For a periodic law: the fraction of its first gap it started at.
    double phase;
    /// For a Hawkes law: the excess intensity now, above the baseline.
    double excess;
    /// For a Hawkes law: how many ticks a tick begets on average, and how fast that
    /// fades in the series' own operational time. Zero for the other laws.
    double branching_ratio;
    double decay;
};

/// A slice of the trace, with the name of every series it mentions.
struct trace_page final {
    std::vector<trace_event> events;
    std::map<std::uint64_t, std::string> names;
};

/// The heat at one instant, for a screen to draw.
struct heat_reading final {
    std::chrono::system_clock::time_point at;
    double heat;
    /// The ticks a second every series' law asks for at that instant: each law's intensity,
    /// times its sensitivity and the pace. The market's lambda(t).
    double intensity = 0.0;
};

/// How finely the history is cut.
enum class history_resolution { second, tenth };

/// One slot of the history as a reader sees it.
struct history_bin final {
    /// When the slot starts.
    std::chrono::system_clock::time_point at;
    /// The ticks of each group in the slot, by the group's index in the history.
    std::vector<std::uint32_t> counts;
    /// The mean pace over the slot, and the share of it the market was active; both zero
    /// when the loop took no reading in it.
    double pace;
    double active_share;
};

/// The ticks of the last stretch of the run, slot by slot and group by group.
struct market_history final {
    double slot_seconds;
    std::vector<std::string> groups;
    std::vector<history_bin> bins;
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

    /// How many decisions the trace keeps.
    static constexpr std::size_t trace_capacity = 65536;

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

    /**
     * @brief Change how the market's pace moves, from the loop's now on. On return
     * the change is in force and in the trace.
     *
     * A speed change reaches every simulated second from now. A feed that fixed
     * its step from the speed when it started keeps that step until it starts again.
     *
     * @throws std::invalid_argument for numbers the clock refuses, in the caller's
     *         thread, and then nothing changes.
     */
    void configure(const domain::market_config& market);

    /// The market as the loop last left it, which is at most one sample period old.
    [[nodiscard]] market_snapshot snapshot() const;

    /**
     * @brief The ticks of the last @p seconds, by slot and by group.
     *
     * By the second, the history spans an hour; by the tenth, ten minutes. A slot with no
     * tick and no reading is left out.
     */
    [[nodiscard]] market_history history(history_resolution resolution, double seconds) const;

    /// The heat readings taken since @p from, oldest first.
    [[nodiscard]] std::vector<heat_reading>
    readings_since(std::chrono::system_clock::time_point from) const;

    [[nodiscard]] std::size_t size() const;

    /**
     * @brief The decisions of the last @p seconds of the run, oldest first.
     *
     * @param series only this series' events, and every market event, when set.
     * @param limit the most events to return; the newest are kept.
     */
    [[nodiscard]] trace_page
    trace(std::optional<std::uint64_t> series,
          double seconds,
          std::size_t limit = std::numeric_limits<std::size_t>::max()) const;

    /// Every running series and the state of its law now, in name order.
    [[nodiscard]] std::vector<series_state> series_states() const;

    /// The wall time of an instant of the run, as a trace event gives it.
    [[nodiscard]] std::chrono::system_clock::time_point wall_at(double at) const;

    /// The loop's own numbers. Reading them starts a new window for the maximum lateness.
    [[nodiscard]] scheduler_stats stats();

    [[nodiscard]] std::uint64_t run_seed() const {
        return run_seed_;
    }

    /// The simulated seconds one unit of market time spans now.
    [[nodiscard]] double market_speed() const;

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
        /// The market's tau at the series' last event, or when it was added.
        double last_tau;
        /// The market's simulated seconds at the same instant.
        double last_simulated;
        /// The phase a periodic law drew when the series was added.
        double phase;
        /// The index of the series' group in the history.
        std::size_t group;
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
    /// The ticks a second @p s's law asks for now: its intensity, sensitivity and the pace.
    [[nodiscard]] double intensity_of(const series& s) const;
    void record(trace_kind kind,
                std::uint64_t series,
                double operational_gap = 0.0,
                double wall_gap = 0.0,
                double compensated_gap = 0.0);

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
    std::vector<std::string> groups_;
    arrival_history by_second_{1.0, 3600};
    arrival_history by_tenth_{0.1, 6000};
    trace_ring trace_{trace_capacity};
    std::map<std::uint64_t, std::string> names_;
    std::uint64_t fired_since_sample_ = 0;
    double last_sample_ = 0.0;
    double ticks_per_second_ = 0.0;
    double lateness_ = 0.0;
    double max_lateness_ = 0.0;
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
