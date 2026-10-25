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
#include <functional>
#include <limits>
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
    void add(ores::analytics::quant::domain::i_arrival_process& arrivals,
             ores::analytics::quant::domain::market_link link,
             stream_body body) {
        streams_.push_back(stream{&arrivals, link, std::move(body), 0.0, false});
    }

    [[nodiscard]] std::size_t size() const {
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
        for (auto& s : streams_)
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
            if (!tick_once(true, period))
                return;
        }
    }

    void stop() {
        stop_flag_.store(true, std::memory_order_relaxed);
    }

    void set_hooks(published_hook on_published, failure_hook on_failed) {
        on_published_ = std::move(on_published);
        on_failed_ = std::move(on_failed);
    }

    [[nodiscard]] std::uint64_t publish_count() const {
        return published_.load(std::memory_order_relaxed);
    }

private:
    std::optional<std::pair<std::size_t, double>> tick_once(bool should_sleep, double period) {
        if (streams_.empty())
            return std::nullopt;

        std::size_t winner = 0;
        double earliest = std::numeric_limits<double>::infinity();
        for (std::size_t i = 0; i < streams_.size(); ++i) {
            if (!streams_[i].armed)
                arm(streams_[i]);
            if (streams_[i].pending < earliest) {
                earliest = streams_[i].pending;
                winner = i;
            }
        }

        if (should_sleep) {
            sleep_for(earliest);
            if (stop_flag_.load(std::memory_order_relaxed))
                return std::nullopt;
        }

        for (auto& s : streams_) {
            s.pending -= earliest;
            if (s.pending < 0.0)
                s.pending = 0.0;
        }

        auto& w = streams_[winner];
        try {
            const auto summary = w.body(earliest);
            const auto n = published_.fetch_add(1, std::memory_order_relaxed) + 1;
            if (on_published_)
                on_published_(n, summary);
        } catch (const std::exception& ex) {
            if (on_failed_)
                on_failed_(ex);
        }

        // The series that fired draws its next gap from its current rate.
        arm(w);

        if (period > 0.0) {
            since_refresh_ += earliest;
            if (since_refresh_ >= period) {
                refresh();
                since_refresh_ = 0.0;
            }
        }
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
            if (on_failed_)
                on_failed_(ex);
            s.pending = 0.0;
        }
        s.armed = true;
    }

    static void sleep_for(double seconds) {
        if (!(seconds > 0.0))
            return;
        std::this_thread::sleep_for(std::chrono::duration_cast<std::chrono::microseconds>(
            std::chrono::duration<double>(seconds)));
    }

    std::vector<stream> streams_;
    std::atomic<bool> stop_flag_{false};
    std::atomic<std::uint64_t> published_{0};
    published_hook on_published_;
    failure_hook on_failed_;
    double since_refresh_ = 0.0;
};

}

#endif
