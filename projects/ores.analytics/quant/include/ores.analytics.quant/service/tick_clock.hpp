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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_TICK_CLOCK_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_TICK_CLOCK_HPP

#include "ores.analytics.quant/domain/i_arrival_process.hpp"
#include "ores.analytics.quant/domain/market_regime.hpp"
#include "ores.analytics.quant/export.hpp"
#include <atomic>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <exception>
#include <functional>
#include <string>
#include <thread>

namespace ores::analytics::quant::service {

/**
 * @brief The one clock every producer runs on: an arrival process turned into a
 * stop-responsive sleep loop that counts published batches.
 *
 * Before each tick it asks the arrival process for the gap to the next arrival,
 * in seconds, and sleeps for that gap. The sleep is sliced so stop() is seen
 * promptly, because a gap can be minutes long. The stop check and the
 * catch-and-continue guard around one tick live here once; a producer supplies
 * only what one tick does.
 *
 * The guard covers the whole tick, including the process step: a throw from
 * either is logged through on_failed and the tick skipped, because a tick-loop
 * thread has no caller to propagate to and an uncaught throw would terminate the
 * service, taking every other feed and NATS handler with it.
 *
 * The clock holds no database, messaging or logging code, only the arrival
 * process interface and the standard library, so it keeps the rule of
 * ores.analytics.quant. The arrival process is borrowed and must outlive the
 * clock.
 */
class ORES_ANALYTICS_QUANT_EXPORT tick_clock final {
public:
    /**
     * @param arrivals The process that draws each gap, in seconds.
     * @param market The market-wide factor, or null for a run with no market.
     *        One run shares one market, so every feed follows the same state.
     * @param sensitivity How strongly this feed follows the market, one being the
     *        average series.
     */
    explicit tick_clock(ores::analytics::quant::domain::i_arrival_process& arrivals,
                        ores::analytics::quant::domain::market_regime* market = nullptr,
                        double sensitivity = 1.0)
        : arrivals_(arrivals)
        , market_(market)
        , sensitivity_(sensitivity) {}

    /**
     * @brief Emits one tick and returns a short summary of it for the log line.
     *
     * The argument is the gap the clock just waited, in seconds. A producer
     * turns it into nominal ticks and steps its value process by that much, so
     * an irregular arrival still advances the price by the elapsed time.
     */
    using tick_body = std::function<std::string(double elapsed_seconds)>;
    using published_hook = std::function<void(std::uint64_t, const std::string&)>;
    using failure_hook = std::function<void(const std::exception&)>;

    /**
     * @brief Runs one tick per gap until stop(). Blocks the calling thread.
     *
     * @p on_published receives the 1-based count of successful ticks; a throw
     * from @p body is passed to @p on_failed and the tick is skipped.
     */
    void
    run(const tick_body& body, const published_hook& on_published, const failure_hook& on_failed) {
        using namespace std::chrono;

        constexpr auto slice = milliseconds(100);

        // A gap of zero is legal: a periodic process at an enormous rate. The
        // loop then ticks without sleeping, which is what the caller asked for.
        while (!stop_flag_.load(std::memory_order_relaxed)) {
            // A negative or non-finite gap would spin the loop or convert to an
            // unspecified integer; a zero gap means "tick without waiting",
            // which a periodic process at a very high rate legitimately asks
            // for.
            // The market decides how busy the feed is before it asks for a gap,
            // and the row's sensitivity decides how much of the market it takes.
            if (market_)
                arrivals_.set_baseline_multiplier(market_->multiplier() * sensitivity_);

            const double gap_seconds = arrivals_.next_gap();
            const bool usable = std::isfinite(gap_seconds) && gap_seconds > 0.0;
            auto remaining =
                duration_cast<microseconds>(duration<double>(usable ? gap_seconds : 0.0));

            while (remaining.count() > 0 && !stop_flag_.load(std::memory_order_relaxed)) {
                const auto nap = remaining < slice ? remaining : duration_cast<microseconds>(slice);
                std::this_thread::sleep_for(nap);
                remaining -= nap;
            }

            if (stop_flag_.load(std::memory_order_relaxed))
                break;

            // The state the gap was drawn in held for the gap, so the market
            // moves on by exactly the time that passed.
            if (market_ && std::isfinite(gap_seconds) && gap_seconds > 0.0)
                market_->advance(gap_seconds);

            try {
                const auto summary = body(usable ? gap_seconds : 0.0);
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
    ores::analytics::quant::domain::i_arrival_process& arrivals_;
    ores::analytics::quant::domain::market_regime* market_;
    double sensitivity_;
    std::atomic<bool> stop_flag_{false};
    std::atomic<std::uint64_t> publish_count_{0};
};

}

#endif
