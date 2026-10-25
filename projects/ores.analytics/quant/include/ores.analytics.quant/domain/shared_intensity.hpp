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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_SHARED_INTENSITY_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_SHARED_INTENSITY_HPP

#include <atomic>
#include <cmath>
#include <cstdint>
#include <stdexcept>

namespace ores::analytics::quant::domain {

/**
 * @brief The endogenous half of the market-wide arrival factor: heat that any
 * event anywhere adds to, and that fades on its own.
 *
 * Every event of every series adds @c jump to it, and it decays at @c decay, so
 * the intensity a series sees is
 *
 *   C(t) = C(t0) * exp(-decay * (t - t0)) + the jumps since t0.
 *
 * That is what makes a panic spread rather than be switched on: a cluster in one
 * series raises every other series' rate, which produces more events, which
 * raises it further, until the decay wins.
 *
 * *Reads are one atomic load.* A feed asks for the value on every tick, so the
 * heat is a single relaxed atomic and readers do nothing else. The decay is
 * applied by the owner (see market_regime for the same shape): letting every
 * reader decay it would take a lock, and at ten thousand feeds a lock on the tick
 * path is the bottleneck the factor exists to avoid.
 *
 * *An event is one atomic add.* An event is rare compared with a tick, so the add
 * is affordable; it is the only shared write in the system's hot paths.
 *
 * The stationary mean of the intensity is jump/decay: the rate at which events
 * add heat divided by how fast it fades.
 */
class shared_intensity final {
public:
    /**
     * @param jump the heat a single event adds, in units of a rate.
     * @param decay how fast the heat fades, in units of one over a second.
     */
    explicit shared_intensity(double jump = 0.0, double decay = 1.0)
        : jump_(jump)
        , decay_(decay) {

        if (!std::isfinite(jump_) || jump_ < 0.0)
            throw std::invalid_argument("shared_intensity: jump must be finite and non-negative");
        if (!std::isfinite(decay_) || decay_ <= 0.0)
            throw std::invalid_argument(
                "shared_intensity: decay must be finite and strictly positive");
    }

    /// @brief The intensity in force now. One atomic load, the tick-path read.
    [[nodiscard]] double value() const {
        return excess_.load(std::memory_order_relaxed);
    }

    /// @brief The heat one event adds.
    [[nodiscard]] double jump() const {
        return jump_;
    }

    /// @brief How fast the heat fades, per second.
    [[nodiscard]] double decay() const {
        return decay_;
    }

    /// @brief The intensity the process settles at when events arrive at
    /// @p event_rate per second: the heat added per second over the decay.
    [[nodiscard]] double stationary_mean(double event_rate) const {
        return event_rate * jump_ / decay_;
    }

    /**
     * @brief Record one event. Safe from any thread: one atomic add.
     *
     * The published value moves at once, so a burst is visible to every series on
     * its next tick rather than at the owner's next refresh.
     */
    void record_event() {
        excess_.fetch_add(jump_, std::memory_order_relaxed);
    }

    /**
     * @brief Fade the heat by @p elapsed_seconds. Called by the owner.
     *
     * Only the owner fades it, so the exponential is computed once per period
     * rather than once per reader.
     */
    void fade(double elapsed_seconds) {
        if (!std::isfinite(elapsed_seconds) || elapsed_seconds < 0.0)
            throw std::invalid_argument("shared_intensity: elapsed_seconds must be finite and "
                                        "non-negative");

        // A plain load-then-store would lose an event that lands in between, so
        // the fade is a compare-and-swap loop: an event that arrives during it
        // makes the loop retry and the event survives.
        const double factor = std::exp(-decay_ * elapsed_seconds);
        double current = excess_.load(std::memory_order_relaxed);
        while (!excess_.compare_exchange_weak(
            current, current * factor, std::memory_order_relaxed, std::memory_order_relaxed)) {
        }
    }

    /// @brief The heat before decay, for a test that wants the raw state.
    [[nodiscard]] double excess() const {
        return excess_.load(std::memory_order_relaxed);
    }

private:
    const double jump_;
    const double decay_;
    std::atomic<double> excess_{0.0};
};

}

#endif
