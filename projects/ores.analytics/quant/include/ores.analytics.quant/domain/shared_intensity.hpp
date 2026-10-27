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
#include <chrono>
#include <cmath>
#include <cstdint>
#include <deque>
#include <mutex>
#include <stdexcept>
#include <utility>
#include <vector>

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
 * *The heat is shared out, not handed to everyone.* C is an excess event rate for
 * the whole market. Each series enrols its mean rate while it runs, and takes the
 * share of C its rate is of the enrolled total R, so the market's event rate is
 * its base rate plus C whatever the number of series. The loop gain is then
 * jump/decay, and the heat settles while that is below one. Handing every series
 * the whole of C would multiply the gain by the number of series, and the heat
 * would run away once that number passed decay/jump.
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
 *
 * *The owner keeps a reading of the heat.* The owner fades it on a period, and
 * takes a reading while it does, so a screen can draw what jump and decay did
 * rather than infer it from the ticks: a spike and the fall that follows it are
 * the internals of the process. The history is bounded in number, and a read
 * filters it by age; the lock it takes is never on a tick.
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

    /**
     * @brief A series' place in the population the heat is shared over.
     *
     * Holds the series' mean rate in the enrolled total for as long as it lives,
     * so the total follows the series that are running.
     */
    class enrolment final {
    public:
        enrolment() = default;

        enrolment(enrolment&& other) noexcept
            : owner_(std::exchange(other.owner_, nullptr))
            , mean_rate_(other.mean_rate_) {}

        enrolment& operator=(enrolment&& other) noexcept {
            if (this != &other) {
                release();
                owner_ = std::exchange(other.owner_, nullptr);
                mean_rate_ = other.mean_rate_;
            }
            return *this;
        }

        enrolment(const enrolment&) = delete;
        enrolment& operator=(const enrolment&) = delete;

        ~enrolment() {
            release();
        }

    private:
        friend class shared_intensity;

        enrolment(shared_intensity& owner, double mean_rate)
            : owner_(&owner)
            , mean_rate_(mean_rate) {
            owner_->add_rate(mean_rate_);
        }

        void release() noexcept {
            if (owner_ != nullptr)
                owner_->add_rate(-mean_rate_);
            owner_ = nullptr;
        }

        shared_intensity* owner_ = nullptr;
        double mean_rate_ = 0.0;
    };

    /**
     * @brief Enrol a series that runs at @p mean_rate arrivals a second.
     */
    [[nodiscard]] enrolment enrol(double mean_rate) {
        if (!std::isfinite(mean_rate) || mean_rate <= 0.0)
            throw std::invalid_argument(
                "shared_intensity: mean_rate must be finite and strictly positive");
        return enrolment(*this, mean_rate);
    }

    /// @brief The summed mean rate of the enrolled series, per second.
    [[nodiscard]] double population_rate() const {
        return population_rate_.load(std::memory_order_relaxed);
    }

    /**
     * @brief What the heat adds to every enrolled series' multiplier: the heat over
     * the enrolled total, so each series takes the share its rate is of it.
     */
    [[nodiscard]] double rate_share() const {
        const double rate = population_rate();
        return rate > 0.0 ? value() / rate : 0.0;
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

        take_reading(std::chrono::system_clock::now());
    }

    /// @brief The heat before decay, for a test that wants the raw state.
    [[nodiscard]] double excess() const {
        return excess_.load(std::memory_order_relaxed);
    }

    /** @brief One reading of the heat, stamped with when the owner took it. */
    struct heat_reading {
        std::chrono::system_clock::time_point at;
        double heat = 0.0;
    };

    /// @brief How often the owner takes a reading, in milliseconds.
    static constexpr std::chrono::milliseconds sample_period{100};

    /// @brief How many readings the history keeps, which bounds what a read returns.
    static constexpr std::size_t history_capacity = 2048;

    /**
     * @brief The readings taken since @p from, oldest first.
     *
     * A copy under a short lock: the owner appends one reading a period, which
     * is rare enough that a reader never waits on it, and no tick takes the lock.
     */
    [[nodiscard]] std::vector<heat_reading>
    readings_since(std::chrono::system_clock::time_point from) const {
        std::lock_guard lock(history_mu_);
        std::vector<heat_reading> taken;
        taken.reserve(history_.size());
        for (const auto& reading : history_) {
            if (reading.at >= from)
                taken.push_back(reading);
        }
        return taken;
    }

    /// @brief How many readings the history holds, which is what a test asserts on.
    [[nodiscard]] std::size_t reading_count() const {
        std::lock_guard lock(history_mu_);
        return history_.size();
    }

private:
    /** Appends a reading when the last one is a period old, so the sample rate is fixed. */
    void take_reading(std::chrono::system_clock::time_point now) {
        std::lock_guard lock(history_mu_);
        if (!history_.empty() && now - history_.back().at < sample_period)
            return;
        history_.push_back(
            heat_reading{.at = now, .heat = excess_.load(std::memory_order_relaxed)});
        while (history_.size() > history_capacity)
            history_.pop_front();
    }

    void add_rate(double delta) {
        // A sum of doubles added and taken away drifts, and a few rounding errors left
        // behind by series that have all gone would divide the heat by almost nothing.
        // So the sum is reset to exactly zero when the last series leaves, and the
        // count and the sum change together under the lock. Enrolment is rare; reads
        // stay one atomic load.
        std::lock_guard lock(enrol_mu_);
        enrolled_ = delta > 0.0 ? enrolled_ + 1 : enrolled_ - 1;
        const double next =
            enrolled_ == 0 ? 0.0 : population_rate_.load(std::memory_order_relaxed) + delta;
        population_rate_.store(next, std::memory_order_relaxed);
    }

    const double jump_;
    const double decay_;
    std::atomic<double> excess_{0.0};
    std::atomic<double> population_rate_{0.0};
    mutable std::mutex history_mu_;
    std::deque<heat_reading> history_;
    std::mutex enrol_mu_;
    std::uint64_t enrolled_ = 0;
};

}

#endif
