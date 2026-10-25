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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_MARKET_REGIME_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_MARKET_REGIME_HPP

#include <atomic>
#include <chrono>
#include <cstdint>
#include <cmath>
#include <optional>
#include <random>
#include <stdexcept>

namespace ores::analytics::quant::domain {

/**
 * @brief Whether the market is calm or busy: the exogenous half of the
 * market-wide arrival factor.
 *
 * A two-state continuous-time Markov chain. While calm it waits an exponential
 * time with mean 1/wake_rate before turning active, and while active it waits
 * one with mean 1/calm_rate before turning calm again. The long-run share of
 * time spent active is therefore wake_rate / (wake_rate + calm_rate).
 *
 * *Readers cost one atomic load.* Every feed asks for the multiplier on every
 * tick, so the published view — the state and the multiplier — is two relaxed
 * atomics that readers load without a lock, a shared write or a cache-line
 * handover. Tens of thousands of feeds can read it at once.
 *
 * *One owner advances the chain.* The chain's own state (how long the current
 * state has left, the random engine) belongs to a single caller, the market's
 * thread, which calls advance_to_now() a few times a second. That caller is the
 * only writer of the chain and the only one that publishes, so the RNG is never
 * shared and no reader ever waits.
 *
 * A scenario can pin either state. Pinning stores an atomic the owner reads, and
 * publishes at once so the pin is visible without waiting for the next tick.
 *
 * Tests drive the chain deterministically through advance(seconds) instead of the
 * wall clock, so an occupancy measurement does not take real time.
 */
class market_regime final {
public:
    enum class state { calm, active };

    /**
     * @param wake_rate the rate at which a calm market turns active, per second.
     * @param calm_rate the rate at which an active market turns calm, per second.
     * @param active_multiplier how much the state lifts a baseline while active.
     * @param seed the random seed, so a run reproduces.
     *
     * The calm multiplier is not a parameter. It follows from the other three so
     * that the long-run mean of the factor is exactly one: a busier market must
     * not quietly raise every configured mean rate, it must only redistribute
     * that rate in time. The three therefore have to allow it — an active
     * multiplier at or above the reciprocal of the active share leaves no room
     * for a positive calm one, and is refused.
     */
    market_regime(double wake_rate = 1.0 / 600.0,
                  double calm_rate = 1.0 / 300.0,
                  double active_multiplier = 2.0,
                  std::uint32_t seed = 42)
        : wake_rate_(wake_rate)
        , calm_rate_(calm_rate)
        , active_multiplier_(active_multiplier)
        , calm_multiplier_(calm_multiplier_for(wake_rate, calm_rate, active_multiplier))
        , rng_(seed)
        , exponential_(1.0) {

        if (!std::isfinite(wake_rate_) || wake_rate_ <= 0.0)
            throw std::invalid_argument("market_regime: wake_rate must be strictly positive");
        if (!std::isfinite(calm_rate_) || calm_rate_ <= 0.0)
            throw std::invalid_argument("market_regime: calm_rate must be strictly positive");
        if (!std::isfinite(active_multiplier_) || active_multiplier_ <= 0.0)
            throw std::invalid_argument(
                "market_regime: active_multiplier must be strictly positive");
        remaining_ = dwell_time();
        publish(state_);
    }

    /// @brief The state in force now. One atomic load.
    [[nodiscard]] state current() const {
        return static_cast<state>(published_state_.load(std::memory_order_relaxed));
    }

    /// @brief The multiplier in force now. One atomic load, the tick-path read.
    [[nodiscard]] double multiplier() const {
        return published_multiplier_.load(std::memory_order_relaxed);
    }

    /// @brief Whether a scenario has pinned the state.
    [[nodiscard]] bool is_pinned() const {
        return pinned_.load(std::memory_order_relaxed) != automatic;
    }

    /// @brief The share of time the chain spends active in the long run.
    [[nodiscard]] double long_run_active_share() const {
        return wake_rate_ / (wake_rate_ + calm_rate_);
    }

    /// @brief The long-run mean of the factor, which is one by construction.
    [[nodiscard]] double long_run_mean_multiplier() const {
        const double p = long_run_active_share();
        return p * active_multiplier_ + (1.0 - p) * calm_multiplier_;
    }

    /**
     * @brief Pin the state, whatever the chain says, until unpin().
     *
     * Safe to call from a control thread while the owner advances: the pin is an
     * atomic the owner reads, and the published view moves at once so the change
     * is visible without waiting for the next advance.
     */
    void pin(state s) {
        pinned_.store(s == state::active ? active : calm, std::memory_order_relaxed);
        // Publish the pinned state's own multiplier: the chain's state is not
        // read here, because the owner may be writing it.
        publish(s);
    }

    /// @brief Hand control back to the chain, which resumes from wherever it was.
    ///
    /// The owner republishes on its next advance, within one period. Nothing is
    /// published here: the chain's own state belongs to the owner.
    void unpin() { pinned_.store(automatic, std::memory_order_relaxed); }

    /**
     * @brief Advance to now, by the time the wall clock reports since the last
     * call. Called by the one owner of the chain, a few times a second.
     */
    void advance_to_now() {
        const auto now = std::chrono::steady_clock::now();
        if (last_advanced_) {
            const auto elapsed =
                std::chrono::duration<double>(now - *last_advanced_).count();
            advance_chain(elapsed);
        }
        last_advanced_ = now;
    }

    /**
     * @brief Advance by @p elapsed_seconds, taking as many transitions as fit.
     *
     * Deterministic: a test uses this instead of the wall clock. A pinned chain
     * does not transition.
     */
    void advance(double elapsed_seconds) {
        if (!std::isfinite(elapsed_seconds) || elapsed_seconds < 0.0)
            throw std::invalid_argument("market_regime: elapsed_seconds must be finite and "
                                        "non-negative");
        advance_chain(elapsed_seconds);
    }

    /// @brief How long the current state still holds, in seconds. Owner-only.
    [[nodiscard]] double remaining_seconds() const { return remaining_; }

private:
    static constexpr int automatic = -1;
    static constexpr int calm = 0;
    static constexpr int active = 1;

    void publish(state s) {
        published_state_.store(s == state::active ? active : calm,
                               std::memory_order_relaxed);
        published_multiplier_.store(s == state::active ? active_multiplier_ : calm_multiplier_,
                                    std::memory_order_relaxed);
    }

    static double calm_multiplier_for(double wake_rate, double calm_rate, double active) {
        if (!std::isfinite(wake_rate) || wake_rate <= 0.0 || !std::isfinite(calm_rate) ||
            calm_rate <= 0.0 || !std::isfinite(active) || active <= 0.0)
            return 1.0; // the constructor refuses these; this keeps it total

        const double p = wake_rate / (wake_rate + calm_rate);
        if (p >= 1.0)
            return 1.0;
        const double calm = (1.0 - p * active) / (1.0 - p);
        if (!std::isfinite(calm) || calm <= 0.0)
            throw std::invalid_argument(
                "market_regime: active_multiplier is too large for this active share, since "
                "the calm multiplier that keeps the long-run mean at one would not be positive");
        return calm;
    }

    void publish_current() {
        const auto p = pinned_.load(std::memory_order_relaxed);
        if (p == automatic)
            publish(state_);
        else
            publish(p == active ? state::active : state::calm);
    }

    void advance_chain(double elapsed_seconds) {
        if (pinned_.load(std::memory_order_relaxed) != automatic) {
            publish_current();
            return;
        }

        double left = elapsed_seconds;
        while (left >= remaining_) {
            left -= remaining_;
            state_ = state_ == state::calm ? state::active : state::calm;
            remaining_ = dwell_time();
        }
        remaining_ -= left;
        // Read the pin again: one could have landed while the chain advanced, and
        // publishing the chain's state would then erase it for a whole period.
        publish_current();
    }

    [[nodiscard]] double dwell_time() {
        const double rate = state_ == state::calm ? wake_rate_ : calm_rate_;
        // The exponential distribution takes a mean, and the mean dwell time is
        // the reciprocal of the rate.
        return exponential_(rng_) / rate;
    }

    const double wake_rate_;
    const double calm_rate_;
    const double active_multiplier_;
    const double calm_multiplier_;

    // The published view: the only thing a feed reads, and the only thing that
    // needs to be atomic.
    std::atomic<int> published_state_{calm};
    std::atomic<double> published_multiplier_{1.0}; // replaced by the ctor's publish
    std::atomic<int> pinned_{automatic};

    // The chain: owned by the one advancing thread.
    state state_ = state::calm;
    double remaining_ = 0.0;
    std::optional<std::chrono::steady_clock::time_point> last_advanced_;
    std::mt19937 rng_;
    std::exponential_distribution<double> exponential_;
};

}

#endif
