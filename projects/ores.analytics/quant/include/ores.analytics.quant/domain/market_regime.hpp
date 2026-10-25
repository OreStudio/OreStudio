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
 * The factor every series shares is multiplier(): one while calm and
 * active_multiplier() while active. A series' own baseline is multiplied by it
 * and by the series' sensitivity, so a busy market lifts everything at once and
 * a curve follows less than a major pair.
 *
 * A scenario can pin either state. Pinning is the operational switch a
 * demonstration or a stress run uses, and it is why the object is not simply a
 * Poisson clock: a test that must wait for a random transition is not a test.
 *
 * The chain holds no time of its own: advance() is told how much time passed, so
 * a caller that sleeps in slices and a caller that jumps straight to the next
 * event use the same object. Not thread-safe.
 */
class market_regime final {
public:
    enum class state { calm, active };

    /**
     * @param wake_rate the rate at which a calm market turns active, per second.
     * @param calm_rate the rate at which an active market turns calm, per second.
     * @param active_multiplier how much the state lifts a baseline while active.
     * @param seed the random seed, so a run reproduces.
     */
    market_regime(double wake_rate = 1.0 / 600.0,
                  double calm_rate = 1.0 / 300.0,
                  double active_multiplier = 3.0,
                  std::uint32_t seed = 42)
        : wake_rate_(wake_rate)
        , calm_rate_(calm_rate)
        , active_multiplier_(active_multiplier)
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
    }

    /// @brief The state in force now.
    [[nodiscard]] state current() const { return pinned_.value_or(state_); }

    /**
     * @brief The multiplier in force now: one while calm, the active
     * multiplier while active.
     *
     * A pinned state reports its own multiplier, so a pinned calm scenario is
     * genuinely calm rather than merely waiting for a transition.
     */
    [[nodiscard]] double multiplier() const {
        return current() == state::active ? active_multiplier_ : 1.0;
    }

    /// @brief Whether a scenario has pinned the state.
    [[nodiscard]] bool is_pinned() const { return pinned_.has_value(); }

    /// @brief The share of time the chain spends active in the long run.
    [[nodiscard]] double long_run_active_share() const {
        return wake_rate_ / (wake_rate_ + calm_rate_);
    }

    /// @brief How long the current state still holds, in seconds.
    [[nodiscard]] double remaining_seconds() const { return remaining_; }

    /// @brief Pin the state, whatever the chain says, until unpin().
    void pin(state s) { pinned_ = s; }

    /// @brief Hand control back to the chain, which resumes from wherever it was.
    void unpin() { pinned_.reset(); }

    /**
     * @brief Advance the chain by @p elapsed_seconds, taking as many
     * transitions as fit.
     *
     * A pinned chain does not transition: the pin is the scenario's statement
     * about the market, and the chain picks up where it left off when freed.
     */
    void advance(double elapsed_seconds) {
        if (!std::isfinite(elapsed_seconds) || elapsed_seconds < 0.0)
            throw std::invalid_argument("market_regime: elapsed_seconds must be finite and "
                                        "non-negative");
        if (is_pinned())
            return;

        double left = elapsed_seconds;
        while (left >= remaining_) {
            left -= remaining_;
            state_ = state_ == state::calm ? state::active : state::calm;
            remaining_ = dwell_time();
        }
        remaining_ -= left;
    }

private:
    [[nodiscard]] double dwell_time() {
        const double rate = state_ == state::calm ? wake_rate_ : calm_rate_;
        // The exponential distribution takes a mean, and the mean dwell time is
        // the reciprocal of the rate.
        return exponential_(rng_) / rate;
    }

    double wake_rate_;
    double calm_rate_;
    double active_multiplier_;
    state state_ = state::calm;
    std::optional<state> pinned_;
    double remaining_ = 0.0;
    std::mt19937 rng_;
    std::exponential_distribution<double> exponential_;
};

}

#endif
