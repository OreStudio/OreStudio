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
#include "ores.analytics.quant/domain/operational_clock.hpp"
#include <cmath>
#include <stdexcept>
#include <string>
#include <utility>

namespace ores::analytics::quant::domain {

namespace {

bool positive(double x) {
    return std::isfinite(x) && x > 0.0;
}

market_state other(market_state s) {
    return s == market_state::calm ? market_state::active : market_state::calm;
}

double checked_calm_multiplier(const market_config& c) {
    if (!positive(c.wake_rate) || !positive(c.calm_rate))
        throw std::invalid_argument("operational_clock: wake_rate and calm_rate must be positive");
    if (!positive(c.active_multiplier))
        throw std::invalid_argument("operational_clock: active_multiplier must be positive");
    if (!std::isfinite(c.jump) || c.jump < 0.0 || !positive(c.decay))
        throw std::invalid_argument(
            "operational_clock: jump must be non-negative and decay positive");
    const double gain = c.jump / c.decay;
    if (gain >= 1.0)
        throw std::invalid_argument("operational_clock: the heat's loop gain, jump / decay, is " +
                                    std::to_string(gain) + "; it must be below one");

    const double p = c.wake_rate / (c.wake_rate + c.calm_rate);
    const double calm = (1.0 - p * c.active_multiplier) / (1.0 - p);
    if (!positive(calm))
        throw std::invalid_argument("operational_clock: active_multiplier is too large for this "
                                    "active share; no positive calm multiplier keeps the mean "
                                    "pace at one");
    return calm;
}

}

operational_clock::operational_clock(const market_config& config, math::rng_stream stream)
    : config_(config)
    , calm_multiplier_(checked_calm_multiplier(config))
    , stream_(std::move(stream)) {
    // Start where the chain spends its time in the long run, so the first
    // minutes of a run are not always calm. A dwell is memoryless, so the time
    // left in the first state is a fresh dwell.
    const double active_share = config_.wake_rate / (config_.wake_rate + config_.calm_rate);
    chain_state_ =
        math::open_uniform(stream_) <= active_share ? market_state::active : market_state::calm;
    remaining_ = unit_dwell(0) /
                 (chain_state_ == market_state::calm ? config_.wake_rate : config_.calm_rate);
    upcoming_dwells_.pop_front();
}

double operational_clock::pace() const {
    const double share = population_rate_ > 0.0 ? heat_ / population_rate_ : 0.0;
    return multiplier(state()) + share;
}

std::optional<double> operational_clock::next_transition_in() const {
    if (pin_)
        return std::nullopt;
    return remaining_;
}

double operational_clock::tau_over(double seconds, double m, double heat) const {
    const double share = population_rate_ > 0.0 ? heat / population_rate_ : 0.0;
    return m * seconds - share * std::expm1(-config_.decay * seconds) / config_.decay;
}

double operational_clock::seconds_for(double distance, double m, double heat) const {
    const double share = population_rate_ > 0.0 ? heat / population_rate_ : 0.0;
    if (share == 0.0)
        return distance / m;

    // tau_over is increasing and concave in the seconds, so Newton's method
    // started below the root, at the pace's upper bound, climbs to it without
    // overshooting.
    double s = distance / (m + share);
    for (int i = 0; i < 100; ++i) {
        const double miss = tau_over(s, m, heat) - distance;
        const double slope = m + share * std::exp(-config_.decay * s);
        const double step = miss / slope;
        s -= step;
        if (std::abs(step) <= 1e-15 * s)
            break;
    }
    return s;
}

double operational_clock::unit_dwell(std::size_t k) {
    while (upcoming_dwells_.size() <= k)
        upcoming_dwells_.push_back(-std::log(math::open_uniform(stream_)));
    return upcoming_dwells_[k];
}

void operational_clock::advance(double seconds) {
    if (!std::isfinite(seconds) || seconds < 0.0)
        throw std::invalid_argument("operational_clock: seconds must be finite and non-negative");

    while (seconds > 0.0) {
        const bool turns = !pin_ && remaining_ <= seconds;
        const double span = turns ? remaining_ : seconds;
        tau_ += tau_over(span, multiplier(state()), heat_);
        heat_ *= std::exp(-config_.decay * span);
        now_ += span;
        seconds -= span;
        if (pin_)
            continue;
        remaining_ -= span;
        if (turns) {
            chain_state_ = other(chain_state_);
            remaining_ = unit_dwell(0) / (chain_state_ == market_state::calm ? config_.wake_rate :
                                                                               config_.calm_rate);
            upcoming_dwells_.pop_front();
        }
    }
}

double operational_clock::time_to_reach(double target) {
    double distance = target - tau_;
    if (distance <= 0.0)
        return 0.0;
    if (pin_)
        return seconds_for(distance, multiplier(*pin_), heat_);

    double elapsed = 0.0;
    double heat = heat_;
    market_state s = chain_state_;
    double remaining = remaining_;
    for (std::size_t k = 0;; ++k) {
        const double span = tau_over(remaining, multiplier(s), heat);
        if (span >= distance)
            return elapsed + seconds_for(distance, multiplier(s), heat);
        distance -= span;
        elapsed += remaining;
        heat *= std::exp(-config_.decay * remaining);
        s = other(s);
        remaining =
            unit_dwell(k) / (s == market_state::calm ? config_.wake_rate : config_.calm_rate);
    }
}

void operational_clock::pin(market_state s) {
    pin_ = s;
}

void operational_clock::unpin() {
    pin_.reset();
}

void operational_clock::record_event() {
    heat_ += config_.jump;
}

void operational_clock::enrol(double mean_rate) {
    if (!positive(mean_rate))
        throw std::invalid_argument("operational_clock: mean_rate must be positive");
    ++enrolled_;
    population_rate_ += mean_rate;
}

void operational_clock::withdraw(double mean_rate) {
    if (enrolled_ == 0)
        throw std::logic_error("operational_clock: withdraw with no series enrolled");
    // A sum of doubles added and taken away drifts, so the last series to leave
    // sets it back to exactly zero.
    --enrolled_;
    population_rate_ = enrolled_ == 0 ? 0.0 : population_rate_ - mean_rate;
}

}
