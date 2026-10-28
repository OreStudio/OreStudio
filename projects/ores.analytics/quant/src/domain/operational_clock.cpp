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
#include <format>
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

void require(bool holds, const std::string& what) {
    if (!holds)
        throw std::invalid_argument("operational_clock: " + what);
}

double checked_calm_multiplier(const market_config& c) {
    require(positive(c.wake_rate), std::format("wake_rate is {}; it must be positive", c.wake_rate));
    require(positive(c.calm_rate), std::format("calm_rate is {}; it must be positive", c.calm_rate));
    require(positive(c.active_multiplier),
            std::format("active_multiplier is {}; it must be positive", c.active_multiplier));
    require(positive(c.speed), std::format("speed is {}; it must be positive", c.speed));
    require(std::isfinite(c.jump) && c.jump >= 0.0,
            std::format("jump is {}; it must be non-negative", c.jump));
    require(positive(c.decay), std::format("decay is {}; it must be positive", c.decay));
    require(std::isfinite(c.tail_loading) && c.tail_loading >= 0.0,
            std::format("tail_loading is {}; it must be non-negative", c.tail_loading));
    const double gain = c.jump / c.decay;
    require(gain < 1.0,
            std::format("the heat's loop gain, jump / decay, is {}; it must be below one", gain));

    const double p = c.wake_rate / (c.wake_rate + c.calm_rate);
    const double calm = (1.0 - p * c.active_multiplier) / (1.0 - p);
    require(positive(calm),
            std::format("the market spends {} of its time active, so an active multiplier "
                        "of {} leaves no positive calm multiplier that keeps the mean pace at one",
                        p,
                        c.active_multiplier));
    return calm;
}

}

void validate(const market_config& config) {
    checked_calm_multiplier(config);
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

double operational_clock::share_of(double heat) const {
    return population_rate_ > 0.0 ? heat / population_rate_ : 0.0;
}

double operational_clock::pace() const {
    return multiplier(state()) + share_of(heat_);
}

double operational_clock::tail_tilt() const {
    return config_.tail_loading == 0.0 ? 0.0 : config_.tail_loading * std::log(pace());
}

std::optional<double> operational_clock::next_transition_in() const {
    if (pin_)
        return std::nullopt;
    return remaining_;
}

double operational_clock::tau_over(double seconds, double m, double heat) const {
    const double share = share_of(heat);
    return m * seconds - share * std::expm1(-config_.decay * seconds) / config_.decay;
}

double operational_clock::seconds_for(double distance, double m, double heat) const {
    const double share = share_of(heat);
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
        const double moved = tau_over(span, multiplier(state()), heat_);
        tau_ += moved;
        simulated_ += config_.speed * moved;
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
    if (!std::isfinite(target))
        throw std::invalid_argument("operational_clock: target must be finite");
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

void operational_clock::reconfigure(const market_config& config) {
    const double calm = checked_calm_multiplier(config);
    const auto rate_of = [](const market_config& c, market_state s) {
        return s == market_state::calm ? c.wake_rate : c.calm_rate;
    };
    remaining_ = remaining_ * rate_of(config_, chain_state_) / rate_of(config, chain_state_);
    config_ = config;
    calm_multiplier_ = calm;
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
    if (!positive(mean_rate) || mean_rate > population_rate_ * (1.0 + 1e-9))
        throw std::invalid_argument(
            "operational_clock: mean_rate must be positive and no more than the enrolled total");
    // A sum of doubles added and taken away drifts, so the last series to leave
    // sets it back to exactly zero.
    --enrolled_;
    population_rate_ = enrolled_ == 0 ? 0.0 : population_rate_ - mean_rate;
}

}
