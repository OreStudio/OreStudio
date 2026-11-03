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
#include "ores.analytics.quant/service/processes/gjr_garch_variance_process.hpp"
#include <algorithm>
#include <cmath>
#include <stdexcept>
#include <utility>

namespace ores::analytics::quant::service {

namespace {

/// Two tick counts this close are one: the day's ticks accumulate by addition.
constexpr double tick_tolerance = 1e-9;

bool positive_finite(double v) {
    return v > 0.0 && std::isfinite(v);
}

}

gjr_garch_variance_process::gjr_garch_variance_process(
    std::unique_ptr<domain::i_stochastic_process> inner,
    math::gjr_garch_model model,
    daily_variance_frame frame,
    double initial_daily_variance)
    : inner_(std::move(inner))
    , model_(model)
    , frame_(frame)
    , variance_(initial_daily_variance) {
    if (!inner_)
        throw std::invalid_argument("gjr_garch_variance_process: the inner process is null");
    if (!inner_->supports_volatility_multiplier())
        throw std::invalid_argument(
            "gjr_garch_variance_process: the inner process cannot scale its volatility");
    if (!positive_finite(frame_.ticks_per_day))
        throw std::invalid_argument(
            "gjr_garch_variance_process: ticks_per_day must be positive and finite");
    if (!positive_finite(frame_.inner_daily_variance))
        throw std::invalid_argument(
            "gjr_garch_variance_process: the inner daily variance must be positive and finite");
    if (!std::isfinite(frame_.inner_daily_drift))
        throw std::invalid_argument(
            "gjr_garch_variance_process: the inner daily drift must be finite");
    if (!positive_finite(variance_))
        throw std::invalid_argument(
            "gjr_garch_variance_process: the initial daily variance must be positive and finite");
    inner_->set_volatility_multiplier(std::sqrt(variance_ / frame_.inner_daily_variance));
    day_open_ = inner_->current();
}

double gjr_garch_variance_process::next() {
    return next(1.0);
}

double gjr_garch_variance_process::next(double elapsed_ticks) {
    if (!(elapsed_ticks > 0.0))
        throw std::invalid_argument(
            "gjr_garch_variance_process: elapsed_ticks must be strictly positive");
    double left = elapsed_ticks;
    while (left > tick_tolerance) {
        const double segment = std::min(left, frame_.ticks_per_day - ticks_into_day_);
        inner_->next(segment);
        ticks_into_day_ += segment;
        left -= segment;
        if (ticks_into_day_ >= frame_.ticks_per_day - tick_tolerance)
            roll();
    }
    return inner_->current();
}

void gjr_garch_variance_process::roll() {
    const double close = inner_->current();
    const double day_return = frame_.measure == return_measure::log_return ?
                                  std::log(close / day_open_) :
                                  close - day_open_;
    variance_ = model_.next_variance(variance_, day_return - frame_.inner_daily_drift);
    inner_->set_volatility_multiplier(std::sqrt(variance_ / frame_.inner_daily_variance));
    ticks_into_day_ = 0.0;
    day_open_ = close;
}

bool gjr_garch_variance_process::supports_variable_step() const {
    return inner_->supports_variable_step();
}

void gjr_garch_variance_process::set_tail_tilt(double tilt) {
    inner_->set_tail_tilt(tilt);
}

double gjr_garch_variance_process::max_step() const {
    return inner_->max_step();
}

double gjr_garch_variance_process::current() const {
    return inner_->current();
}

}
