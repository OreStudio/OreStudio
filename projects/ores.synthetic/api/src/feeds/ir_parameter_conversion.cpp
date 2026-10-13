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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.synthetic.api/feeds/ir_parameter_conversion.hpp"
#include <algorithm>
#include <cctype>
#include <cmath>
#include <stdexcept>

namespace ores::synthetic::feed {

namespace {

enum class engine {
    vasicek,
    cox_ingersoll_ross,
    hull_white,
    black_karasinski,
    two_factor_gaussian
};

constexpr double basis_point = 1e-4;

/// The second factor of the simple Two-Factor Gaussian view, relative to the first.
constexpr double second_speed_ratio = 0.6;
constexpr double second_volatility_ratio = 0.5;

engine engine_of(const std::string& process_type) {
    auto code = process_type;
    std::transform(code.begin(), code.end(), code.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
    });
    if (code == "vasicek")
        return engine::vasicek;
    if (code == "cox_ingersoll_ross")
        return engine::cox_ingersoll_ross;
    if (code == "hull_white")
        return engine::hull_white;
    if (code == "black_karasinski")
        return engine::black_karasinski;
    if (code == "two_factor_gaussian")
        return engine::two_factor_gaussian;
    throw std::invalid_argument("Unknown process type: " + process_type);
}

double named(const std::map<std::string, double>& parameters, const std::string& name) {
    const auto found = parameters.find(name);
    if (found == parameters.end())
        throw std::invalid_argument("The parameter " + name + " is missing.");
    return found->second;
}

bool close(double a, double b) {
    return std::abs(a - b) <= 1e-6 * std::max({1.0, std::abs(a), std::abs(b)});
}

/// The total move of the two factors as the simple view splits it: √(σ₁² + σ₂²) at ρ of zero.
const double two_factor_spread = std::sqrt(1.0 + second_volatility_ratio * second_volatility_ratio);

}

std::map<std::string, double> ir_parameters_from_simple(const std::string& process_type,
                                                        const ir_simple_view& view) {
    const auto kind = engine_of(process_type);
    if (!(view.half_life_years > 0.0))
        throw std::invalid_argument("The half-life must be longer than zero.");
    if (view.yearly_move_bp < 0.0)
        throw std::invalid_argument("The yearly move cannot be negative.");
    if (view.start_rate < 0.0 || view.long_run_rate < 0.0)
        throw std::invalid_argument("A rate cannot be negative.");

    const double kappa = std::log(2.0) / view.half_life_years;
    const double move = view.yearly_move_bp * basis_point;
    const bool divides = kind == engine::cox_ingersoll_ross || kind == engine::black_karasinski;
    if (divides && !(view.long_run_rate > 0.0))
        throw std::invalid_argument("This engine needs a long-run rate above zero.");
    if (kind == engine::black_karasinski && !(view.start_rate > 0.0))
        throw std::invalid_argument("This engine needs a start rate above zero.");

    switch (kind) {
        case engine::vasicek:
        case engine::hull_white:
            return {{"kappa", kappa},
                    {"theta", view.long_run_rate},
                    {"sigma", move},
                    {"initial_rate", view.start_rate}};
        case engine::cox_ingersoll_ross:
            return {{"kappa", kappa},
                    {"theta", view.long_run_rate},
                    {"sigma", move / std::sqrt(view.long_run_rate)},
                    {"initial_rate", view.start_rate}};
        case engine::black_karasinski:
            return {{"kappa", kappa},
                    {"theta", std::log(view.long_run_rate)},
                    {"sigma", move / view.long_run_rate},
                    {"initial_rate", view.start_rate}};
        case engine::two_factor_gaussian: {
            const double sigma_x = move / two_factor_spread;
            return {{"kappa_x", kappa},
                    {"kappa_y", kappa * second_speed_ratio},
                    {"theta", view.long_run_rate},
                    {"sigma_x", sigma_x},
                    {"sigma_y", sigma_x * second_volatility_ratio},
                    {"rho", 0.0},
                    {"initial_rate", view.start_rate}};
        }
    }
    throw std::invalid_argument("Unknown process type: " + process_type);
}

ir_simple_reading ir_simple_from_parameters(const std::string& process_type,
                                            const std::map<std::string, double>& parameters) {
    const auto kind = engine_of(process_type);
    const bool two_factor = kind == engine::two_factor_gaussian;
    const double kappa = named(parameters, two_factor ? "kappa_x" : "kappa");
    const double theta = named(parameters, "theta");
    const double start = named(parameters, "initial_rate");

    ir_simple_reading out;
    if (kind == engine::cox_ingersoll_ross) {
        const double sigma = named(parameters, "sigma");
        if (sigma * sigma > 2.0 * kappa * theta)
            out.caution = "touches_zero";
    }
    if (!(kappa > 0.0)) {
        out.reason = "The rate never returns to its long-run level.";
        return out;
    }

    ir_simple_view view;
    view.start_rate = start;
    view.half_life_years = std::log(2.0) / kappa;
    switch (kind) {
        case engine::vasicek:
        case engine::hull_white:
            view.long_run_rate = theta;
            view.yearly_move_bp = named(parameters, "sigma") / basis_point;
            break;
        case engine::cox_ingersoll_ross:
            if (!(theta > 0.0)) {
                out.reason = "This engine needs a long-run rate above zero.";
                return out;
            }
            view.long_run_rate = theta;
            view.yearly_move_bp = named(parameters, "sigma") * std::sqrt(theta) / basis_point;
            break;
        case engine::black_karasinski:
            view.long_run_rate = std::exp(theta);
            view.yearly_move_bp = named(parameters, "sigma") * view.long_run_rate / basis_point;
            break;
        case engine::two_factor_gaussian: {
            const double sigma_x = named(parameters, "sigma_x");
            if (!close(named(parameters, "kappa_y"), kappa * second_speed_ratio) ||
                !close(named(parameters, "sigma_y"), sigma_x * second_volatility_ratio) ||
                !close(named(parameters, "rho"), 0.0))
                out.reason = "The second factor and the correlation are set by hand.";
            view.long_run_rate = theta;
            view.yearly_move_bp = sigma_x * two_factor_spread / basis_point;
            break;
        }
    }
    out.view = view;
    out.exact = out.reason.empty();
    return out;
}

}
