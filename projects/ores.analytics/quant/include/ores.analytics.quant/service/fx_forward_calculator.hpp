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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_FX_FORWARD_CALCULATOR_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_FX_FORWARD_CALCULATOR_HPP

#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/service/curve_bootstrap_engine.hpp"
#include <chrono>
#include <expected>
#include <optional>
#include <string>
#include <vector>

namespace ores::analytics::quant::service {

struct fx_forward_tenor final {
    std::string tenor_id;
    std::chrono::year_month_day value_date{};
    /// The near leg of the swap this tenor quotes; absent means the spot date. ON and TN name
    /// their own, because each is a swap of its own two dates.
    std::optional<std::chrono::year_month_day> near_date;
};

struct fx_forward_request final {
    std::string base_currency;
    std::string quote_currency;
    /// In units of the quote currency per unit of the base currency.
    double spot = 0.0;
    std::chrono::year_month_day spot_date{};
    /// The value of one point in the rate, such as 0.0001 for EUR/USD and 0.01 for USD/JPY.
    double pip_factor = 0.0;
    /// Ascending by date. Each must cover the spot date, every value date and every near date, so
    /// a curve as published needs its reference date added at a discount factor of one.
    std::vector<bootstrapped_point> base_curve;
    std::vector<bootstrapped_point> quote_curve;
    interpolation_method_code interpolation = interpolation_method_code::LOG_LINEAR_DISCOUNT;
    std::vector<fx_forward_tenor> tenors;
};

struct fx_forward_point final {
    std::string tenor_id;
    std::chrono::year_month_day value_date{};
    std::chrono::year_month_day near_date{};
    double base_discount = 0.0;
    double quote_discount = 0.0;
    double forward_rate = 0.0;
    /// (forward - spot) / pip factor.
    double forward_points = 0.0;
    /// (forward at the value date - forward at the near date) / pip factor.
    double swap_points = 0.0;
};

/**
 * @brief The FX forward of every tenor, by covered interest parity between two discount curves.
 *
 * F(T) = S * (D_base(T) / D_base(spot date)) / (D_quote(T) / D_quote(spot date)). The rate is
 * implied by the two curves, so it holds no cross-currency basis.
 *
 * Refuses a spot or pip factor that is not positive and finite, and a date a curve cannot state,
 * naming the currency and the tenor.
 */
ORES_ANALYTICS_QUANT_EXPORT std::expected<std::vector<fx_forward_point>, std::string>
calculate_fx_forwards(const fx_forward_request& request);

}

#endif
