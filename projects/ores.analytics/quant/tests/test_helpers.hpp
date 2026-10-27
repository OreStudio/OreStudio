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
#ifndef ORES_ANALYTICS_QUANT_TESTS_TEST_HELPERS_HPP_
#define ORES_ANALYTICS_QUANT_TESTS_TEST_HELPERS_HPP_

#include "ores.analytics.quant/service/forward_rate_calculator.hpp"
#include "ores.analytics.quant/service/quantlib_calendar_rulesets.hpp"
#include <Eigen/Dense>

using ores::analytics::quant::service::bootstrap_curve_role_code;
using ores::analytics::quant::service::day_count_convention_code;
using ores::analytics::quant::service::interpolation_method_code;
using ores::analytics::quant::domain::instantiated_holiday;

inline constexpr auto A365 = day_count_convention_code::A365;

inline constexpr auto DEPOSIT = bootstrap_curve_role_code::DEPOSIT;

inline constexpr auto FLAT_FORWARD_THEN_LOG_LINEAR =
    interpolation_method_code::FLAT_FORWARD_THEN_LOG_LINEAR;

inline constexpr auto SWAP = bootstrap_curve_role_code::SWAP;

inline Eigen::VectorXd vector(std::initializer_list<double> values) {
    Eigen::VectorXd result(values.size());
    std::size_t i = 0;
    for (const double v : values)
        result[i++] = v;
    return result;
}

inline std::vector<std::chrono::year_month_day>
dates_for(std::size_t calendar_index, const std::vector<instantiated_holiday>& all) {
    std::vector<std::chrono::year_month_day> result;
    for (const auto& h : all)
        if (h.calendar_index == calendar_index)
            result.push_back(h.date);
    return result;
}

inline auto epoch(int seconds) {
    return std::chrono::system_clock::time_point{} + std::chrono::seconds(seconds);
}

#endif
