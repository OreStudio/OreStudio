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
#ifndef ORES_ORE_CORE_RESULTS_CHILD_ROW_READER_HPP
#define ORES_ORE_CORE_RESULTS_CHILD_ROW_READER_HPP

#include "ores.ore.core/export.hpp"
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::results {

/**
 * @brief One cashflow a run produced, as =flows.csv= states it.
 *
 * The flow is identified by its trade, its leg, its cashflow number and the day
 * it pays. A figure the engine states as its null string stays empty rather
 * than becoming a zero, so a reader can tell a flow with no fixing from one
 * whose fixing is zero.
 */
struct flow_row {
    std::string trade_id;
    std::string leg_no;
    std::string cashflow_no;
    std::string pay_date;
    std::string flow_type;
    std::string currency;
    std::optional<double> amount;
    std::optional<double> coupon;
    std::optional<double> accrual;
    std::optional<double> accrued_amount;
    std::optional<double> notional;
    std::optional<double> discount_factor;
    std::optional<double> present_value;
    std::optional<double> present_value_base;
    std::string source;
};

/**
 * @brief One cashflow of a stressed run, with each figure in three parts.
 */
struct stressed_flow_row {
    std::string trade_id;
    std::string scenario_label;
    std::string leg_no;
    std::string cashflow_no;
    std::string pay_date;
    std::string flow_type;
    std::string currency;
    std::optional<double> amount_base;
    std::optional<double> amount_scenario;
    std::optional<double> amount_difference;
    std::optional<double> present_value_base;
    std::optional<double> present_value_scenario;
    std::optional<double> present_value_difference;
    std::string source;
};

/**
 * @brief One cashflow of a build, as =todaysmarketcalibration_cashflows.csv= states it.
 */
struct calibration_flow_row {
    std::string market_object_type;
    std::string market_object_id;
    std::string pillar_date;
    std::string rate_helper_type;
    std::string cashflow_no;
    std::string leg_no;
    std::string pay_date;
    std::string flow_type;
    std::string currency;
    std::optional<double> amount;
    std::optional<double> coupon;
    std::optional<double> accrued_amount;
    std::optional<double> notional;
    std::optional<double> discount_factor;
    std::optional<double> present_value;
    std::string source;
};

/**
 * @brief What reading a child-row family produced, and what it refused.
 */
template <typename Row>
struct child_read_outcome {
    std::vector<Row> rows;
    std::vector<std::string> refusals;
};

/**
 * @brief Reads =flows.csv=, the cashflows of a priced portfolio.
 *
 * A row without its trade, leg, cashflow number or pay date is refused: those
 * four are what make the flow a flow. A figure that is not a number is refused
 * and the heading is named.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT child_read_outcome<flow_row>
read_flows(std::string_view csv, std::string_view file_name);

/**
 * @brief Reads =stresstest_flows.csv=, where every figure has three parts.
 *
 * A family that states one part of a figure without the other two is refused
 * whole, because a row of the file cannot then be compared across scenarios.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT child_read_outcome<stressed_flow_row>
read_stressed_flows(std::string_view csv, std::string_view file_name);

/**
 * @brief Reads =todaysmarketcalibration_cashflows.csv=, the flows a build used.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT child_read_outcome<calibration_flow_row>
read_calibration_flows(std::string_view csv, std::string_view file_name);

}

#endif
