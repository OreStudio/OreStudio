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
#ifndef ORES_TRADING_API_BUILDERS_BARRIER_PRODUCT_BUILDERS_HPP
#define ORES_TRADING_API_BUILDERS_BARRIER_PRODUCT_BUILDERS_HPP

#include "ores.marketdata.api/datum/market_reference.hpp"
#include "ores.trading.api/builders/equity_builders.hpp"
#include "ores.trading.api/builders/fx_builders.hpp"
#include "ores.trading.api/builders/option_position.hpp"
#include "ores.trading.api/builders/required.hpp"
#include "ores.trading.api/builders/scripted_builders.hpp"
#include "ores.trading.api/export.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::trading::builders {

/// An option whose barrier is watched only between two dates.
struct window_barrier_option_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> currency;
    required<utility::decimal::decimal> fixing_amount;
    required<utility::decimal::decimal> strike;
    required<option_kind> kind;
    required<option_position> position;
    required<std::chrono::year_month_day> expiry;
    required<std::chrono::year_month_day> window_start;
    required<std::chrono::year_month_day> window_end;
    required<scripted_barrier> barrier;
};

class ORES_TRADING_API_EXPORT equity_window_barrier_option_builder final {
public:
    static constexpr std::string_view trade_type = "EquityWindowBarrierOption";
    explicit equity_window_barrier_option_builder(window_barrier_option_terms terms);
    [[nodiscard]] equity_result build() const;

private:
    window_barrier_option_terms terms_;
};

class ORES_TRADING_API_EXPORT fx_window_barrier_option_builder final {
public:
    static constexpr std::string_view trade_type = "FxWindowBarrierOption";
    explicit fx_window_barrier_option_builder(window_barrier_option_terms terms);
    [[nodiscard]] fx_result build() const;

private:
    window_barrier_option_terms terms_;
};

/**
 * @brief A vanilla option on an equity with barriers watched on a schedule.
 *
 * The barriers are monitored daily from the monitoring start to the expiry.
 * The kiko type says how a knock-in and a knock-out interact; ORE reads it
 * only when the barriers hold both. The builder states no rebate.
 */
struct generic_barrier_option_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> pay_currency;
    required<utility::decimal::decimal> quantity;
    required<utility::decimal::decimal> strike;
    required<option_kind> kind;
    required<option_position> position;
    required<std::chrono::year_month_day> expiry;
    required<std::chrono::year_month_day> settlement;
    required<std::chrono::year_month_day> monitoring_start;
    required<std::vector<scripted_barrier>> barriers;
    std::optional<std::string> kiko_type;
};

class ORES_TRADING_API_EXPORT equity_generic_barrier_option_builder final {
public:
    static constexpr std::string_view trade_type = "EquityGenericBarrierOption";
    explicit equity_generic_barrier_option_builder(generic_barrier_option_terms terms);
    [[nodiscard]] equity_result build() const;

private:
    generic_barrier_option_terms terms_;
};

}

#endif
