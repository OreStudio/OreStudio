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
#ifndef ORES_TRADING_API_BUILDERS_MULTI_ASSET_BUILDERS_HPP
#define ORES_TRADING_API_BUILDERS_MULTI_ASSET_BUILDERS_HPP

#include "ores.marketdata.api/datum/market_reference.hpp"
#include "ores.trading.api/builders/build_error.hpp"
#include "ores.trading.api/builders/option_position.hpp"
#include "ores.trading.api/builders/required.hpp"
#include "ores.trading.api/builders/scripted_builders.hpp"
#include "ores.trading.api/domain/instrument.hpp"
#include "ores.trading.api/export.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <expected>
#include <string>
#include <string_view>
#include <vector>

namespace ores::trading::builders {

/// The rows of a product whose underlyings each trade names.
using multi_asset_result = std::expected<domain::multi_asset_instrument_data, build_error>;

/// The kind of each leg of a double digital option, as ORE reads Type1 and Type2.
enum class digital_kind { call, put, collar };

/// A double digital option, which ORE reads through a class of its own.
struct double_digital_option_terms final {
    required<std::chrono::year_month_day> expiry;
    required<std::chrono::year_month_day> settlement;
    required<utility::decimal::decimal> binary_payout;
    required<utility::decimal::decimal> binary_level_1;
    required<utility::decimal::decimal> binary_level_2;
    required<digital_kind> kind_1;
    required<digital_kind> kind_2;
    required<option_position> position;
    required<std::string> pay_currency;
    required<marketdata::datum::market_reference> underlying_1;
    required<marketdata::datum::market_reference> underlying_2;
};

class ORES_TRADING_API_EXPORT double_digital_option_builder final {
public:
    static constexpr std::string_view trade_type = "DoubleDigitalOption";
    explicit double_digital_option_builder(double_digital_option_terms terms);
    [[nodiscard]] multi_asset_result build() const;

private:
    double_digital_option_terms terms_;
};

/// A performance option on a basket, which ORE reads through a class of its own.
struct performance_option_terms final {
    required<utility::decimal::decimal> notional;
    required<utility::decimal::decimal> participation_rate;
    required<std::chrono::year_month_day> valuation_date;
    required<std::chrono::year_month_day> settlement_date;
    required<std::vector<utility::decimal::decimal>> strike_prices;
    required<utility::decimal::decimal> strike;
    required<option_position> position;
    required<std::string> pay_currency;
    required<std::vector<marketdata::datum::market_reference>> underlyings;
};

class ORES_TRADING_API_EXPORT performance_option_builder final {
public:
    static constexpr std::string_view trade_type = "PerformanceOption_01";
    explicit performance_option_builder(performance_option_terms terms);
    [[nodiscard]] multi_asset_result build() const;

private:
    performance_option_terms terms_;
};

/**
 * @brief An autocallable that redeems when the underlying fixes at or above its
 * trigger, which ORE reads through a class of its own.
 */
struct autocallable_terms final {
    required<utility::decimal::decimal> notional;
    required<utility::decimal::decimal> determination_level;
    required<utility::decimal::decimal> trigger_level;
    required<marketdata::datum::market_reference> underlying;
    required<option_position> position;
    required<std::string> pay_currency;
    required<scripted_schedule> fixing_dates;
    required<scripted_schedule> settlement_dates;
    required<std::vector<utility::decimal::decimal>> accumulation_factors;
    required<utility::decimal::decimal> cap;
};

class ORES_TRADING_API_EXPORT autocallable_builder final {
public:
    static constexpr std::string_view trade_type = "Autocallable_01";
    explicit autocallable_builder(autocallable_terms terms);
    [[nodiscard]] multi_asset_result build() const;

private:
    autocallable_terms terms_;
};


/**
 * @brief A European option on one underlying that knocks in or out on
 * another's level at expiry.
 *
 * The barrier style is European, which ORE reads with no barrier schedule.
 */
struct european_option_barrier_terms final {
    required<scripted_family> family;
    required<marketdata::datum::market_reference> option_underlying;
    required<marketdata::datum::market_reference> barrier_underlying;
    required<utility::decimal::decimal> quantity;
    required<option_kind> kind;
    required<option_position> position;
    required<utility::decimal::decimal> strike;
    required<utility::decimal::decimal> premium_amount;
    required<std::string> premium_currency;
    required<std::chrono::year_month_day> premium_date;
    required<std::chrono::year_month_day> expiry;
    required<scripted_barrier> barrier;
    required<std::chrono::year_month_day> settlement;
    required<std::string> pay_currency;
};

class ORES_TRADING_API_EXPORT european_option_barrier_builder final {
public:
    static constexpr std::string_view trade_type = "EuropeanOptionBarrier";
    explicit european_option_barrier_builder(european_option_barrier_terms terms);
    [[nodiscard]] multi_asset_result build() const;

private:
    european_option_barrier_terms terms_;
};

}

#endif
