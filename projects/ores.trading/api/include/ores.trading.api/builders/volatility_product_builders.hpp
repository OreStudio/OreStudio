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
#ifndef ORES_TRADING_API_BUILDERS_VOLATILITY_PRODUCT_BUILDERS_HPP
#define ORES_TRADING_API_BUILDERS_VOLATILITY_PRODUCT_BUILDERS_HPP

#include "ores.marketdata.api/datum/market_reference.hpp"
#include "ores.trading.api/builders/basket_option_terms.hpp"
#include "ores.trading.api/builders/equity_builders.hpp"
#include "ores.trading.api/builders/fx_builders.hpp"
#include "ores.trading.api/builders/option_position.hpp"
#include "ores.trading.api/builders/required.hpp"
#include "ores.trading.api/export.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <string>
#include <string_view>
#include <vector>

namespace ores::trading::builders {

/**
 * @brief A swap on the realised variance of a weighted basket, observed daily
 * from the start to the end.
 *
 * The strike is a volatility, such as 0.2. A squared payoff pays on variance
 * rather than volatility.
 */
struct basket_variance_swap_terms final {
    required<std::vector<basket_option_member>> members;
    required<std::string> currency;
    required<utility::decimal::decimal> notional;
    required<utility::decimal::decimal> strike;
    required<bool> squared_payoff;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> end;
    required<std::chrono::year_month_day> settlement;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT equity_basket_variance_swap_builder final {
public:
    static constexpr std::string_view trade_type = "EquityBasketVarianceSwap";
    explicit equity_basket_variance_swap_builder(basket_variance_swap_terms terms);
    [[nodiscard]] equity_result build() const;

private:
    basket_variance_swap_terms terms_;
};

class ORES_TRADING_API_EXPORT fx_basket_variance_swap_builder final {
public:
    static constexpr std::string_view trade_type = "FxBasketVarianceSwap";
    explicit fx_basket_variance_swap_builder(basket_variance_swap_terms terms);
    [[nodiscard]] fx_result build() const;

private:
    basket_variance_swap_terms terms_;
};

/**
 * @brief An agreement to buy, on the FVA date, a straddle struck at the
 * underlying's level then and priced at the implied volatility agreed now.
 */
struct forward_volatility_agreement_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> pay_currency;
    required<utility::decimal::decimal> quantity;
    required<utility::decimal::decimal> implied_vol_strike;
    required<std::chrono::year_month_day> fva_date;
    required<std::chrono::year_month_day> option_expiry;
    required<std::chrono::year_month_day> premium_date;
    required<std::chrono::year_month_day> settlement;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT equity_forward_volatility_agreement_builder final {
public:
    static constexpr std::string_view trade_type = "EquityForwardVolatilityAgreement";
    explicit equity_forward_volatility_agreement_builder(forward_volatility_agreement_terms terms);
    [[nodiscard]] equity_result build() const;

private:
    forward_volatility_agreement_terms terms_;
};

class ORES_TRADING_API_EXPORT fx_forward_volatility_agreement_builder final {
public:
    static constexpr std::string_view trade_type = "FxForwardVolatilityAgreement";
    explicit fx_forward_volatility_agreement_builder(forward_volatility_agreement_terms terms);
    [[nodiscard]] fx_result build() const;

private:
    forward_volatility_agreement_terms terms_;
};

}

#endif
