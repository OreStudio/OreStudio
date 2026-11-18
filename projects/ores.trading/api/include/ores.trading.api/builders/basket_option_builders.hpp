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
#ifndef ORES_TRADING_API_BUILDERS_BASKET_OPTION_BUILDERS_HPP
#define ORES_TRADING_API_BUILDERS_BASKET_OPTION_BUILDERS_HPP

#include "ores.trading.api/builders/basket_option_terms.hpp"
#include "ores.trading.api/builders/equity_builders.hpp"
#include "ores.trading.api/builders/fx_builders.hpp"
#include "ores.trading.api/export.hpp"
#include <string_view>
#include <type_traits>

namespace ores::trading::builders {

/// The products ORE reads over a weighted basket: a basket and a rainbow in each family.
enum class basket_product { equity_basket, fx_basket, equity_rainbow, fx_rainbow };

/// The equity family's row and shared rows; a rainbow takes the rainbow payoffs.
ORES_TRADING_API_EXPORT equity_result build_equity_basket_option(std::string_view trade_type,
                                                                 bool rainbow,
                                                                 const basket_option_terms& terms);

/// The FX family's row and shared rows; a rainbow takes the rainbow payoffs.
ORES_TRADING_API_EXPORT fx_result build_fx_basket_option(std::string_view trade_type,
                                                         bool rainbow,
                                                         const basket_option_terms& terms);

template <basket_product Product>
class basket_option_builder final {
    static constexpr bool equity =
        Product == basket_product::equity_basket || Product == basket_product::equity_rainbow;
    static constexpr bool rainbow =
        Product == basket_product::equity_rainbow || Product == basket_product::fx_rainbow;

public:
    static constexpr std::string_view trade_type = [] {
        switch (Product) {
            case basket_product::equity_basket:
                return std::string_view("EquityBasketOption");
            case basket_product::fx_basket:
                return std::string_view("FxBasketOption");
            case basket_product::equity_rainbow:
                return std::string_view("EquityRainbowOption");
            default:
                return std::string_view("FxRainbowOption");
        }
    }();
    explicit basket_option_builder(basket_option_terms terms)
        : terms_(std::move(terms)) {}
    [[nodiscard]] std::conditional_t<equity, equity_result, fx_result> build() const {
        if constexpr (equity)
            return build_equity_basket_option(trade_type, rainbow, terms_);
        else
            return build_fx_basket_option(trade_type, rainbow, terms_);
    }

private:
    basket_option_terms terms_;
};

}

#endif
