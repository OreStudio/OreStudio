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
#ifndef ORES_TRADING_API_BUILDERS_VANILLA_SWAP_BUILDER_HPP
#define ORES_TRADING_API_BUILDERS_VANILLA_SWAP_BUILDER_HPP

#include "ores.marketdata.api/conventions/convention_lookup.hpp"
#include "ores.marketdata.api/datum/market_reference.hpp"
#include "ores.trading.api/builders/build_error.hpp"
#include "ores.trading.api/builders/required.hpp"
#include "ores.trading.api/domain/instrument.hpp"
#include "ores.trading.api/export.hpp"
#include <chrono>
#include <expected>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::builders {

enum class side { pay, receive };

/// The economic terms of a vanilla swap; conventions supply the rest.
struct vanilla_swap_terms final {
    required<utility::decimal::decimal> notional;
    required<std::string> currency;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> maturity;
    required<utility::decimal::decimal> fixed_rate;
    required<marketdata::datum::market_reference> index;
    required<side> fixed_side;
};

/**
 * @brief Builds a complete vanilla swap from its economic terms.
 *
 * The frequency, day count, calendar and adjustment of each leg come from the
 * conventions of the index, as ORE's TradeGenerator derives them. A setter
 * overrides one.
 */
class ORES_TRADING_API_EXPORT vanilla_swap_builder final {
public:
    static constexpr std::string_view trade_type = "Swap";

    vanilla_swap_builder(const marketdata::conventions::convention_lookup& conventions,
                         vanilla_swap_terms terms);

    vanilla_swap_builder& fixed_day_count(std::string value);

    /// Replaces the adjustment the convention states for the fixed leg.
    vanilla_swap_builder& fixed_adjustment(std::string value);

    [[nodiscard]] std::expected<domain::swap_instrument_data, build_error> build() const;

private:
    const marketdata::conventions::convention_lookup& conventions_;
    vanilla_swap_terms terms_;
    std::optional<std::string> fixed_day_count_;
    std::optional<std::string> fixed_adjustment_;
};

}

#endif
