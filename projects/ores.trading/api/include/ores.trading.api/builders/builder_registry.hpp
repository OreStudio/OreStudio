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
#ifndef ORES_TRADING_API_BUILDERS_BUILDER_REGISTRY_HPP
#define ORES_TRADING_API_BUILDERS_BUILDER_REGISTRY_HPP

#include "ores.trading.api/builders/bond_builders.hpp"
#include "ores.trading.api/builders/commodity_builders.hpp"
#include "ores.trading.api/builders/credit_builders.hpp"
#include "ores.trading.api/builders/equity_builders.hpp"
#include "ores.trading.api/builders/fx_builders.hpp"
#include "ores.trading.api/builders/rate_builders.hpp"
#include "ores.trading.api/builders/vanilla_swap_builder.hpp"
#include <set>
#include <string_view>
#include <tuple>

namespace ores::trading::builders {

/// Every trade builder; each states the trade type it builds.
using all_builders =
    std::tuple<vanilla_swap_builder,
               credit_default_swap_builder,
               index_credit_default_swap_builder,
               index_credit_default_swap_option_builder,
               credit_linked_swap_builder,
               risk_participation_agreement_builder,
               cross_currency_swap_builder,
               forward_rate_agreement_builder,
               cap_floor_builder,
               swaption_builder,
               callable_swap_builder,
               knock_out_swap_builder,
               flexi_swap_builder,
               balance_guaranteed_swap_builder,
               inflation_swap_builder,
               fx_forward_builder,
               fx_swap_builder,
               fx_option_builder,
               fx_barrier_option_builder<fx_barrier_product::barrier>,
               fx_barrier_option_builder<fx_barrier_product::double_barrier>,
               fx_barrier_option_builder<fx_barrier_product::european_barrier>,
               fx_barrier_option_builder<fx_barrier_product::kiko>,
               fx_generic_barrier_option_builder,
               fx_digital_option_builder<fx_digital_product::digital>,
               fx_digital_option_builder<fx_digital_product::touch>,
               fx_digital_option_builder<fx_digital_product::double_touch>,
               fx_digital_option_builder<fx_digital_product::digital_barrier>,
               fx_average_forward_builder,
               fx_tarf_builder,
               fx_variance_swap_builder,
               fx_accumulator_builder,
               bond_builder,
               bond_position_builder,
               bond_option_builder,
               forward_bond_builder,
               bond_future_builder,
               bond_repo_builder,
               bond_trs_builder,
               callable_bond_builder,
               convertible_bond_builder,
               ascot_builder,
               equity_option_builder,
               equity_cliquet_option_builder,
               equity_asian_option_builder,
               equity_barrier_option_builder<equity_barrier_product::barrier>,
               equity_barrier_option_builder<equity_barrier_product::european_barrier>,
               equity_barrier_option_builder<equity_barrier_product::double_barrier>,
               equity_digital_option_builder,
               equity_touch_option_builder,
               equity_forward_builder,
               equity_swap_builder,
               equity_worst_of_basket_swap_builder,
               equity_variance_swap_builder,
               equity_accumulator_builder,
               equity_tarf_builder,
               equity_position_builder,
               equity_option_position_builder,
               commodity_forward_builder,
               commodity_option_builder,
               commodity_average_price_option_builder,
               commodity_option_strip_builder,
               commodity_swap_builder,
               commodity_swaption_builder,
               commodity_variance_swap_builder,
               commodity_basket_option_builder,
               commodity_digital_option_builder,
               commodity_digital_average_price_option_builder,
               commodity_position_builder,
               commodity_spread_option_builder,
               intraday_power_forward_builder>;

namespace detail {

template <typename... Builders>
[[nodiscard]] std::set<std::string_view> trade_types_of(std::tuple<Builders...>*) {
    return {Builders::trade_type...};
}

}

/// The trade types that have a builder.
[[nodiscard]] inline std::set<std::string_view> built_trade_types() {
    return detail::trade_types_of(static_cast<all_builders*>(nullptr));
}

}

#endif
