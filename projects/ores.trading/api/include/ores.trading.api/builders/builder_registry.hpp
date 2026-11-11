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

#include "ores.trading.api/builders/credit_builders.hpp"
#include "ores.trading.api/builders/rate_builders.hpp"
#include "ores.trading.api/builders/vanilla_swap_builder.hpp"
#include <set>
#include <string_view>
#include <tuple>

namespace ores::trading::builders {

/// Every trade builder; each states the trade type it builds.
using all_builders = std::tuple<vanilla_swap_builder,
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
                                inflation_swap_builder>;

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
