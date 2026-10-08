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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_trade_type_routing.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_API_DOMAIN_TRADE_TYPE_ROUTING_HPP
#define ORES_TRADING_API_DOMAIN_TRADE_TYPE_ROUTING_HPP

#include <optional>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief The instrument tables a trade type can be routed to.
 *
 * Generated from ores.trading.trade_type_catalogue: one value per table the
 * catalogue routes at least one trade type to.
 */
enum class instrument_table {
    balance_guaranteed_swap_instrument,
    bond_instrument,
    callable_swap_instrument,
    cap_floor_instrument,
    commodity_instrument,
    composite_instrument,
    credit_instrument,
    equity_accumulator_instrument,
    equity_asian_option_instrument,
    equity_barrier_option_instrument,
    equity_digital_option_instrument,
    equity_forward_instrument,
    equity_option_instrument,
    equity_position_instrument,
    equity_swap_instrument,
    equity_variance_swap_instrument,
    fra_instrument,
    fx_accumulator_instrument,
    fx_asian_forward_instrument,
    fx_barrier_option_instrument,
    fx_digital_option_instrument,
    fx_forward_instrument,
    fx_vanilla_option_instrument,
    fx_variance_swap_instrument,
    inflation_swap_instrument,
    knock_out_swap_instrument,
    scripted_instrument,
    swaption_instrument,
    vanilla_swap_instrument
};

/**
 * @brief The instrument table that holds a trade of the given type, or
 * nullopt where no table holds the type.
 */
[[nodiscard]] inline std::optional<instrument_table>
instrument_table_for(std::string_view trade_type) {
    if (trade_type == "CompositeTrade")
        return instrument_table::composite_instrument;
    if (trade_type == "Swap")
        return instrument_table::vanilla_swap_instrument;
    if (trade_type == "CrossCurrencySwap")
        return instrument_table::vanilla_swap_instrument;
    if (trade_type == "ForwardRateAgreement")
        return instrument_table::fra_instrument;
    if (trade_type == "CapFloor")
        return instrument_table::cap_floor_instrument;
    if (trade_type == "Swaption")
        return instrument_table::swaption_instrument;
    if (trade_type == "FlexiSwap")
        return instrument_table::vanilla_swap_instrument;
    if (trade_type == "BalanceGuaranteedSwap")
        return instrument_table::balance_guaranteed_swap_instrument;
    if (trade_type == "CallableSwap")
        return instrument_table::callable_swap_instrument;
    if (trade_type == "KnockOutSwap")
        return instrument_table::knock_out_swap_instrument;
    if (trade_type == "RiskParticipationAgreement")
        return instrument_table::credit_instrument;
    if (trade_type == "InflationSwap")
        return instrument_table::inflation_swap_instrument;
    if (trade_type == "FxForward")
        return instrument_table::fx_forward_instrument;
    if (trade_type == "FxSwap")
        return instrument_table::fx_forward_instrument;
    if (trade_type == "FxOption")
        return instrument_table::fx_vanilla_option_instrument;
    if (trade_type == "FxDigitalOption")
        return instrument_table::fx_digital_option_instrument;
    if (trade_type == "FxAverageForward")
        return instrument_table::fx_asian_forward_instrument;
    if (trade_type == "FxBarrierOption")
        return instrument_table::fx_barrier_option_instrument;
    if (trade_type == "FxDoubleBarrierOption")
        return instrument_table::fx_barrier_option_instrument;
    if (trade_type == "FxEuropeanBarrierOption")
        return instrument_table::fx_barrier_option_instrument;
    if (trade_type == "FxGenericBarrierOption")
        return instrument_table::fx_barrier_option_instrument;
    if (trade_type == "FxKIKOBarrierOption")
        return instrument_table::fx_barrier_option_instrument;
    if (trade_type == "FxTouchOption")
        return instrument_table::fx_digital_option_instrument;
    if (trade_type == "FxDoubleTouchOption")
        return instrument_table::fx_digital_option_instrument;
    if (trade_type == "FxDigitalBarrierOption")
        return instrument_table::fx_digital_option_instrument;
    if (trade_type == "FxVarianceSwap")
        return instrument_table::fx_variance_swap_instrument;
    if (trade_type == "FxAccumulator")
        return instrument_table::fx_accumulator_instrument;
    if (trade_type == "FxTaRF")
        return instrument_table::fx_asian_forward_instrument;
    if (trade_type == "CreditDefaultSwap")
        return instrument_table::credit_instrument;
    if (trade_type == "CreditDefaultSwapOption")
        return instrument_table::credit_instrument;
    if (trade_type == "IndexCreditDefaultSwap")
        return instrument_table::credit_instrument;
    if (trade_type == "IndexCreditDefaultSwapOption")
        return instrument_table::credit_instrument;
    if (trade_type == "SyntheticCDO")
        return instrument_table::credit_instrument;
    if (trade_type == "CreditLinkedSwap")
        return instrument_table::credit_instrument;
    if (trade_type == "CBO")
        return instrument_table::credit_instrument;
    if (trade_type == "Bond")
        return instrument_table::bond_instrument;
    if (trade_type == "ForwardBond")
        return instrument_table::bond_instrument;
    if (trade_type == "BondFuture")
        return instrument_table::bond_instrument;
    if (trade_type == "BondOption")
        return instrument_table::bond_instrument;
    if (trade_type == "BondRepo")
        return instrument_table::bond_instrument;
    if (trade_type == "BondTRS")
        return instrument_table::bond_instrument;
    if (trade_type == "BondPosition")
        return instrument_table::bond_instrument;
    if (trade_type == "CallableBond")
        return instrument_table::bond_instrument;
    if (trade_type == "ConvertibleBond")
        return instrument_table::bond_instrument;
    if (trade_type == "Ascot")
        return instrument_table::bond_instrument;
    if (trade_type == "EquityOption")
        return instrument_table::equity_option_instrument;
    if (trade_type == "EquityAsianOption")
        return instrument_table::equity_asian_option_instrument;
    if (trade_type == "EquityBarrierOption")
        return instrument_table::equity_barrier_option_instrument;
    if (trade_type == "EquityDoubleBarrierOption")
        return instrument_table::equity_barrier_option_instrument;
    if (trade_type == "EquityEuropeanBarrierOption")
        return instrument_table::equity_barrier_option_instrument;
    if (trade_type == "EquityTouchOption")
        return instrument_table::equity_digital_option_instrument;
    if (trade_type == "EquityDigitalOption")
        return instrument_table::equity_digital_option_instrument;
    if (trade_type == "EquityForward")
        return instrument_table::equity_forward_instrument;
    if (trade_type == "EquitySwap")
        return instrument_table::equity_swap_instrument;
    if (trade_type == "EquityVarianceSwap")
        return instrument_table::equity_variance_swap_instrument;
    if (trade_type == "EquityCliquetOption")
        return instrument_table::equity_option_instrument;
    if (trade_type == "EquityAccumulator")
        return instrument_table::equity_accumulator_instrument;
    if (trade_type == "EquityTaRF")
        return instrument_table::equity_accumulator_instrument;
    if (trade_type == "EquityWorstOfBasketSwap")
        return instrument_table::equity_swap_instrument;
    if (trade_type == "EquityOutperformanceOption")
        return instrument_table::equity_option_instrument;
    if (trade_type == "TotalReturnSwap")
        return instrument_table::composite_instrument;
    if (trade_type == "ContractForDifference")
        return instrument_table::composite_instrument;
    if (trade_type == "EquityPosition")
        return instrument_table::equity_position_instrument;
    if (trade_type == "EquityOptionPosition")
        return instrument_table::equity_position_instrument;
    if (trade_type == "CommodityForwardVolatilityAgreement")
        return instrument_table::commodity_instrument;
    if (trade_type == "IntradayPowerForward")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommodityForward")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommodityOption")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommodityDigitalOption")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommodityDigitalAveragePriceOption")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommodityAsianOption")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommodityAveragePriceOption")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommoditySpreadOption")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommodityOptionStrip")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommoditySwap")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommoditySwaption")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommodityVarianceSwap")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommodityPairwiseVarianceSwap")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommodityBasketVarianceSwap")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommodityAccumulator")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommodityTaRF")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommodityWorstOfBasketSwap")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommodityBestEntryOption")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommodityWindowBarrierOption")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommodityGenericBarrierOption")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommodityBasketOption")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommodityRainbowOption")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommodityStrikeResettableOption")
        return instrument_table::commodity_instrument;
    if (trade_type == "CommodityPosition")
        return instrument_table::commodity_instrument;
    if (trade_type == "ScriptedTrade")
        return instrument_table::scripted_instrument;
    if (trade_type == "Autocallable_01")
        return instrument_table::scripted_instrument;
    if (trade_type == "DoubleDigitalOption")
        return instrument_table::scripted_instrument;
    if (trade_type == "PerformanceOption_01")
        return instrument_table::scripted_instrument;
    return std::nullopt;
}

}

#endif
