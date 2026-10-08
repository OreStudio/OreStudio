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
 * Template: cpp_trade_type_batch.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_API_DOMAIN_INSTRUMENT_BATCH_HPP
#define ORES_TRADING_API_DOMAIN_INSTRUMENT_BATCH_HPP

#include "ores.trading.api/domain/balance_guaranteed_swap_instrument.hpp"
#include "ores.trading.api/domain/bond_instrument_data.hpp"
#include "ores.trading.api/domain/callable_swap_call_date.hpp"
#include "ores.trading.api/domain/callable_swap_instrument.hpp"
#include "ores.trading.api/domain/cap_floor_instrument.hpp"
#include "ores.trading.api/domain/commodity_basket_constituent.hpp"
#include "ores.trading.api/domain/commodity_instrument.hpp"
#include "ores.trading.api/domain/composite_instrument.hpp"
#include "ores.trading.api/domain/composite_leg.hpp"
#include "ores.trading.api/domain/credit_instrument.hpp"
#include "ores.trading.api/domain/equity_accumulator_instrument.hpp"
#include "ores.trading.api/domain/equity_asian_option_instrument.hpp"
#include "ores.trading.api/domain/equity_barrier_option_instrument.hpp"
#include "ores.trading.api/domain/equity_digital_option_instrument.hpp"
#include "ores.trading.api/domain/equity_forward_instrument.hpp"
#include "ores.trading.api/domain/equity_option_instrument.hpp"
#include "ores.trading.api/domain/equity_position_instrument.hpp"
#include "ores.trading.api/domain/equity_position_option_underlying.hpp"
#include "ores.trading.api/domain/equity_swap_instrument.hpp"
#include "ores.trading.api/domain/equity_variance_swap_instrument.hpp"
#include "ores.trading.api/domain/fra_instrument.hpp"
#include "ores.trading.api/domain/fx_accumulator_instrument.hpp"
#include "ores.trading.api/domain/fx_asian_forward_instrument.hpp"
#include "ores.trading.api/domain/fx_barrier_option_instrument.hpp"
#include "ores.trading.api/domain/fx_digital_option_instrument.hpp"
#include "ores.trading.api/domain/fx_forward_instrument.hpp"
#include "ores.trading.api/domain/fx_vanilla_option_instrument.hpp"
#include "ores.trading.api/domain/fx_variance_swap_instrument.hpp"
#include "ores.trading.api/domain/inflation_swap_instrument.hpp"
#include "ores.trading.api/domain/knock_out_swap_instrument.hpp"
#include "ores.trading.api/domain/scripted_instrument.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"
#include "ores.trading.api/domain/swap_leg_amount.hpp"
#include "ores.trading.api/domain/swap_leg_rate.hpp"
#include "ores.trading.api/domain/swaption_instrument.hpp"
#include "ores.trading.api/domain/vanilla_swap_instrument.hpp"
#include <boost/uuid/uuid.hpp>
#include <utility>
#include <vector>

namespace ores::trading::domain {

/**
 * @brief Every instrument and instrument child an export carries, one typed
 * array per entity.
 *
 * Generated from ores.trading.trade_type_catalogue. A std::variant cannot
 * cross the wire, because rfl names no alternative, and neither can a payload
 * that encodes one in a string. The tag therefore lives on the container
 * instead of on every element: each element carries the trade id it belongs
 * to, and the array it sits in states its type. A receiver joins an array to a
 * trade by that id, so no element needs a discriminator.
 */
struct instrument_batch {
    /**
     * @brief The balance_guaranteed_swap_instrument rows, keyed by trade id.
     */
    std::vector<balance_guaranteed_swap_instrument> balance_guaranteed_swap_instruments;
    /**
     * @brief The bond_instrument_data rows, keyed by trade id.
     */
    std::vector<bond_instrument_data> bond_instruments;
    /**
     * @brief The callable_swap_instrument rows, keyed by trade id.
     */
    std::vector<callable_swap_instrument> callable_swap_instruments;
    /**
     * @brief The cap_floor_instrument rows, keyed by trade id.
     */
    std::vector<cap_floor_instrument> cap_floor_instruments;
    /**
     * @brief The commodity_instrument rows, keyed by trade id.
     */
    std::vector<commodity_instrument> commodity_instruments;
    /**
     * @brief The composite_instrument rows, keyed by trade id.
     */
    std::vector<composite_instrument> composite_instruments;
    /**
     * @brief The credit_instrument rows, keyed by trade id.
     */
    std::vector<credit_instrument> credit_instruments;
    /**
     * @brief The equity_accumulator_instrument rows, keyed by trade id.
     */
    std::vector<equity_accumulator_instrument> equity_accumulator_instruments;
    /**
     * @brief The equity_asian_option_instrument rows, keyed by trade id.
     */
    std::vector<equity_asian_option_instrument> equity_asian_option_instruments;
    /**
     * @brief The equity_barrier_option_instrument rows, keyed by trade id.
     */
    std::vector<equity_barrier_option_instrument> equity_barrier_option_instruments;
    /**
     * @brief The equity_digital_option_instrument rows, keyed by trade id.
     */
    std::vector<equity_digital_option_instrument> equity_digital_option_instruments;
    /**
     * @brief The equity_forward_instrument rows, keyed by trade id.
     */
    std::vector<equity_forward_instrument> equity_forward_instruments;
    /**
     * @brief The equity_option_instrument rows, keyed by trade id.
     */
    std::vector<equity_option_instrument> equity_option_instruments;
    /**
     * @brief The equity_position_instrument rows, keyed by trade id.
     */
    std::vector<equity_position_instrument> equity_position_instruments;
    /**
     * @brief The equity_swap_instrument rows, keyed by trade id.
     */
    std::vector<equity_swap_instrument> equity_swap_instruments;
    /**
     * @brief The equity_variance_swap_instrument rows, keyed by trade id.
     */
    std::vector<equity_variance_swap_instrument> equity_variance_swap_instruments;
    /**
     * @brief The fra_instrument rows, keyed by trade id.
     */
    std::vector<fra_instrument> fra_instruments;
    /**
     * @brief The fx_accumulator_instrument rows, keyed by trade id.
     */
    std::vector<fx_accumulator_instrument> fx_accumulator_instruments;
    /**
     * @brief The fx_asian_forward_instrument rows, keyed by trade id.
     */
    std::vector<fx_asian_forward_instrument> fx_asian_forward_instruments;
    /**
     * @brief The fx_barrier_option_instrument rows, keyed by trade id.
     */
    std::vector<fx_barrier_option_instrument> fx_barrier_option_instruments;
    /**
     * @brief The fx_digital_option_instrument rows, keyed by trade id.
     */
    std::vector<fx_digital_option_instrument> fx_digital_option_instruments;
    /**
     * @brief The fx_forward_instrument rows, keyed by trade id.
     */
    std::vector<fx_forward_instrument> fx_forward_instruments;
    /**
     * @brief The fx_vanilla_option_instrument rows, keyed by trade id.
     */
    std::vector<fx_vanilla_option_instrument> fx_vanilla_option_instruments;
    /**
     * @brief The fx_variance_swap_instrument rows, keyed by trade id.
     */
    std::vector<fx_variance_swap_instrument> fx_variance_swap_instruments;
    /**
     * @brief The inflation_swap_instrument rows, keyed by trade id.
     */
    std::vector<inflation_swap_instrument> inflation_swap_instruments;
    /**
     * @brief The knock_out_swap_instrument rows, keyed by trade id.
     */
    std::vector<knock_out_swap_instrument> knock_out_swap_instruments;
    /**
     * @brief The scripted_instrument rows, keyed by trade id.
     */
    std::vector<scripted_instrument> scripted_instruments;
    /**
     * @brief The swaption_instrument rows, keyed by trade id.
     */
    std::vector<swaption_instrument> swaption_instruments;
    /**
     * @brief The vanilla_swap_instrument rows, keyed by trade id.
     */
    std::vector<vanilla_swap_instrument> vanilla_swap_instruments;
    /**
     * @brief The callable_swap_call_date rows, keyed by trade id.
     */
    std::vector<callable_swap_call_date> callable_swap_call_dates;
    /**
     * @brief The commodity_basket_constituent rows, keyed by trade id.
     */
    std::vector<commodity_basket_constituent> commodity_basket_constituents;
    /**
     * @brief The composite_leg rows, keyed by trade id.
     */
    std::vector<composite_leg> composite_legs;
    /**
     * @brief The equity_position_option_underlying rows, keyed by trade id.
     */
    std::vector<equity_position_option_underlying> equity_position_option_underlyings;
    /**
     * @brief The swap_leg rows, keyed by trade id.
     */
    std::vector<swap_leg> swap_legs;
    /**
     * @brief The swap_leg_amount rows, keyed by trade id.
     */
    std::vector<swap_leg_amount> swap_leg_amounts;
    /**
     * @brief The swap_leg_rate rows, keyed by trade id.
     */
    std::vector<swap_leg_rate> swap_leg_rates;
};

/**
 * @brief The trade id a batch member is keyed by.
 *
 * A generated entity nests its key in an identity group, a document container
 * holds the entity that does, and a flat child states it directly. The reader
 * picks the shape the member has, so no member needs its own accessor and a
 * model that gains or loses the group needs no change here.
 */
template <typename T>
[[nodiscard]] boost::uuids::uuid trade_id_of(const T& v) {
    if constexpr (requires { v.identity.trade_id; })
        return v.identity.trade_id;
    else if constexpr (requires { v.instrument.identity.trade_id; })
        return v.instrument.identity.trade_id;
    else
        return v.trade_id;
}

/**
 * @brief Adds one balance_guaranteed_swap_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, balance_guaranteed_swap_instrument v) {
    batch.balance_guaranteed_swap_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one bond_instrument_data to the array that holds its type.
 */
inline void append(instrument_batch& batch, bond_instrument_data v) {
    batch.bond_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one callable_swap_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, callable_swap_instrument v) {
    batch.callable_swap_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one cap_floor_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, cap_floor_instrument v) {
    batch.cap_floor_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one commodity_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, commodity_instrument v) {
    batch.commodity_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one composite_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, composite_instrument v) {
    batch.composite_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one credit_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, credit_instrument v) {
    batch.credit_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one equity_accumulator_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, equity_accumulator_instrument v) {
    batch.equity_accumulator_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one equity_asian_option_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, equity_asian_option_instrument v) {
    batch.equity_asian_option_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one equity_barrier_option_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, equity_barrier_option_instrument v) {
    batch.equity_barrier_option_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one equity_digital_option_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, equity_digital_option_instrument v) {
    batch.equity_digital_option_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one equity_forward_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, equity_forward_instrument v) {
    batch.equity_forward_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one equity_option_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, equity_option_instrument v) {
    batch.equity_option_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one equity_position_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, equity_position_instrument v) {
    batch.equity_position_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one equity_swap_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, equity_swap_instrument v) {
    batch.equity_swap_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one equity_variance_swap_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, equity_variance_swap_instrument v) {
    batch.equity_variance_swap_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one fra_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, fra_instrument v) {
    batch.fra_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one fx_accumulator_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, fx_accumulator_instrument v) {
    batch.fx_accumulator_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one fx_asian_forward_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, fx_asian_forward_instrument v) {
    batch.fx_asian_forward_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one fx_barrier_option_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, fx_barrier_option_instrument v) {
    batch.fx_barrier_option_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one fx_digital_option_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, fx_digital_option_instrument v) {
    batch.fx_digital_option_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one fx_forward_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, fx_forward_instrument v) {
    batch.fx_forward_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one fx_vanilla_option_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, fx_vanilla_option_instrument v) {
    batch.fx_vanilla_option_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one fx_variance_swap_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, fx_variance_swap_instrument v) {
    batch.fx_variance_swap_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one inflation_swap_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, inflation_swap_instrument v) {
    batch.inflation_swap_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one knock_out_swap_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, knock_out_swap_instrument v) {
    batch.knock_out_swap_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one scripted_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, scripted_instrument v) {
    batch.scripted_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one swaption_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, swaption_instrument v) {
    batch.swaption_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one vanilla_swap_instrument to the array that holds its type.
 */
inline void append(instrument_batch& batch, vanilla_swap_instrument v) {
    batch.vanilla_swap_instruments.push_back(std::move(v));
}

/**
 * @brief Adds one callable_swap_call_date to the array that holds its type.
 */
inline void append(instrument_batch& batch, callable_swap_call_date v) {
    batch.callable_swap_call_dates.push_back(std::move(v));
}

/**
 * @brief Adds one commodity_basket_constituent to the array that holds its type.
 */
inline void append(instrument_batch& batch, commodity_basket_constituent v) {
    batch.commodity_basket_constituents.push_back(std::move(v));
}

/**
 * @brief Adds one composite_leg to the array that holds its type.
 */
inline void append(instrument_batch& batch, composite_leg v) {
    batch.composite_legs.push_back(std::move(v));
}

/**
 * @brief Adds one equity_position_option_underlying to the array that holds its type.
 */
inline void append(instrument_batch& batch, equity_position_option_underlying v) {
    batch.equity_position_option_underlyings.push_back(std::move(v));
}

/**
 * @brief Adds one swap_leg to the array that holds its type.
 */
inline void append(instrument_batch& batch, swap_leg v) {
    batch.swap_legs.push_back(std::move(v));
}

/**
 * @brief Adds one swap_leg_amount to the array that holds its type.
 */
inline void append(instrument_batch& batch, swap_leg_amount v) {
    batch.swap_leg_amounts.push_back(std::move(v));
}

/**
 * @brief Adds one swap_leg_rate to the array that holds its type.
 */
inline void append(instrument_batch& batch, swap_leg_rate v) {
    batch.swap_leg_rates.push_back(std::move(v));
}

/**
 * @brief The member of an array that belongs to the trade, or null.
 */
template <typename T>
[[nodiscard]] const T* find(const std::vector<T>& members, boost::uuids::uuid trade_id) {
    for (const auto& member : members)
        if (trade_id_of(member) == trade_id)
            return &member;
    return nullptr;
}

/**
 * @brief Every member of an array that belongs to the trade, in array order.
 */
template <typename T>
[[nodiscard]] std::vector<T> find_all(const std::vector<T>& members, boost::uuids::uuid trade_id) {
    std::vector<T> found;
    for (const auto& member : members)
        if (trade_id_of(member) == trade_id)
            found.push_back(member);
    return found;
}

}

#endif
