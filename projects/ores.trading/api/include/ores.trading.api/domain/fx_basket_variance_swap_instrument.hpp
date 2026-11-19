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
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_API_DOMAIN_FX_BASKET_VARIANCE_SWAP_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_FX_BASKET_VARIANCE_SWAP_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/digest/canonical_form.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief FX basket variance swap.
 *
 * Represents FxBasketVarianceSwap trades: a swap on the realised variance of a weighted
 * basket, which ORE reads through a class of its own. The members are
 * instrument underlying rows under the role underlying; an FX index is named FX- and the
 * index.
 * The ValuationSchedule is an instrument schedule row under the role
 * valuation_schedule.
 */
struct fx_basket_variance_swap_instrument final {
    instrument_identity identity;

    /**
     * @brief Long or Short, as LongShort states it.
     */
    std::string long_short;

    /**
     * @brief The strike as a volatility, such as 0.2, as Strike states it.
     */
    ores::utility::decimal::decimal strike;

    /**
     * @brief The amount the payoff is multiplied by, as Notional states it.
     */
    ores::utility::decimal::decimal notional;

    /**
     * @brief Whether the swap pays on variance rather than volatility, as SquaredPayoff states it.
     */
    bool squared_payoff = false;

    /**
     * @brief The cap on the realised variance, as Cap states it.
     */
    std::optional<ores::utility::decimal::decimal> cap;

    /**
     * @brief The floor on the realised variance, as Floor states it.
     */
    std::optional<ores::utility::decimal::decimal> floor;

    /**
     * @brief The date the swap pays, as SettlementDate states it.
     */
    std::chrono::year_month_day settlement_date;

    /**
     * @brief The currency the swap pays in, as Currency states it.
     */
    std::string currency;

    /**
     * @brief Optional free-text description.
     */
    std::string description;

    ores::dq::domain::audit_record audit;
    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const fx_basket_variance_swap_instrument&,
                           const fx_basket_variance_swap_instrument&) = default;
};

/**
 * @brief The canonical text of the members of fx_basket_variance_swap_instrument the customer
 * agreed to.
 *
 * Renders, in member order, the columns the model declares economic and each
 * group whose own model declares an economic field. The economic digest hashes
 * this text, so a member the customer did not agree to never moves it.
 */
[[nodiscard]] inline std::string canonical_form(const fx_basket_variance_swap_instrument& v) {
    std::string out = "{";
    ores::utility::digest::append_member(out, "long_short", v.long_short);
    ores::utility::digest::append_member(out, "strike", v.strike);
    ores::utility::digest::append_member(out, "notional", v.notional);
    ores::utility::digest::append_member(out, "squared_payoff", v.squared_payoff);
    ores::utility::digest::append_member(out, "cap", v.cap);
    ores::utility::digest::append_member(out, "floor", v.floor);
    ores::utility::digest::append_member(out, "settlement_date", v.settlement_date);
    ores::utility::digest::append_member(out, "currency", v.currency);
    out.push_back('}');
    return out;
}

/**
 * @brief Dispatch-key identifier for fx_basket_variance_swap_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const fx_basket_variance_swap_instrument&) {
    return "ores.trading.fx_basket_variance_swap_instrument";
}

}

#endif
