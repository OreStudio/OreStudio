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
#ifndef ORES_TRADING_API_DOMAIN_EQUITY_STRIKE_RESETTABLE_OPTION_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_EQUITY_STRIKE_RESETTABLE_OPTION_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/digest/canonical_form.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Equity strike resettable option.
 *
 * Represents EquityStrikeResettableOption trades: an option whose strike resets to the reset
 * strike when the underlying crosses the trigger price on an observation
 * date. ORE reads it through a class of its own. The underlying is an
 * instrument underlying row under the role underlying; an equity is named EQ- and its address.
 * The ObservationDates are an instrument schedule row under the role
 * observation_schedule.
 */
struct equity_strike_resettable_option_instrument final {
    instrument_identity identity;

    /**
     * @brief Long or Short, as LongShort states it.
     */
    std::string long_short;

    /**
     * @brief Call or Put, as OptionType states it.
     */
    std::string option_type;

    /**
     * @brief The currency the option pays in, as Currency states it.
     */
    std::string currency;

    /**
     * @brief The strike before any reset, as Strike states it.
     */
    ores::utility::decimal::decimal strike;

    /**
     * @brief The strike after a reset, as ResetStrike states it.
     */
    ores::utility::decimal::decimal reset_strike;

    /**
     * @brief The number of units, as Quantity states it.
     */
    double quantity{};

    /**
     * @brief Up or Down: the side the underlying crosses the trigger price on, as TriggerType
     * states it.
     */
    std::string trigger_type;

    /**
     * @brief The level that resets the strike, as TriggerPrice states it.
     */
    ores::utility::decimal::decimal trigger_price;

    /**
     * @brief The expiry of the option, as ExpiryDate states it.
     */
    std::chrono::year_month_day expiry_date;

    /**
     * @brief The date the option pays, as SettlementDate states it.
     */
    std::chrono::year_month_day settlement_date;

    /**
     * @brief The premium amount, as Premium states it.
     */
    ores::utility::decimal::decimal premium;

    /**
     * @brief The date the premium is paid, as PremiumDate states it.
     */
    std::chrono::year_month_day premium_date;

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
    friend bool operator==(const equity_strike_resettable_option_instrument&,
                           const equity_strike_resettable_option_instrument&) = default;
};

/**
 * @brief The canonical text of the members of equity_strike_resettable_option_instrument the
 * customer agreed to.
 *
 * Renders, in member order, the columns the model declares economic and each
 * group whose own model declares an economic field. The economic digest hashes
 * this text, so a member the customer did not agree to never moves it.
 */
[[nodiscard]] inline std::string
canonical_form(const equity_strike_resettable_option_instrument& v) {
    std::string out = "{";
    ores::utility::digest::append_member(out, "long_short", v.long_short);
    ores::utility::digest::append_member(out, "option_type", v.option_type);
    ores::utility::digest::append_member(out, "currency", v.currency);
    ores::utility::digest::append_member(out, "strike", v.strike);
    ores::utility::digest::append_member(out, "reset_strike", v.reset_strike);
    ores::utility::digest::append_member(out, "quantity", v.quantity);
    ores::utility::digest::append_member(out, "trigger_type", v.trigger_type);
    ores::utility::digest::append_member(out, "trigger_price", v.trigger_price);
    ores::utility::digest::append_member(out, "expiry_date", v.expiry_date);
    ores::utility::digest::append_member(out, "settlement_date", v.settlement_date);
    ores::utility::digest::append_member(out, "premium", v.premium);
    ores::utility::digest::append_member(out, "premium_date", v.premium_date);
    out.push_back('}');
    return out;
}

/**
 * @brief Dispatch-key identifier for equity_strike_resettable_option_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view
entity_type_of(const equity_strike_resettable_option_instrument&) {
    return "ores.trading.equity_strike_resettable_option_instrument";
}

}

#endif
