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
#ifndef ORES_TRADING_API_DOMAIN_FX_BEST_ENTRY_OPTION_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_FX_BEST_ENTRY_OPTION_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/digest/canonical_form.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief FX best entry option.
 *
 * Represents FxBestEntryOption trades: an option whose strike is the best level the
 * underlying reaches over the strike observation dates, reset only when it
 * crosses the trigger level. ORE reads it through a class of its own. The
 * underlying is an instrument underlying row under the role underlying;
 * an FX index is named FX- and the index. The StrikeObservationDates are an instrument schedule
 * row under the role strike_observation_schedule.
 */
struct fx_best_entry_option_instrument final {
    instrument_identity identity;

    /**
     * @brief Long or Short, as LongShort states it.
     */
    std::string long_short;

    /**
     * @brief The currency the option pays in, as Currency states it.
     */
    std::string currency;

    /**
     * @brief The amount the payoff is multiplied by, as Notional states it.
     */
    ores::utility::decimal::decimal notional;

    /**
     * @brief The participation in the performance, as Multiplier states it.
     */
    ores::utility::decimal::decimal multiplier;

    /**
     * @brief The cap on the performance, as Cap states it.
     */
    ores::utility::decimal::decimal cap;

    /**
     * @brief The initial strike as a fraction of the level, as Strike states it.
     */
    ores::utility::decimal::decimal strike;

    /**
     * @brief The level, as a fraction of the initial level, below which the strike resets, as
     * TriggerLevel states it.
     */
    ores::utility::decimal::decimal trigger_level;

    /**
     * @brief The floor on a reset strike as a fraction of the initial level, as ResetMinimum states
     * it.
     */
    ores::utility::decimal::decimal reset_minimum;

    /**
     * @brief The premium amount, as Premium states it.
     */
    ores::utility::decimal::decimal premium;

    /**
     * @brief The expiry of the option, as ExpiryDate states it.
     */
    std::chrono::year_month_day expiry_date;

    /**
     * @brief The date the initial level is set, as StrikeDate states it.
     */
    std::chrono::year_month_day strike_date;

    /**
     * @brief The date the premium is paid, as PremiumDate states it.
     */
    std::chrono::year_month_day premium_date;

    /**
     * @brief The date the option pays, as SettlementDate states it.
     */
    std::chrono::year_month_day settlement_date;

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
    friend bool operator==(const fx_best_entry_option_instrument&,
                           const fx_best_entry_option_instrument&) = default;
};

/**
 * @brief The canonical text of the members of fx_best_entry_option_instrument the customer agreed
 * to.
 *
 * Renders, in member order, the columns the model declares economic and each
 * group whose own model declares an economic field. The economic digest hashes
 * this text, so a member the customer did not agree to never moves it.
 */
[[nodiscard]] inline std::string canonical_form(const fx_best_entry_option_instrument& v) {
    std::string out = "{";
    ores::utility::digest::append_member(out, "long_short", v.long_short);
    ores::utility::digest::append_member(out, "currency", v.currency);
    ores::utility::digest::append_member(out, "notional", v.notional);
    ores::utility::digest::append_member(out, "multiplier", v.multiplier);
    ores::utility::digest::append_member(out, "cap", v.cap);
    ores::utility::digest::append_member(out, "strike", v.strike);
    ores::utility::digest::append_member(out, "trigger_level", v.trigger_level);
    ores::utility::digest::append_member(out, "reset_minimum", v.reset_minimum);
    ores::utility::digest::append_member(out, "premium", v.premium);
    ores::utility::digest::append_member(out, "expiry_date", v.expiry_date);
    ores::utility::digest::append_member(out, "strike_date", v.strike_date);
    ores::utility::digest::append_member(out, "premium_date", v.premium_date);
    ores::utility::digest::append_member(out, "settlement_date", v.settlement_date);
    out.push_back('}');
    return out;
}

/**
 * @brief Dispatch-key identifier for fx_best_entry_option_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const fx_best_entry_option_instrument&) {
    return "ores.trading.fx_best_entry_option_instrument";
}

}

#endif
