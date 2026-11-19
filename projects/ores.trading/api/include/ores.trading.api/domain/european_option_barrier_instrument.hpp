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
#ifndef ORES_TRADING_API_DOMAIN_EUROPEAN_OPTION_BARRIER_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_EUROPEAN_OPTION_BARRIER_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/digest/canonical_form.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief European option with a barrier on another underlying.
 *
 * Represents EuropeanOptionBarrier trades: a European option on one
 * underlying that knocks in or out on another's level. ORE reads it through a class of its own, and
 * its underlyings may be equities, FX rates or commodities, so it is held in the multi-asset
 * family, where each underlying row keeps its own type. The option
 * underlying and the barrier underlying are instrument underlying rows under
 * the roles optionunderlying and barrierunderlying; the Premiums block
 * is instrument option premium rows; the BarrierSchedule is an instrument
 * schedule row under barrier_schedule.
 */
struct european_option_barrier_instrument final {
    instrument_identity identity;

    /**
     * @brief The number of units, as Quantity states it.
     */
    double quantity{};

    /**
     * @brief Call or Put, as PutCall states it.
     */
    std::string option_type;

    /**
     * @brief Long or Short, as LongShort states it.
     */
    std::string long_short;

    /**
     * @brief The strike, as Strike states it.
     */
    ores::utility::decimal::decimal strike;

    /**
     * @brief The premium, as PremiumAmount states it.
     */
    ores::utility::decimal::decimal premium_amount;

    /**
     * @brief The premium's currency, as PremiumCurrency states it.
     */
    std::string premium_currency;

    /**
     * @brief The date the premium is paid, as PremiumDate states it.
     */
    std::chrono::year_month_day premium_date;

    /**
     * @brief The expiry of the option, as OptionExpiry states it.
     */
    std::chrono::year_month_day option_expiry;

    /**
     * @brief The barrier underlying's level, as BarrierLevel states it.
     */
    ores::utility::decimal::decimal barrier_level;

    /**
     * @brief UpAndOut, UpAndIn, DownAndOut or DownAndIn, as BarrierType states it.
     */
    std::string barrier_type;

    /**
     * @brief European or American, as BarrierStyle states it.
     */
    std::string barrier_style;

    /**
     * @brief The date the option pays, as SettlementDate states it.
     */
    std::chrono::year_month_day settlement_date;

    /**
     * @brief The currency the product pays in, as PayCcy states it.
     */
    std::string pay_currency;

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
    friend bool operator==(const european_option_barrier_instrument&,
                           const european_option_barrier_instrument&) = default;
};

/**
 * @brief The canonical text of the members of european_option_barrier_instrument the customer
 * agreed to.
 *
 * Renders, in member order, the columns the model declares economic and each
 * group whose own model declares an economic field. The economic digest hashes
 * this text, so a member the customer did not agree to never moves it.
 */
[[nodiscard]] inline std::string canonical_form(const european_option_barrier_instrument& v) {
    std::string out = "{";
    ores::utility::digest::append_member(out, "quantity", v.quantity);
    ores::utility::digest::append_member(out, "option_type", v.option_type);
    ores::utility::digest::append_member(out, "long_short", v.long_short);
    ores::utility::digest::append_member(out, "strike", v.strike);
    ores::utility::digest::append_member(out, "premium_amount", v.premium_amount);
    ores::utility::digest::append_member(out, "premium_currency", v.premium_currency);
    ores::utility::digest::append_member(out, "premium_date", v.premium_date);
    ores::utility::digest::append_member(out, "option_expiry", v.option_expiry);
    ores::utility::digest::append_member(out, "barrier_level", v.barrier_level);
    ores::utility::digest::append_member(out, "barrier_type", v.barrier_type);
    ores::utility::digest::append_member(out, "barrier_style", v.barrier_style);
    ores::utility::digest::append_member(out, "settlement_date", v.settlement_date);
    ores::utility::digest::append_member(out, "pay_currency", v.pay_currency);
    out.push_back('}');
    return out;
}

/**
 * @brief Dispatch-key identifier for european_option_barrier_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const european_option_barrier_instrument&) {
    return "ores.trading.european_option_barrier_instrument";
}

}

#endif
