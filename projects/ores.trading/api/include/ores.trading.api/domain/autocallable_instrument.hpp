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
#ifndef ORES_TRADING_API_DOMAIN_AUTOCALLABLE_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_AUTOCALLABLE_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/digest/canonical_form.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Autocallable (variant 01).
 *
 * Represents Autocallable_01 trades: a note that redeems early once the
 * underlying fixes at or above the trigger, paying the factors accrued to
 * that date. ORE reads it through a class of its own, and its underlyings may be
 * equities, FX rates or commodities, so it is held in the multi-asset
 * family, where each underlying row keeps its own type. The underlying is an instrument underlying
 * row under the role underlying; the fixing and settlement dates are instrument schedule rows under
 * fixing_schedule and settlement_schedule; the accumulation factors are instrument value rows under
 * accumulation_factor.
 */
struct autocallable_instrument final {
    instrument_identity identity;

    /**
     * @brief The notional, as NotionalAmount states it.
     */
    ores::utility::decimal::decimal notional_amount;

    /**
     * @brief The level below which the note pays less at maturity, as DeterminationLevel states it.
     */
    ores::utility::decimal::decimal determination_level;

    /**
     * @brief The level that redeems the note, as TriggerLevel states it.
     */
    ores::utility::decimal::decimal trigger_level;

    /**
     * @brief Long or Short, as Position states it.
     */
    std::string long_short;

    /**
     * @brief The currency the product pays in, as PayCcy states it.
     */
    std::string pay_currency;

    /**
     * @brief The cap on the accumulated factor, as Cap states it.
     */
    ores::utility::decimal::decimal cap;

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
    friend bool operator==(const autocallable_instrument&,
                           const autocallable_instrument&) = default;
};

/**
 * @brief The canonical text of the members of autocallable_instrument the customer agreed to.
 *
 * Renders, in member order, the columns the model declares economic and each
 * group whose own model declares an economic field. The economic digest hashes
 * this text, so a member the customer did not agree to never moves it.
 */
[[nodiscard]] inline std::string canonical_form(const autocallable_instrument& v) {
    std::string out = "{";
    ores::utility::digest::append_member(out, "notional_amount", v.notional_amount);
    ores::utility::digest::append_member(out, "determination_level", v.determination_level);
    ores::utility::digest::append_member(out, "trigger_level", v.trigger_level);
    ores::utility::digest::append_member(out, "long_short", v.long_short);
    ores::utility::digest::append_member(out, "pay_currency", v.pay_currency);
    ores::utility::digest::append_member(out, "cap", v.cap);
    out.push_back('}');
    return out;
}

/**
 * @brief Dispatch-key identifier for autocallable_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const autocallable_instrument&) {
    return "ores.trading.autocallable_instrument";
}

}

#endif
