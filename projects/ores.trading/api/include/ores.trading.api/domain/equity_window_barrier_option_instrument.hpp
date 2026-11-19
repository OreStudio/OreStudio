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
#ifndef ORES_TRADING_API_DOMAIN_EQUITY_WINDOW_BARRIER_OPTION_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_EQUITY_WINDOW_BARRIER_OPTION_INSTRUMENT_HPP

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
 * @brief Equity window barrier option.
 *
 * Represents EquityWindowBarrierOption trades: an option whose barrier is watched only
 * between the start and the end date. ORE reads it through a class of its
 * own. The OptionData block is an instrument option row with its exercise
 * dates; the BarrierData block is an instrument barrier row under the role
 * barrier; the underlying is an instrument underlying row under the role
 * underlying; an equity is named EQ- and its address.
 */
struct equity_window_barrier_option_instrument final {
    instrument_identity identity;

    /**
     * @brief The currency the option pays in, as Currency states it.
     */
    std::string currency;

    /**
     * @brief The amount the payoff is multiplied by, as FixingAmount states it.
     */
    ores::utility::decimal::decimal fixing_amount;

    /**
     * @brief The strike, as Strike or the Value of StrikeData states it.
     */
    ores::utility::decimal::decimal strike;

    /**
     * @brief The currency of the strike, as the Currency of StrikeData states it; absent when the
     * document states a plain Strike.
     */
    std::optional<std::string> strike_currency;

    /**
     * @brief The first date the barrier is watched, as StartDate states it.
     */
    std::chrono::year_month_day start_date;

    /**
     * @brief The last date the barrier is watched, as EndDate states it.
     */
    std::chrono::year_month_day end_date;

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
    friend bool operator==(const equity_window_barrier_option_instrument&,
                           const equity_window_barrier_option_instrument&) = default;
};

/**
 * @brief The canonical text of the members of equity_window_barrier_option_instrument the customer
 * agreed to.
 *
 * Renders, in member order, the columns the model declares economic and each
 * group whose own model declares an economic field. The economic digest hashes
 * this text, so a member the customer did not agree to never moves it.
 */
[[nodiscard]] inline std::string canonical_form(const equity_window_barrier_option_instrument& v) {
    std::string out = "{";
    ores::utility::digest::append_member(out, "currency", v.currency);
    ores::utility::digest::append_member(out, "fixing_amount", v.fixing_amount);
    ores::utility::digest::append_member(out, "strike", v.strike);
    ores::utility::digest::append_member(out, "strike_currency", v.strike_currency);
    ores::utility::digest::append_member(out, "start_date", v.start_date);
    ores::utility::digest::append_member(out, "end_date", v.end_date);
    out.push_back('}');
    return out;
}

/**
 * @brief Dispatch-key identifier for equity_window_barrier_option_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view
entity_type_of(const equity_window_barrier_option_instrument&) {
    return "ores.trading.equity_window_barrier_option_instrument";
}

}

#endif
