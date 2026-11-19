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
#ifndef ORES_TRADING_API_DOMAIN_EQUITY_GENERIC_BARRIER_OPTION_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_EQUITY_GENERIC_BARRIER_OPTION_INSTRUMENT_HPP

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
 * @brief Equity generic barrier option.
 *
 * Represents EquityGenericBarrierOption trades: a vanilla, asset-or-nothing
 * or cash-or-nothing option with barriers watched on a schedule. ORE reads it
 * through a class of its own. The OptionData block is an instrument option row
 * with its exercise dates; each BarrierData under Barriers is an instrument
 * barrier row under the role barrier, and each under TransatlanticBarrier
 * one under transatlantic; the barrier ScheduleData is an instrument
 * schedule row under barrier_schedule; the underlying is an instrument
 * underlying row under the role underlying.
 */
struct equity_generic_barrier_option_instrument final {
    instrument_identity identity;

    /**
     * @brief The currency the option pays in, as PayCurrency states it.
     */
    std::string pay_currency;

    /**
     * @brief The number of units, as Quantity states it.
     */
    std::optional<double> quantity;

    /**
     * @brief The strike, as Strike states it.
     */
    std::optional<ores::utility::decimal::decimal> strike;

    /**
     * @brief The cash amount a cash-or-nothing payoff pays, as Amount states it.
     */
    std::optional<ores::utility::decimal::decimal> amount;

    /**
     * @brief The date the option pays, as SettlementDate states it.
     */
    std::optional<std::chrono::year_month_day> settlement_date;

    /**
     * @brief The period from expiry to payment, as SettlementLag states it.
     */
    std::optional<std::string> settlement_lag;

    /**
     * @brief The calendar of the settlement lag, as SettlementCalendar states it.
     */
    std::optional<std::string> settlement_calendar;

    /**
     * @brief The convention of the settlement lag, as SettlementConvention states it.
     */
    std::optional<std::string> settlement_convention;

    /**
     * @brief The first date the barriers are watched, as the StartDate of Barriers states it.
     */
    std::optional<std::chrono::year_month_day> barrier_start_date;

    /**
     * @brief The last date the barriers are watched, as the EndDate of Barriers states it.
     */
    std::optional<std::chrono::year_month_day> barrier_end_date;

    /**
     * @brief How a knock-in and a knock-out interact, as the KikoType of Barriers states it.
     */
    std::optional<std::string> kiko_type;

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
    friend bool operator==(const equity_generic_barrier_option_instrument&,
                           const equity_generic_barrier_option_instrument&) = default;
};

/**
 * @brief The canonical text of the members of equity_generic_barrier_option_instrument the customer
 * agreed to.
 *
 * Renders, in member order, the columns the model declares economic and each
 * group whose own model declares an economic field. The economic digest hashes
 * this text, so a member the customer did not agree to never moves it.
 */
[[nodiscard]] inline std::string canonical_form(const equity_generic_barrier_option_instrument& v) {
    std::string out = "{";
    ores::utility::digest::append_member(out, "pay_currency", v.pay_currency);
    ores::utility::digest::append_member(out, "quantity", v.quantity);
    ores::utility::digest::append_member(out, "strike", v.strike);
    ores::utility::digest::append_member(out, "amount", v.amount);
    ores::utility::digest::append_member(out, "settlement_date", v.settlement_date);
    ores::utility::digest::append_member(out, "settlement_lag", v.settlement_lag);
    ores::utility::digest::append_member(out, "settlement_calendar", v.settlement_calendar);
    ores::utility::digest::append_member(out, "settlement_convention", v.settlement_convention);
    ores::utility::digest::append_member(out, "barrier_start_date", v.barrier_start_date);
    ores::utility::digest::append_member(out, "barrier_end_date", v.barrier_end_date);
    ores::utility::digest::append_member(out, "kiko_type", v.kiko_type);
    out.push_back('}');
    return out;
}

/**
 * @brief Dispatch-key identifier for equity_generic_barrier_option_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view
entity_type_of(const equity_generic_barrier_option_instrument&) {
    return "ores.trading.equity_generic_barrier_option_instrument";
}

}

#endif
