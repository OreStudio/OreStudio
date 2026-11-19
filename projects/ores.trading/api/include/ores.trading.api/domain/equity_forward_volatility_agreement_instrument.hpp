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
#ifndef ORES_TRADING_API_DOMAIN_EQUITY_FORWARD_VOLATILITY_AGREEMENT_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_EQUITY_FORWARD_VOLATILITY_AGREEMENT_INSTRUMENT_HPP

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
 * @brief Equity forward volatility agreement.
 *
 * Represents EquityForwardVolatilityAgreement trades: an agreement to buy, on the FVA date, a
 * straddle struck at the level then and priced at the implied volatility
 * agreed now. ORE reads it through a class of its own. The underlying is an
 * instrument underlying row under the role underlying; an equity is named EQ- and its address.
 */
struct equity_forward_volatility_agreement_instrument final {
    instrument_identity identity;

    /**
     * @brief The date the straddle is struck, as FvaDate states it.
     */
    std::chrono::year_month_day fva_date;

    /**
     * @brief The expiry of the straddle, as OptionExpiry states it.
     */
    std::chrono::year_month_day option_expiry;

    /**
     * @brief The date the premium is paid, as PremiumDate states it.
     */
    std::chrono::year_month_day premium_date;

    /**
     * @brief Long or Short, as LongShort states it.
     */
    std::string long_short;

    /**
     * @brief The implied volatility agreed now, as ImpliedVolStrike states it.
     */
    double implied_vol_strike{};

    /**
     * @brief The number of straddles, as Quantity states it.
     */
    double quantity{};

    /**
     * @brief The currency the agreement pays in, as PayCcy states it.
     */
    std::string pay_currency;

    /**
     * @brief The date the straddle pays, as SettlementDate states it.
     */
    std::chrono::year_month_day settlement_date;

    /**
     * @brief The strike of the straddle, as UnderlyingStrike states it; absent is the level on the
     * FVA date.
     */
    std::optional<ores::utility::decimal::decimal> underlying_strike;

    /**
     * @brief The day count of the time to expiry, as DayCountFraction states it.
     */
    std::optional<std::string> day_count_fraction;

    /**
     * @brief The dividend yield of the underlying, as DividendYield states it.
     */
    std::optional<ores::utility::decimal::decimal> dividend_yield;

    /**
     * @brief The rate that discounts the payoff, as FixedRate states it.
     */
    std::optional<ores::utility::decimal::decimal> fixed_rate;

    /**
     * @brief The payoff the script computes, as PayoffType states it.
     */
    std::optional<std::string> payoff_type;

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
    friend bool operator==(const equity_forward_volatility_agreement_instrument&,
                           const equity_forward_volatility_agreement_instrument&) = default;
};

/**
 * @brief The canonical text of the members of equity_forward_volatility_agreement_instrument the
 * customer agreed to.
 *
 * Renders, in member order, the columns the model declares economic and each
 * group whose own model declares an economic field. The economic digest hashes
 * this text, so a member the customer did not agree to never moves it.
 */
[[nodiscard]] inline std::string
canonical_form(const equity_forward_volatility_agreement_instrument& v) {
    std::string out = "{";
    ores::utility::digest::append_member(out, "fva_date", v.fva_date);
    ores::utility::digest::append_member(out, "option_expiry", v.option_expiry);
    ores::utility::digest::append_member(out, "premium_date", v.premium_date);
    ores::utility::digest::append_member(out, "long_short", v.long_short);
    ores::utility::digest::append_member(out, "implied_vol_strike", v.implied_vol_strike);
    ores::utility::digest::append_member(out, "quantity", v.quantity);
    ores::utility::digest::append_member(out, "pay_currency", v.pay_currency);
    ores::utility::digest::append_member(out, "settlement_date", v.settlement_date);
    ores::utility::digest::append_member(out, "underlying_strike", v.underlying_strike);
    ores::utility::digest::append_member(out, "day_count_fraction", v.day_count_fraction);
    ores::utility::digest::append_member(out, "dividend_yield", v.dividend_yield);
    ores::utility::digest::append_member(out, "fixed_rate", v.fixed_rate);
    ores::utility::digest::append_member(out, "payoff_type", v.payoff_type);
    out.push_back('}');
    return out;
}

/**
 * @brief Dispatch-key identifier for equity_forward_volatility_agreement_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view
entity_type_of(const equity_forward_volatility_agreement_instrument&) {
    return "ores.trading.equity_forward_volatility_agreement_instrument";
}

}

#endif
