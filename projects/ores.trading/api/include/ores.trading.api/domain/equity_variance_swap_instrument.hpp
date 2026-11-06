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
#ifndef ORES_TRADING_API_DOMAIN_EQUITY_VARIANCE_SWAP_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_EQUITY_VARIANCE_SWAP_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/digest/canonical_form.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Equity Variance Swap instrument.
 *
 * Represents EquityVarianceSwap trades.
 */
struct equity_variance_swap_instrument final {
    instrument_identity identity;

    /**
     * @brief Name of the underlying equity.
     */
    std::string underlying_name;

    /**
     * @brief ISO 4217 currency code.
     *
     * Soft FK to ores_refdata_currencies_tbl: ISO 4217 currency codes belong to ores.refdata, so
     * the dependency is recorded rather than copied. PR 4 tightens the soft reference into a real
     * foreign key.
     */
    std::string currency;

    /**
     * @brief Vega notional. Must be positive.
     */
    ores::utility::decimal::decimal notional;

    /**
     * @brief Strike variance.
     *
     * A variance strike is a volatility rather than a money amount, so the column is a binary
     * double for the same reason a basket weight is. The check refuses the values a double admits
     * and a numeric does not: an infinite or not-a-number strike has no meaning and would poison
     * every comparison downstream.
     */
    double variance_strike;

    /**
     * @brief ISO 8601 date string (YYYY-MM-DD).
     */
    std::chrono::year_month_day start_date;

    /**
     * @brief ISO 8601 date string (YYYY-MM-DD).
     */
    std::chrono::year_month_day maturity_date;

    /**
     * @brief Long or Short.
     */
    std::string long_short;

    /**
     * @brief The calendar the swap observes in, as Calendar states it.
     */
    std::optional<std::string> calendar;

    /**
     * @brief Whether the swap pays on variance or volatility, as MomentType states it.
     */
    std::optional<std::string> moment_type;

    /**
     * @brief Whether dividends already paid count, as AddPastDividends states it.
     */
    std::optional<bool> add_past_dividends;

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
    friend bool operator==(const equity_variance_swap_instrument&,
                           const equity_variance_swap_instrument&) = default;
};

/**
 * @brief The canonical text of the members of equity_variance_swap_instrument the customer agreed
 * to.
 *
 * Renders, in member order, the columns the model declares economic and each
 * group whose own model declares an economic field. The economic digest hashes
 * this text, so a member the customer did not agree to never moves it.
 */
[[nodiscard]] inline std::string canonical_form(const equity_variance_swap_instrument& v) {
    std::string out = "{";
    ores::utility::digest::append_member(out, "underlying_name", v.underlying_name);
    ores::utility::digest::append_member(out, "currency", v.currency);
    ores::utility::digest::append_member(out, "notional", v.notional);
    ores::utility::digest::append_member(out, "variance_strike", v.variance_strike);
    ores::utility::digest::append_member(out, "start_date", v.start_date);
    ores::utility::digest::append_member(out, "maturity_date", v.maturity_date);
    ores::utility::digest::append_member(out, "long_short", v.long_short);
    ores::utility::digest::append_member(out, "calendar", v.calendar);
    ores::utility::digest::append_member(out, "moment_type", v.moment_type);
    ores::utility::digest::append_member(out, "add_past_dividends", v.add_past_dividends);
    out.push_back('}');
    return out;
}

/**
 * @brief Dispatch-key identifier for equity_variance_swap_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const equity_variance_swap_instrument&) {
    return "ores.trading.equity_variance_swap_instrument";
}

}

#endif
