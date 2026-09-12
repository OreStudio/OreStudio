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
#ifndef ORES_TRADING_API_DOMAIN_INSTRUMENT_OPTION_HPP
#define ORES_TRADING_API_DOMAIN_INSTRUMENT_OPTION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief The option block a document states on an instrument, keyed to the instrument.
 *
 * One row per instrument whose document stated an option block, keyed to
 * the instrument.
 *
 * The bond option and the ascot both state the same optionData element,
 * and the equity, FX and commodity products state it too. The nine bond
 * tables carry the option's type and its strike and nothing else, so this
 * table holds the block: the exercise and payment terms, the settlement
 * terms, and the three nested groups whose lists get child tables of
 * their own.
 *
 * A member the document omits is a null column. Three members are
 * themselves groups whose every member is optional, so a null column set
 * cannot say whether the document stated the group and left it bare or
 * omitted it. Each of those three carries a flag on this row:
 * has_exercise_data, has_payment_data and has_settlement_data.
 *
 * Every other member is text, and the text is the document's own
 * spelling. The schema types the premium amount, the exercise price list
 * and the several flags as text or as its own bool, which enumerates
 * thirteen spellings, so a decoded form would not re-emit what the
 * document held.
 */
struct instrument_option final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID of the instrument whose document stated this option block.
     */
    boost::uuids::uuid instrument_id;

    /**
     * @brief Whether the holder is long or short the option.
     *
     * The schema declares the member required, so a row here always states it. The type is the
     * schema's own enumeration of two values.
     */
    std::string long_short;

    /**
     * @brief The option's type, as the document spelled it.
     */
    std::optional<std::string> option_type;

    /**
     * @brief The payoff the option settles to.
     */
    std::optional<std::string> payoff_type;

    /**
     * @brief The second payoff, for the products that state two.
     */
    std::optional<std::string> payoff_type_2;

    /**
     * @brief Exercise style, for example European or American.
     */
    std::optional<std::string> style;

    /**
     * @brief Notice period the holder must give before exercising.
     */
    std::optional<std::string> notice_period;

    /**
     * @brief Calendar the notice period is counted against.
     */
    std::optional<std::string> notice_calendar;

    /**
     * @brief Date convention the notice period rolls under.
     */
    std::optional<std::string> notice_convention;

    /**
     * @brief Flag saying the option may be exercised between coupons.
     */
    std::optional<std::string> mid_coupon_exercise;

    /**
     * @brief Settlement type the option pays under.
     */
    std::optional<std::string> settlement;

    /**
     * @brief Method the settlement takes, for example cash or physical.
     */
    std::optional<std::string> settlement_method;

    /**
     * @brief Flag saying the payoff falls due at expiry.
     */
    std::optional<std::string> pay_off_at_expiry;

    /**
     * @brief Premium amount for the single-premium spelling.
     *
     * The column is text because the schema states the amount as text, and the corpus states
     * amounts the numeric form would reformat. The premium list spelling keeps its rows in the
     * premium table.
     */
    std::optional<std::string> premium_amount;

    /**
     * @brief Currency the single premium pays in.
     */
    std::optional<std::string> premium_currency;

    /**
     * @brief Date the single premium pays on.
     */
    std::optional<std::string> premium_pay_date;

    /**
     * @brief The exercise price list, as the document spelled it.
     *
     * The schema types the member as text, so the column keeps the document's own text rather than
     * a decoded list of numbers.
     */
    std::optional<std::string> exercise_prices;

    /**
     * @brief Period an exercise fee settles over.
     */
    std::optional<std::string> exercise_fee_settlement_period;

    /**
     * @brief Calendar an exercise fee settles against.
     */
    std::optional<std::string> exercise_fee_settlement_calendar;

    /**
     * @brief Date convention an exercise fee settles under.
     */
    std::optional<std::string> exercise_fee_settlement_convention;

    /**
     * @brief Flag saying the option exercises itself when it is in the money.
     *
     * The schema's bool type enumerates thirteen spellings, so the column keeps the document's own
     * spelling rather than a canonical one.
     */
    std::optional<std::string> automatic_exercise;

    /**
     * @brief True when the document stated an exercise block, whether or not the block carried a
     * price.
     */
    bool has_exercise_data = false;

    /**
     * @brief Date the exercise block names.
     *
     * The schema types this member as a date, so the generated reader has already parsed it and the
     * writer re-emits a canonical form. A date column therefore carries the member whole.
     */
    std::optional<std::string> exercise_date;

    /**
     * @brief Price the exercise block states, when it states one.
     */
    std::optional<double> exercise_price;

    /**
     * @brief True when the document stated a payment-dates block.
     */
    bool has_payment_data = false;

    /**
     * @brief Number of days the payment dates are derived forward by.
     */
    std::optional<std::int64_t> payment_lag;

    /**
     * @brief Calendar the payment dates roll against.
     */
    std::optional<std::string> payment_calendar;

    /**
     * @brief Date convention the payment dates roll under.
     */
    std::optional<std::string> payment_convention;

    /**
     * @brief The date the payment dates are measured from.
     */
    std::optional<std::string> payment_relative_to;

    /**
     * @brief True when the document stated a settlement block.
     */
    bool has_settlement_data = false;

    /**
     * @brief Currency the settlement block pays in.
     */
    std::optional<std::string> settlement_pay_currency;

    /**
     * @brief FX index the settlement block fixes against.
     */
    std::optional<std::string> settlement_fx_index;

    /**
     * @brief Date the settlement block fixes on, when the document states one.
     */
    std::optional<std::string> settlement_fixing_date;

    /**
     * @brief Username of the person who last modified this instrument option.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

/**
 * @brief Dispatch-key identifier for instrument_option, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const instrument_option&) {
    return "ores.trading.instrument_option";
}

}

#endif
