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
 * Template: cpp_field_group.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_API_DOMAIN_BOND_OPTION_DATA_HPP
#define ORES_TRADING_API_DOMAIN_BOND_OPTION_DATA_HPP

#include "ores.trading.api/domain/bond_option_exercise.hpp"
#include "ores.trading.api/domain/bond_option_exercise_fee.hpp"
#include "ores.trading.api/domain/bond_option_payment_data.hpp"
#include "ores.trading.api/domain/bond_option_premium.hpp"
#include "ores.trading.api/domain/bond_option_settlement.hpp"
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::domain {

/**
 * @brief The option block, shared by every product that states one.
 *
 * bondOptionData and AscotData hold the same optionData element, and the equity,
 * FX and commodity products hold it too. The nine tables carry no option column
 * beyond the type and the strike, so the container holds the block whole and
 * export re-emits it.
 *
 * The schema requires the long and short flag and makes every other member
 * optional, so a member the document omits stays absent. The premium amount and
 * the exercise price list are text in the schema, not numbers, and stay text
 * here, and the automatic exercise flag carries the document's own spelling of
 * the schema's bool type.
 */
struct bond_option_data {
    /**
     * @brief The document's own spelling of the long or short flag.
     */
    std::string long_short;

    /**
     * @brief The kind of option the document states.
     */
    std::optional<std::string> option_type;

    /**
     * @brief The payoff the option takes.
     */
    std::optional<std::string> payoff_type;

    /**
     * @brief The second payoff the option takes, when the document states one.
     */
    std::optional<std::string> payoff_type_2;

    /**
     * @brief The exercise style, such as European or American.
     */
    std::optional<std::string> style;

    /**
     * @brief How much notice an exercise needs, in ORE's tenor spelling.
     */
    std::optional<std::string> notice_period;

    /**
     * @brief Business day calendar the notice dates are adjusted against.
     */
    std::optional<std::string> notice_calendar;

    /**
     * @brief Business day convention the notice dates are adjusted under.
     */
    std::optional<std::string> notice_convention;

    /**
     * @brief True when the holder may exercise mid-coupon.
     */
    std::optional<std::string> mid_coupon_exercise;

    /**
     * @brief The settlement the option takes.
     */
    std::optional<std::string> settlement;

    /**
     * @brief How the option settles, such as Cash or Physical.
     */
    std::optional<std::string> settlement_method;

    /**
     * @brief True when the payoff lands at expiry.
     */
    std::optional<std::string> pay_off_at_expiry;

    /**
     * @brief The premium amount, as the document spells it.
     */
    std::optional<std::string> premium_amount;

    /**
     * @brief The currency the premium pays in.
     */
    std::optional<std::string> premium_currency;

    /**
     * @brief The date the premium pays on.
     */
    std::optional<std::string> premium_pay_date;

    /**
     * @brief The premium list the document states, in document order.
     */
    std::vector<bond_option_premium> premiums;

    /**
     * @brief The exercise prices, as the document spells them.
     */
    std::optional<std::string> exercise_prices;

    /**
     * @brief The exercise fees the document states, in document order.
     */
    std::vector<bond_option_exercise_fee> exercise_fees;

    /**
     * @brief The settlement period an exercise fee pays after.
     */
    std::optional<std::string> exercise_fee_settlement_period;

    /**
     * @brief Business day calendar the fee settlement dates are adjusted against.
     */
    std::optional<std::string> exercise_fee_settlement_calendar;

    /**
     * @brief Business day convention the fee settlement dates are adjusted under.
     */
    std::optional<std::string> exercise_fee_settlement_convention;

    /**
     * @brief The document's own spelling of the automatic exercise flag.
     */
    std::optional<std::string> automatic_exercise;

    /**
     * @brief The exercise the document states outright, when it states one.
     */
    std::optional<bond_option_exercise> exercise_data;

    /**
     * @brief The option's payment dates, when the document states them.
     */
    std::optional<bond_option_payment_data> payment_data;

    /**
     * @brief The settlement terms the option pays under, when the document states them.
     */
    std::optional<bond_option_settlement> settlement_data;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_option_data&, const bond_option_data&) = default;
};

}

#endif
