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
#ifndef ORES_TRADING_API_DOMAIN_BOND_LEG_DATA_HPP
#define ORES_TRADING_API_DOMAIN_BOND_LEG_DATA_HPP

#include "ores.trading.api/domain/bond_amortization_data.hpp"
#include "ores.trading.api/domain/bond_float_data.hpp"
#include "ores.trading.api/domain/bond_leg_rate_data.hpp"
#include "ores.trading.api/domain/bond_schedule_data.hpp"
#include "ores.trading.api/domain/bond_settlement_data.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::domain {

/**
 * @brief A leg as the document states it, member by member.
 *
 * The fact rows hold the leg's rate and its index. Everything else the leg
 * states is either a member here or a term on the issue row, and the two
 * directions are:
 *
 * - currency and day_counter have issue columns, so the mapper mirrors them
 *   there and this type carries the document's own statement. On export the
 *   document wins and the row is the fallback, which matters when the container
 *   came from a row set rather than a document.
 * - payer, leg_type and the payment terms have no column: the payer and the leg
 *   type are document flags, and the payment terms, the payment calendar and the
 *   two lag members are the destination of the shared instrument-keyed tables
 *   the parent story describes.
 * - the schedules and the three lists of rows go to those same tables.
 *
 * Every member is an optional, so a member the document states and this
 * container does not hold stays distinguishable from one the document omits. A
 * member the schema declares required is engaged by every document, and an
 * unengaged one then means the container came from a row set.
 *
 * A bond states one leg per coupon, and the schema declares the element
 * unbounded, so the instrument holds a list of these and not one.
 */
struct bond_leg_data {
    /**
     * @brief True when the leg's holder pays, and false when it receives.
     */
    std::optional<bool> payer;

    /**
     * @brief The kind of leg the document states, such as Fixed or Floating.
     */
    std::optional<std::string> leg_type;

    /**
     * @brief The currency the leg pays in.
     */
    std::optional<std::string> currency;

    /**
     * @brief Business day convention the payment dates are adjusted under.
     */
    std::optional<std::string> payment_convention;

    /**
     * @brief How long after each period end the payment falls, in ORE's tenor spelling.
     */
    std::optional<std::string> payment_lag;

    /**
     * @brief Business day calendar the payment dates are adjusted against.
     */
    std::optional<std::string> payment_calendar;

    /**
     * @brief Day count convention the leg accrues under.
     */
    std::optional<std::string> day_counter;

    /**
     * @brief Day count convention the final period accrues under, when the document states one.
     */
    std::optional<std::string> last_period_day_counter;

    /**
     * @brief How many days after each period end the notional payment falls.
     */
    std::optional<std::int64_t> notional_payment_lag;

    /**
     * @brief True when the notional payments keep the leg's own dates.
     */
    std::optional<bool> strict_notional_dates;

    /**
     * @brief The schedule the leg's accrual periods follow.
     */
    bond_schedule_data schedule;

    /**
     * @brief The amortization rows the document states, in document order.
     */
    std::vector<bond_amortization_data> amortizations;

    /**
     * @brief The notional steps the document states, one bond_float_data row each.
     */
    std::vector<bond_float_data> notionals;

    /**
     * @brief The payment dates the document states outright, in document order.
     */
    std::vector<std::string> payment_dates;

    /**
     * @brief True when the leg takes its indexings from the asset leg.
     */
    std::optional<bool> indexings_from_asset_leg;

    /**
     * @brief The leg's rate group, when the document states one.
     */
    std::optional<bond_leg_rate_data> rate;

    /**
     * @brief The schedule the leg's payment dates follow.
     */
    bond_schedule_data payment_schedule;

    /**
     * @brief The leg's settlement block, when the document states one.
     */
    std::optional<bond_settlement_data> settlement;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_leg_data&, const bond_leg_data&) = default;
};

}

#endif
