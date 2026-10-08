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
#ifndef ORES_TRADING_API_DOMAIN_BOND_FLOATING_LEG_DATA_HPP
#define ORES_TRADING_API_DOMAIN_BOND_FLOATING_LEG_DATA_HPP

#include "ores.trading.api/domain/bond_float_data.hpp"
#include "ores.trading.api/domain/bond_schedule_data.hpp"
#include "ores.trading.api/domain/bond_stub_interpolation.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::domain {

/**
 * @brief A floating leg's rate terms.
 *
 * The three lists of numbers (spreads, caps, floors, gearings) all carry the
 * bond_float_data shape. The two schedules are schedules the document states on
 * the leg itself rather than on the leg's outer block.
 */
struct bond_floating_leg_data {
    /**
     * @brief The index the leg's coupon resets against.
     */
    std::string index;

    /**
     * @brief True when the coupon fixes after the accrual period ends.
     */
    std::optional<bool> is_in_arrears;

    /**
     * @brief The most recent period the document states specially, when it states one.
     */
    std::optional<std::string> last_recent_period;

    /**
     * @brief The calendar that resolves the special period, when the document states one.
     */
    std::optional<std::string> last_recent_period_calendar;

    /**
     * @brief How many days before the period end the index fixes.
     */
    std::optional<std::uint64_t> fixing_days;

    /**
     * @brief The backward-looking window the index averages over, in ORE's tenor spelling.
     */
    std::optional<std::string> lookback;

    /**
     * @brief How many days before the period end the rate is cut off.
     */
    std::optional<std::int64_t> rate_cutoff;

    /**
     * @brief True when the coupon averages the index over the period.
     */
    std::optional<bool> is_averaged;

    /**
     * @brief True when the period splits into several fixing sub-periods.
     */
    std::optional<bool> has_sub_periods;

    /**
     * @brief True when the spread is added to the averaged index.
     */
    std::optional<bool> include_spread;

    /**
     * @brief True when the cross-currency reset is switched off.
     */
    std::optional<bool> is_not_resetting_xccy;

    /**
     * @brief The spreads the document states, one bond_float_data row each.
     */
    std::vector<bond_float_data> spreads;

    /**
     * @brief The caps the document states, one bond_float_data row each.
     */
    std::vector<bond_float_data> caps;

    /**
     * @brief The floors the document states, one bond_float_data row each.
     */
    std::vector<bond_float_data> floors;

    /**
     * @brief The gearings the document states, one bond_float_data row each.
     */
    std::vector<bond_float_data> gearings;

    /**
     * @brief True when the leg's cap or floor stands alone.
     */
    std::optional<bool> naked_option;

    /**
     * @brief True when the cap or floor applies to the leg only.
     */
    std::optional<bool> local_cap_floor;

    /**
     * @brief The schedule the coupon's fixings follow.
     */
    bond_schedule_data fixing_schedule;

    /**
     * @brief The schedule the coupon's resets follow.
     */
    bond_schedule_data reset_schedule;

    /**
     * @brief The indices the front stub interpolates between, when the document states them.
     */
    std::optional<bond_stub_interpolation> front_stub_interpolation;

    /**
     * @brief The indices the back stub interpolates between, when the document states them.
     */
    std::optional<bond_stub_interpolation> back_stub_interpolation;

    /**
     * @brief True when a stub discounts on the original curve.
     */
    std::optional<bool> stub_use_original_curve;

    /**
     * @brief True when an observation shift moves the fixing dates.
     */
    std::optional<bool> observation_shift;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_floating_leg_data&, const bond_floating_leg_data&) = default;
};

}

#endif
