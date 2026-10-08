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
#ifndef ORES_TRADING_API_DOMAIN_BOND_FORMULA_BASED_LEG_DATA_HPP
#define ORES_TRADING_API_DOMAIN_BOND_FORMULA_BASED_LEG_DATA_HPP

#include <cstdint>
#include <optional>
#include <string>

namespace ores::trading::domain {

/**
 * @brief A formula-based leg's rate terms.
 *
 */
struct bond_formula_based_leg_data {
    /**
     * @brief The index the formula reads.
     */
    std::string index;

    /**
     * @brief True when the formula fixes after the accrual period ends.
     */
    std::optional<bool> is_in_arrears;

    /**
     * @brief How many days before the period end the formula fixes.
     */
    std::int64_t fixing_days = {};

    /**
     * @brief The calendar that resolves the fixing dates, when the document states one.
     */
    std::optional<std::string> fixing_calendar;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_formula_based_leg_data&,
                           const bond_formula_based_leg_data&) = default;
};

}

#endif
