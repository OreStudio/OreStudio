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
#ifndef ORES_TRADING_API_DOMAIN_BOND_LEG_RATE_DATA_HPP
#define ORES_TRADING_API_DOMAIN_BOND_LEG_RATE_DATA_HPP

#include "ores.trading.api/domain/bond_fixed_leg_data.hpp"
#include "ores.trading.api/domain/bond_floating_leg_data.hpp"
#include "ores.trading.api/domain/bond_formula_based_leg_data.hpp"
#include <optional>

namespace ores::trading::domain {

/**
 * @brief The rate group of a leg, one alternative per leg type.
 *
 * The ORE schema states the group as a choice of eighteen alternatives. A bond
 * leg carries a coupon, and the corpus states three of them: fixed, floating and
 * formula-based. The other fifteen are a recorded boundary, and the alternative
 * a document states is the engaged member here.
 */
struct bond_leg_rate_data {
    /**
     * @brief The fixed leg's rate terms, when the document states a fixed coupon.
     */
    std::optional<bond_fixed_leg_data> fixed;

    /**
     * @brief The floating leg's rate terms, when the document states a floating coupon.
     */
    std::optional<bond_floating_leg_data> floating;

    /**
     * @brief The formula-based leg's rate terms, when the document states that coupon.
     */
    std::optional<bond_formula_based_leg_data> formula_based;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_leg_rate_data&, const bond_leg_rate_data&) = default;
};

}

#endif
