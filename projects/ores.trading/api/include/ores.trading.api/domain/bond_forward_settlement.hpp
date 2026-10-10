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
#ifndef ORES_TRADING_API_DOMAIN_BOND_FORWARD_SETTLEMENT_HPP
#define ORES_TRADING_API_DOMAIN_BOND_FORWARD_SETTLEMENT_HPP

#include "ores.utility/decimal/decimal.hpp"
#include <optional>
#include <string>

namespace ores::trading::domain {

/**
 * @brief A forward bond's settlement block.
 *
 * The forward maturity date is required and every other member is optional. The
 * nine tables carry no forward settlement column, so the container holds the
 * block whole and export re-emits it.
 */
struct bond_forward_settlement {
    /**
     * @brief The forward's maturity date, as ISO 8601 text.
     */
    std::string forward_maturity_date;

    /**
     * @brief The date the forward settles on, when the document states one.
     */
    std::optional<std::string> forward_settlement_date;

    /**
     * @brief The settlement the forward takes, when the document states one.
     */
    std::optional<std::string> settlement;

    /**
     * @brief The amount the forward settles for, when the document states one.
     */
    std::optional<ores::utility::decimal::decimal> amount;

    /**
     * @brief The rate the forward locks, when the document states one.
     */
    std::optional<ores::utility::decimal::decimal> lock_rate;

    /**
     * @brief The position's sensitivity to a basis point, when the document states one.
     */
    std::optional<double> dv01;

    /**
     * @brief Day count convention the lock rate accrues under, when the document states one.
     */
    std::optional<std::string> lock_rate_day_counter;

    /**
     * @brief The dirty settlement the forward takes, when the document states one.
     */
    std::optional<std::string> settlement_dirty;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_forward_settlement&,
                           const bond_forward_settlement&) = default;
};

}

#endif
