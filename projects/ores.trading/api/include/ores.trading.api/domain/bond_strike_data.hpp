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
#ifndef ORES_TRADING_API_DOMAIN_BOND_STRIKE_DATA_HPP
#define ORES_TRADING_API_DOMAIN_BOND_STRIKE_DATA_HPP

#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <optional>
#include <string>
#include <tuple>

namespace ores::trading::domain {

/**
 * @brief The strike, stated as a price, as a yield or as a bare number.
 *
 * A strike the document states as a bare element reaches the option fact row.
 * The schema states the element form as a choice of three: a price with its
 * currency, a yield with its compounding, or a number with an optional currency.
 * Each alternative is a pair here, and the three are mutually exclusive in a
 * document.
 */
struct bond_strike_data {
    /**
     * @brief The strike as a price, when the document states one.
     */
    std::optional<ores::utility::decimal::decimal> price_value;

    /**
     * @brief The currency the price strike is stated in.
     */
    std::optional<std::string> price_currency;

    /**
     * @brief The strike as a yield, when the document states one.
     */
    std::optional<ores::utility::decimal::decimal> yield_value;

    /**
     * @brief The compounding the yield strike is stated under.
     */
    std::optional<std::string> yield_compounding;

    /**
     * @brief The strike as a bare number, when the document states one.
     */
    std::optional<ores::utility::decimal::decimal> bare_value;

    /**
     * @brief The currency the bare strike is stated in, when the document states one.
     */
    std::optional<std::string> bare_currency;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_strike_data&, const bond_strike_data&) = default;
};
/**
 * @brief The members of bond_strike_data, named as the wire names
 * them and in the order the type declares them.
 *
 * A group is a member of the entity that holds it, so the carriers reach this
 * shape through it: the parent's field names the member, and this one names the
 * members inside it.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_strike_data&) {
    return std::tuple{
        utility::codec::field{"price_value", &bond_strike_data::price_value},
        utility::codec::field{"price_currency", &bond_strike_data::price_currency},
        utility::codec::field{"yield_value", &bond_strike_data::yield_value},
        utility::codec::field{"yield_compounding", &bond_strike_data::yield_compounding},
        utility::codec::field{"bare_value", &bond_strike_data::bare_value},
        utility::codec::field{"bare_currency", &bond_strike_data::bare_currency},
    };
}

}

#endif
