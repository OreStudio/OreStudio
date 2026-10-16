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
#ifndef ORES_TRADING_API_DOMAIN_BOND_AMORTIZATION_DATA_HPP
#define ORES_TRADING_API_DOMAIN_BOND_AMORTIZATION_DATA_HPP

#include "ores.utility/decimal/decimal.hpp"
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>

namespace ores::trading::domain {

/**
 * @brief One row of an amortization block.
 *
 */
struct bond_amortization_data {
    /**
     * @brief The kind of amortization step the row states, such as Constant or Floating.
     */
    std::string type;

    /**
     * @brief The amount the step moves the notional by, when the document states one.
     */
    std::optional<ores::utility::decimal::decimal> value;

    /**
     * @brief The date the step starts on, when the document states one.
     */
    std::optional<std::string> start_date;

    /**
     * @brief The date the step ends on, when the document states one.
     */
    std::optional<std::string> end_date;

    /**
     * @brief How often the step repeats over its window, in ORE's tenor spelling.
     */
    std::optional<std::string> frequency;

    /**
     * @brief True when the document allows the notional to fall below zero.
     */
    std::optional<bool> underflow;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_amortization_data&, const bond_amortization_data&) = default;
};

/**
 * @brief The members of bond_amortization_data the customer agreed to.
 */
inline constexpr std::string_view bond_amortization_data_economic_fields[] = {
    "type",
    "value",
    "start_date",
    "end_date",
    "frequency",
    "underflow",
};

/**
 * @brief Names the members of bond_amortization_data that are economic.
 *
 * The economic digest reads this at compile time and decides nothing by a
 * field's name. A type without this overload cannot be digested.
 */
[[nodiscard]] constexpr std::span<const std::string_view>
economic_fields_of(std::type_identity<bond_amortization_data>) {
    return bond_amortization_data_economic_fields;
}

}

#endif
