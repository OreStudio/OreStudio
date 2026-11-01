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
#ifndef ORES_TRADING_API_DOMAIN_BOND_FORWARD_PREMIUM_HPP
#define ORES_TRADING_API_DOMAIN_BOND_FORWARD_PREMIUM_HPP

#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <string>
#include <tuple>

namespace ores::trading::domain {

/**
 * @brief A forward bond's premium: the amount and the date it pays.
 *
 * Both members are required. The amount is money, so it is an exact decimal; the
 * schema spells it as text and the mapper parses those digits. The date is ISO
 * 8601 text.
 */
struct bond_forward_premium {
    /**
     * @brief The premium amount.
     */
    ores::utility::decimal::decimal amount;

    /**
     * @brief The date the premium pays on, as ISO 8601 text.
     */
    std::string date;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_forward_premium&, const bond_forward_premium&) = default;
};
/**
 * @brief The members of bond_forward_premium, named as the wire names
 * them and in the order the type declares them.
 *
 * A group is a member of the entity that holds it, so the carriers reach this
 * shape through it: the parent's field names the member, and this one names the
 * members inside it.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_forward_premium&) {
    return std::tuple{
        utility::codec::field{"amount", &bond_forward_premium::amount},
        utility::codec::field{"date", &bond_forward_premium::date},
    };
}

}

#endif
