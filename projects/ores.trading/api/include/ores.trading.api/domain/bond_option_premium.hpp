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
#ifndef ORES_TRADING_API_DOMAIN_BOND_OPTION_PREMIUM_HPP
#define ORES_TRADING_API_DOMAIN_BOND_OPTION_PREMIUM_HPP

#include "ores.trading.api/domain/bond_option_settlement.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <optional>
#include <string>
#include <tuple>

namespace ores::trading::domain {

/**
 * @brief One entry of an option's premium list.
 *
 * The amount, the currency and the pay date are required, and the settlement
 * block beside them is optional.
 */
struct bond_option_premium {
    /**
     * @brief The premium amount.
     */
    ores::utility::decimal::decimal amount;

    /**
     * @brief The currency the premium pays in.
     */
    std::string currency;

    /**
     * @brief The date the premium pays on.
     */
    std::string pay_date;

    /**
     * @brief The settlement terms the premium pays under, when the document states them.
     */
    std::optional<bond_option_settlement> settlement;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_option_premium&, const bond_option_premium&) = default;
};
/**
 * @brief The members of bond_option_premium, named as the wire names
 * them and in the order the type declares them.
 *
 * A group is a member of the entity that holds it, so the carriers reach this
 * shape through it: the parent's field names the member, and this one names the
 * members inside it.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_option_premium&) {
    return std::tuple{
        utility::codec::field{"amount", &bond_option_premium::amount},
        utility::codec::field{"currency", &bond_option_premium::currency},
        utility::codec::field{"pay_date", &bond_option_premium::pay_date},
        utility::codec::field{"settlement", &bond_option_premium::settlement},
    };
}

}

#endif
