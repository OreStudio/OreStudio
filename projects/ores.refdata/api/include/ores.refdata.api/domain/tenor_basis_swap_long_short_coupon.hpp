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
#ifndef ORES_REFDATA_API_DOMAIN_TENOR_BASIS_SWAP_LONG_SHORT_COUPON_HPP
#define ORES_REFDATA_API_DOMAIN_TENOR_BASIS_SWAP_LONG_SHORT_COUPON_HPP

#include "ores.utility/codec/wire_fields.hpp"
#include <optional>
#include <string>
#include <tuple>

namespace ores::refdata::domain {

/**
 * @brief The coupon treatment of the long and short form of a tenor basis swap.
 *
 * Whether the spread is included and which sub-period convention applies. ORE
 * writes these beside the legs of both forms, so each form declares them rather
 * than the shared central row.
 *
 * Extracted as a plain nested sub-struct so that
 * rfl::internal::no_duplicate_field_names never instantiates a Literal
 * wider than 5 fields for this block. See the decomposition section of
 * doc/knowledge/architecture/data_oriented_design.org.
 */
struct tenor_basis_swap_long_short_coupon {
    /**
     * @brief Whether the spread is included in the coupon at all.
     */
    std::optional<bool> include_spread;

    /**
     * @brief The sub-periods coupon convention ORE applies, as its own code.
     */
    std::optional<std::string> sub_periods_coupon_type;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const tenor_basis_swap_long_short_coupon&,
                           const tenor_basis_swap_long_short_coupon&) = default;
};
/**
 * @brief The members of tenor_basis_swap_long_short_coupon, named as the wire names
 * them and in the order the type declares them.
 *
 * A group is a member of the entity that holds it, so the carriers reach this
 * shape through it: the parent's field names the member, and this one names the
 * members inside it.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_basis_swap_long_short_coupon&) {
    return std::tuple{
        utility::codec::field{"include_spread",
                              &tenor_basis_swap_long_short_coupon::include_spread},
        utility::codec::field{"sub_periods_coupon_type",
                              &tenor_basis_swap_long_short_coupon::sub_periods_coupon_type},
    };
}

}

#endif
