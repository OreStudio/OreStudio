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
#ifndef ORES_REFDATA_API_DOMAIN_TENOR_BASIS_SWAP_PAY_RECEIVE_COUPON_HPP
#define ORES_REFDATA_API_DOMAIN_TENOR_BASIS_SWAP_PAY_RECEIVE_COUPON_HPP

#include <optional>
#include <string>

namespace ores::refdata::domain {

/**
 * @brief The coupon treatment of the paying and receiving form of a tenor basis swap.
 *
 * How the coupons are built for this form: which leg carries the quoted
 * spread, whether the spread is included, and which sub-period convention
 * applies. ORE writes these three beside the legs of both forms, so each form
 * declares them rather than the shared central row.
 *
 * Extracted as a plain nested sub-struct so that
 * rfl::internal::no_duplicate_field_names never instantiates a Literal
 * wider than 5 fields for this block. See the decomposition section of
 * doc/knowledge/architecture/data_oriented_design.org.
 */
struct tenor_basis_swap_pay_receive_coupon {
    /**
     * @brief Whether the quoted spread is added to the receiving leg rather than the paying one.
     */
    std::optional<bool> spread_on_rec;

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
    friend bool operator==(const tenor_basis_swap_pay_receive_coupon&,
                           const tenor_basis_swap_pay_receive_coupon&) = default;
};

}

#endif
