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
#ifndef ORES_REFDATA_API_DOMAIN_TENOR_BASIS_SWAP_LONG_SHORT_LEGS_HPP
#define ORES_REFDATA_API_DOMAIN_TENOR_BASIS_SWAP_LONG_SHORT_LEGS_HPP

#include <optional>
#include <string>

namespace ores::refdata::domain {

/**
 * @brief The two legs of the long and short form of a tenor basis swap.
 *
 * The long leg and the short leg, each naming an index and an optional payment
 * tenor, and the spread flag the short leg carries. A row fills all five or
 * none: the convention carries this form or the paying and receiving form.
 *
 * Extracted as a plain nested sub-struct so that
 * rfl::internal::no_duplicate_field_names never instantiates a Literal
 * wider than 5 fields for this block. See the decomposition section of
 * doc/knowledge/architecture/data_oriented_design.org.
 */
struct tenor_basis_swap_long_short_legs {
    /**
     * @brief Index the long leg references, in the long and short form.
     */
    std::optional<std::string> long_index;

    /**
     * @brief Payment tenor of the long leg, as ORE's period code.
     */
    std::optional<std::string> long_pay_tenor;

    /**
     * @brief Index the short leg references, in the long and short form.
     */
    std::optional<std::string> short_index;

    /**
     * @brief Payment tenor of the short leg, as ORE's period code.
     */
    std::optional<std::string> short_pay_tenor;

    /**
     * @brief Whether the quoted spread is added to the short leg rather than the long one.
     */
    std::optional<bool> spread_on_short;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const tenor_basis_swap_long_short_legs&,
                           const tenor_basis_swap_long_short_legs&) = default;
};

}

#endif
