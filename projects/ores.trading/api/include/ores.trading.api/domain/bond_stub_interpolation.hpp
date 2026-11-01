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
#ifndef ORES_TRADING_API_DOMAIN_BOND_STUB_INTERPOLATION_HPP
#define ORES_TRADING_API_DOMAIN_BOND_STUB_INTERPOLATION_HPP

#include "ores.utility/codec/wire_fields.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <tuple>

namespace ores::trading::domain {

/**
 * @brief The two indices a stub interpolates between, and the rounding.
 *
 */
struct bond_stub_interpolation {
    /**
     * @brief The index at the short end of the interpolation.
     */
    std::string short_index;

    /**
     * @brief The index at the long end of the interpolation.
     */
    std::string long_index;

    /**
     * @brief The rounding the interpolated rate takes, when the document states one.
     */
    std::optional<std::string> rounding_type;

    /**
     * @brief The number of decimal places the rounding keeps, when the document states one.
     */
    std::optional<std::int64_t> rounding_precision;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_stub_interpolation&,
                           const bond_stub_interpolation&) = default;
};
/**
 * @brief The members of bond_stub_interpolation, named as the wire names
 * them and in the order the type declares them.
 *
 * A group is a member of the entity that holds it, so the carriers reach this
 * shape through it: the parent's field names the member, and this one names the
 * members inside it.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_stub_interpolation&) {
    return std::tuple{
        utility::codec::field{"short_index", &bond_stub_interpolation::short_index},
        utility::codec::field{"long_index", &bond_stub_interpolation::long_index},
        utility::codec::field{"rounding_type", &bond_stub_interpolation::rounding_type},
        utility::codec::field{"rounding_precision", &bond_stub_interpolation::rounding_precision},
    };
}

}

#endif
