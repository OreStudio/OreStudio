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
#ifndef ORES_TRADING_API_DOMAIN_BOND_FIXED_LEG_DATA_HPP
#define ORES_TRADING_API_DOMAIN_BOND_FIXED_LEG_DATA_HPP

#include "ores.trading.api/domain/bond_float_data.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/digest/canonical_form.hpp"
#include <string>
#include <tuple>
#include <vector>

namespace ores::trading::domain {

/**
 * @brief A fixed leg's rate terms.
 *
 */
struct bond_fixed_leg_data {
    /**
     * @brief The fixed rates the document states, one bond_float_data row each.
     */
    std::vector<bond_float_data> rates;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_fixed_leg_data&, const bond_fixed_leg_data&) = default;
};
/**
 * @brief The members of bond_fixed_leg_data, named as the wire names
 * them and in the order the type declares them.
 *
 * A group is a member of the entity that holds it, so the carriers reach this
 * shape through it: the parent's field names the member, and this one names the
 * members inside it.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<bond_fixed_leg_data>) {
    return std::tuple{
        utility::codec::field{"rates", &bond_fixed_leg_data::rates},
    };
}

/**
 * @brief The canonical text of the members of bond_fixed_leg_data the customer agreed to.
 *
 * Renders, in member order, the fields the model declares economic. The
 * economic digest hashes this text, so a field the customer did not agree to
 * never moves it.
 */
[[nodiscard]] inline std::string canonical_form(const bond_fixed_leg_data& v) {
    std::string out = "{";
    ores::utility::digest::append_member(out, "rates", v.rates);
    out.push_back('}');
    return out;
}

}

#endif
