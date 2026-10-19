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
#ifndef ORES_ORE_CORE_DOMAIN_CONVENTIONS_FAMILY_PACK_HPP
#define ORES_ORE_CORE_DOMAIN_CONVENTIONS_FAMILY_PACK_HPP

#include "conventions_family_units.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <format>
#include <rfl/json.hpp>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

/**
 * @file conventions_family_pack.hpp
 * @brief Packs a family's rows into the bundle and unpacks them again.
 *
 * These are templates over the family's value type, so reflecting a family
 * happens in the unit that names the type and nowhere else.
 */
namespace ores::ore::domain::conventions_family {

/**
 * @brief One family's elements, mapped and packed in document order.
 */
template <typename Convert>
void pack_family(refdata::service::conventions_bundle& bundle,
                 std::string_view family,
                 const std::vector<xml::record_reader>& rows,
                 Convert convert) {
    for (std::size_t i = 0; i < rows.size(); ++i)
        bundle.rows.push_back(
            {std::string(family), static_cast<int>(i), rfl::json::write(convert(rows[i]))});
}

template <typename Row>
void pack_row(refdata::service::conventions_bundle& bundle,
              std::string_view family,
              int sequence,
              const Row& row) {
    bundle.rows.push_back({std::string(family), sequence, rfl::json::write(row)});
}

/**
 * @brief One payload as the family's own type.
 *
 * A payload that does not decode is a family that cannot be written, so it
 * throws rather than disappearing from the document.
 */
template <typename Row>
Row unpack_row(const package_row& row) {
    auto decoded = rfl::json::read<Row>(row.payload);
    if (!decoded)
        throw std::runtime_error(
            std::format("Conventions payload for family \"{}\" does not decode.", row.family));
    return std::move(*decoded);
}

}

#endif
