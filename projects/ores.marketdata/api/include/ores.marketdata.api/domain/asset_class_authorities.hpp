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
 * Template: cpp_asset_class_authorities.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_MARKETDATA_API_DOMAIN_ASSET_CLASS_AUTHORITIES_HPP
#define ORES_MARKETDATA_API_DOMAIN_ASSET_CLASS_AUTHORITIES_HPP

#include <optional>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief The taxonomy's codes, in display order, as the catalogue declares
 * them.
 *
 * A test-data generator that must emit a code the catalogue holds reads this
 * rather than restating the list, so the taxonomy has one author.
 */
inline constexpr std::string_view asset_class_codes[] = {
    "fx",
    "interest_rates",
    "credit",
    "equity",
    "commodity",
    "inflation",
    "bond",
};

/**
 * @brief The refdata asset class an oresmd authority names, or nullopt where
 * it names none.
 *
 * Generated from ores.refdata.asset_class_catalogue, which declares the oresmd
 * namespace and the mapping. An authority the catalogue maps to no class, such
 * as a pairwise correlation or the generic wrapper, answers nullopt.
 */
[[nodiscard]] inline std::optional<std::string_view>
asset_class_for_authority(std::string_view authority) {
    if (authority == "ir")
        return std::string_view{"interest_rates"};
    if (authority == "credit")
        return std::string_view{"credit"};
    if (authority == "equity")
        return std::string_view{"equity"};
    if (authority == "commodity")
        return std::string_view{"commodity"};
    if (authority == "fx")
        return std::string_view{"fx"};
    if (authority == "inflation")
        return std::string_view{"inflation"};
    if (authority == "security")
        return std::string_view{"bond"};
    if (authority == "shape_profile")
        return std::string_view{"commodity"};
    if (authority == "rating")
        return std::string_view{"credit"};
    if (authority == "power")
        return std::string_view{"commodity"};
    return std::nullopt;
}

}

#endif
