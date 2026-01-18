/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_DQ_DOMAIN_METHODOLOGY_NAMES_HPP
#define ORES_DQ_DOMAIN_METHODOLOGY_NAMES_HPP

#include <string_view>

namespace ores::dq::domain::methodology_names {

/**
 * @brief Well-known methodology names.
 *
 * These constants provide type-safe references to well-known system
 * methodologies. Using these constants instead of string literals helps
 * prevent typos and makes it easier to find all references.
 */

/**
 * @brief Wikipedia ISO 3166 country data extraction methodology.
 */
inline constexpr std::string_view wikipedia_iso_3166 =
    "Wikipedia ISO 3166 Extraction";

/**
 * @brief GitHub flag icons download methodology.
 */
inline constexpr std::string_view github_flag_icons =
    "GitHub Flag Icons Download";

/**
 * @brief GitHub cryptocurrency icons download methodology.
 */
inline constexpr std::string_view github_crypto_icons =
    "GitHub Cryptocurrency Icons Download";

/**
 * @brief Wikipedia ISO 4217 currency data extraction methodology.
 */
inline constexpr std::string_view wikipedia_iso_4217 =
    "Wikipedia ISO 4217 Extraction";

/**
 * @brief GitHub cryptocurrencies JSON download methodology.
 */
inline constexpr std::string_view github_cryptocurrencies =
    "GitHub Cryptocurrencies JSON Download";

/**
 * @brief FpML Genericode download methodology.
 */
inline constexpr std::string_view fpml_genericode =
    "FpML Genericode Download";

/**
 * @brief Synthetic data generation methodology.
 *
 * Used for all programmatically generated test data via ores.synthetic.
 */
inline constexpr std::string_view synthetic_data_generation =
    "Synthetic Data Generation";

}

#endif
