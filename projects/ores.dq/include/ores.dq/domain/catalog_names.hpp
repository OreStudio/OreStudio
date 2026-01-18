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
#ifndef ORES_DQ_DOMAIN_CATALOG_NAMES_HPP
#define ORES_DQ_DOMAIN_CATALOG_NAMES_HPP

#include <string_view>

namespace ores::dq::domain::catalog_names {

/**
 * @brief Well-known catalog names.
 *
 * These constants provide type-safe references to well-known system catalogs.
 * Using these constants instead of string literals helps prevent typos and
 * makes it easier to find all references to a specific catalog.
 *
 * Note: These are convenience constants only. Users can create their own
 * catalogs with any name and declare dependencies using arbitrary strings.
 */

/**
 * @brief ISO reference data catalog.
 *
 * Contains ISO standard reference data including countries (ISO 3166),
 * currencies (ISO 4217), and other international standards.
 */
inline constexpr std::string_view iso_reference_data = "ISO Reference Data";

/**
 * @brief Core DQ dimensions catalog.
 *
 * Contains the fundamental DQ dimension values: origin, nature, and
 * treatment dimensions that classify datasets.
 */
inline constexpr std::string_view core_dq_dimensions = "Core DQ Dimensions";

/**
 * @brief Market data catalog.
 *
 * Contains market pricing, quotes, and trading data.
 */
inline constexpr std::string_view market_data = "Market Data";

/**
 * @brief Trade data catalog.
 *
 * Contains transaction and trade execution data.
 */
inline constexpr std::string_view trade_data = "Trade Data";

/**
 * @brief Risk analytics catalog.
 *
 * Contains risk metrics, exposures, and analytics data.
 */
inline constexpr std::string_view risk_analytics = "Risk Analytics";

/**
 * @brief Regulatory data catalog.
 *
 * Contains regulatory compliance and reporting data.
 */
inline constexpr std::string_view regulatory_data = "Regulatory Data";

}

#endif
