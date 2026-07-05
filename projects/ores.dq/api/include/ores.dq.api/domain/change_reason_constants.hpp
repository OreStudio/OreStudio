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

#ifndef ORES_DQ_API_DOMAIN_CHANGE_REASON_CONSTANTS_HPP
#define ORES_DQ_API_DOMAIN_CHANGE_REASON_CONSTANTS_HPP

#include <string_view>

namespace ores::dq::domain::change_reason_constants {

/**
 * @brief Change reason codes used throughout the system.
 *
 * These codes must match entries in the change_reasons database table.
 * They live in ores.dq.api — change reasons are a data quality domain
 * concept — and are consumed across components (IAM, refdata, etc.).
 */
namespace codes {

/**
 * @brief Used when creating new records in the system.
 *
 * Applied automatically by service layer when entities are first created.
 */
constexpr std::string_view new_record = "system.new_record";

/**
 * @brief Used for data loaded from external sources (e.g. ORE XML import).
 *
 * Requires data lineage commentary for auditability.
 */
constexpr std::string_view external_data_import = "system.external_data_import";

/**
 * @brief Used for updates that don't change material business data.
 *
 * Examples: password changes, email updates, touch operations (version bump
 * without field changes).
 */
constexpr std::string_view non_material_update = "common.non_material_update";

/**
 * @brief Used by synthetic-data generators for freshly created test fixtures.
 */
constexpr std::string_view synthetic_new = "system.new";

/**
 * @brief Used by synthetic-data generators for general test-only fixtures.
 */
constexpr std::string_view synthetic_test_data = "system.test";

} // namespace codes

/**
 * @brief Change reason category codes.
 *
 * Categories group related change reasons together.
 */
namespace categories {

/**
 * @brief Common reasons applicable to most entity types.
 *
 * Used as the default category when fetching reasons for amendments.
 */
constexpr std::string_view common = "common";

/**
 * @brief System-generated reasons not typically shown to users.
 */
constexpr std::string_view system = "system";

} // namespace categories

} // namespace ores::dq::domain::change_reason_constants

#endif
