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
#ifndef ORES_REFDATA_API_DOMAIN_DAY_COUNT_FRACTION_TYPE_HPP
#define ORES_REFDATA_API_DOMAIN_DAY_COUNT_FRACTION_TYPE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Day count fraction convention code (e.g. Actual/360, Actual/365, 30/360).
 *
 * Reference data table defining valid day count fraction conventions used to
 * compute accrual fractions between two dates (e.g. for coupon accrual on
 * instrument legs and the *_convention entities). Values are sourced from
 * ORE's ore_types.xsd. Follows the same aux-type pattern (code / name /
 * description / display_order) as
 * [[id:B94A2D2A-7653-4165-AD13-B13008BE3B5A][business_day_convention_type]].
 */
struct day_count_fraction_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique day count fraction type code.
     *
     * Examples: 'A360', 'A365F', 'Thirty360', 'ActActISDA'.
     */
    std::string code;

    /**
     * @brief Human-readable name of the day count fraction (e.g. "Actual/360").
     */
    std::string name;

    /**
     * @brief Human-readable description of the day count fraction.
     */
    std::string description;

    /**
     * @brief Ordinal position for dropdown/list display.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this day count fraction type.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

/**
 * @brief Dispatch-key identifier for day_count_fraction_type, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const day_count_fraction_type&) {
    return "ores.refdata.day_count_fraction_type";
}

}

#endif
