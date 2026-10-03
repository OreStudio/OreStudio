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
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REPORTING_DOMAIN_REPORT_TYPE_CONFIGURATION_TYPE_HPP
#define ORES_REPORTING_DOMAIN_REPORT_TYPE_CONFIGURATION_TYPE_HPP

#include <chrono>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief A configuration type a run of a report type requires.
 *
 * Many-to-many junction between report types and configuration types: one row
 * for each configuration type a run of the report type cannot start without. A
 * risk report needs pricing engines, today's market, curve configuration and
 * conventions whatever its analytics; a configuration type is needed by more than
 * one report type. Starting a report checks that its definition binds one
 * configuration of every type its report type requires.
 */
struct report_type_configuration_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief The report type that has the requirement.
     */
    std::string report_type_code;

    /**
     * @brief The configuration type a run of the report type requires.
     */
    std::string configuration_type_code;

    /**
     * @brief Username of the person who last modified this report type configuration type.
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

    /**
     * @brief Value equality, on the same terms as an entity's.
     */
    friend bool operator==(const report_type_configuration_type&,
                           const report_type_configuration_type&) = default;
};

/**
 * @brief Dispatch-key identifier for report_type_configuration_type, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const report_type_configuration_type&) {
    return "ores.reporting.report_type_configuration_type";
}

}

#endif
