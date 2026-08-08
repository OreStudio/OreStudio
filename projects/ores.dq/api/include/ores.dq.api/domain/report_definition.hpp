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
#ifndef ORES_DQ_API_DOMAIN_REPORT_DEFINITION_HPP
#define ORES_DQ_API_DOMAIN_REPORT_DEFINITION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::dq::domain {

/**
 * @brief Report Definition artefacts - default report templates for the ORE analytics bundle
 *
 * Report Definition artefacts - default report templates for the ORE
 * analytics bundle
 */
struct report_definition final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate key for the report definition record.
     */
    boost::uuids::uuid id;

    /**
     * @brief Report Definition artefacts - default report templates for the ORE
analytics bundle
     */
    std::string name;

    /**
     * @brief Report Definition artefacts - default report templates for the ORE
analytics bundle
     */
    std::optional<std::string> description;

    /**
     * @brief Report Definition artefacts - default report templates for the ORE
analytics bundle
     */
    std::string report_type;

    /**
     * @brief Report Definition artefacts - default report templates for the ORE
analytics bundle
     */
    std::string schedule_expression;

    /**
     * @brief Report Definition artefacts - default report templates for the ORE
analytics bundle
     */
    std::string concurrency_policy;

    /**
     * @brief Report Definition artefacts - default report templates for the ORE
analytics bundle
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this report definition.
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
 * @brief Dispatch-key identifier for report_definition, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const report_definition&) {
    return "ores.dq.report_definition";
}

}

#endif
