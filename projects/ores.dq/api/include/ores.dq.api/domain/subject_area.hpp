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
#ifndef ORES_DQ_API_DOMAIN_SUBJECT_AREA_HPP
#define ORES_DQ_API_DOMAIN_SUBJECT_AREA_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string>
#include <string_view>

namespace ores::dq::domain {

/**
 * @brief Sub-classification within a data domain.
 *
 * Sub-classification within a data domain. Examples: currencies,
 * countries, images. Rows are authored directly (not mirrored from an
 * external source).
 */
struct subject_area final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique name identifying this subject area within its data domain.
     *
     * Examples: "currencies", "countries", "images".
     */
    std::string name;

    /**
     * @brief Name of the data domain this subject area belongs to. References
     * ores_dq_data_domains_tbl (soft FK). Together with name this forms a true compound physical
     * primary key, enforced by ~sql_schema_domain_entity_create.mustache~'s composite primary
     * key/GIST exclusion constraint support ([[id:6C5FADD1-478A-46ED-A266-35A8182B814E][6C5FADD1]])
     * -- replacing the previous plain Column + bolt-on unique-index workaround.
     */
    std::string domain_name;

    /**
     * @brief Human-readable description of this subject area.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this subject area.
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
 * @brief Dispatch-key identifier for subject_area, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const subject_area&) {
    return "ores.dq.subject_area";
}

}

#endif
