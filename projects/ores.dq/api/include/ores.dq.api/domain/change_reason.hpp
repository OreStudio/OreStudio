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
#ifndef ORES_DQ_API_DOMAIN_CHANGE_REASON_HPP
#define ORES_DQ_API_DOMAIN_CHANGE_REASON_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string>
#include <string_view>

namespace ores::dq::domain {

/**
 * @brief A specific, selectable reason for making a change.
 *
 * A change reason is a specific, selectable reason for making a change
 * to a record, scoped to a change reason category. Examples: "Typo
 * Fix" (category: data_correction), "New Regulation" (category:
 * regulatory_update). Rows are authored directly (not mirrored from an
 * external source).
 */
struct change_reason final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique code identifying this change reason.
     *
     * Examples: "typo_fix", "new_regulation".
     */
    std::string code;

    /**
     * @brief Human-readable description of this change reason.
     */
    std::string description;

    /**
     * @brief Code of the change reason category this reason belongs to. References
     * ores_dq_change_reason_categories_tbl (soft FK).
     */
    std::string category_code;

    /**
     * @brief Whether this reason may be selected when creating a new record.
     */
    bool applies_to_new = false;

    /**
     * @brief Whether this reason may be selected when amending an existing record.
     */
    bool applies_to_amend = false;

    /**
     * @brief Whether this reason may be selected when deleting a record.
     */
    bool applies_to_delete = false;

    /**
     * @brief Whether selecting this reason requires free-text commentary.
     */
    bool requires_commentary = false;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this change reason.
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
 * @brief Dispatch-key identifier for change_reason, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const change_reason&) {
    return "ores.dq.change_reason";
}

}

#endif
