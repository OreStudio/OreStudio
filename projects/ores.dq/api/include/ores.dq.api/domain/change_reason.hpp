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
#ifndef ORES_DQ_API_DOMAIN_CHANGE_REASON_HPP
#define ORES_DQ_API_DOMAIN_CHANGE_REASON_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::dq::domain {

/**
 * @brief A specific, selectable reason for making a change.
 *
 * A change reason is a specific, selectable reason for making a change
 * to a record, scoped to a change reason category. The categories the
 * seed creates are system, common and trade, each aligned to a
 * regulatory standard; a reason names one of them. Rows are authored
 * directly (not mirrored from an external source).
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
     *
     * The value is one of the categories the DQ seed creates, so a generated instance satisfies the
     * trigger that validates the reference.
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
     *
     * The transaction-time window's start, which the store sets from its own
     * clock. It travels with the audit members because it is only ever read
     * with them: the history builder takes a version type that carries an
     * actor *and* this timestamp, so an entity without the actor has no use
     * for the timestamp either.
     */
    std::chrono::system_clock::time_point recorded_at;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const change_reason&, const change_reason&) = default;
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
