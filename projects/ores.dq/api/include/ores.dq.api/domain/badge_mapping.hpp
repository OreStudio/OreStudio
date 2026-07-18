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
#ifndef ORES_DQ_DOMAIN_BADGE_MAPPING_HPP
#define ORES_DQ_DOMAIN_BADGE_MAPPING_HPP

#include <chrono>
#include <string>
#include <string_view>

namespace ores::dq::domain {

/**
 * @brief Links a (code domain, entity code) pair to a badge definition.
 *
 * Universal mapping table that associates any (code_domain, entity_code)
 * pair with a badge definition. This is the single join point between
 * domain values and their visual presentation.
 *
 * Examples:
 * - ('party_status', 'ACTIVE')  -> 'active'
 * - ('party_status', 'FROZEN')  -> 'frozen'
 * - ('fsm_state',    'DRAFT')   -> 'fsm_draft'
 * - ('login_status', 'Online')  -> 'login_online'
 * - ('dq_nature',    'Actual')  -> 'dq_actual'
 *
 * Populated via seed scripts; no management UI needed.
 */
struct badge_mapping final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief Code of the domain this mapping belongs to.
     *
     * References code_domain.code (soft FK). Examples: 'party_status', 'fsm_state'.
     */
    std::string code_domain_code;

    /**
     * @brief The domain value being mapped to a badge.
     *
     * Examples: 'ACTIVE', 'DRAFT', 'Online', 'Actual'.
     */
    std::string entity_code;

    /**
     * @brief The badge definition to use for this mapping.
     *
     * References badge_definition.code (soft FK). Examples: 'active', 'fsm_draft'.
     */
    std::string badge_code;

    /**
     * @brief Username of the person who last modified this badge mapping.
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
 * @brief Dispatch-key identifier for badge_mapping, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const badge_mapping&) {
    return "ores.dq.badge_mapping";
}

}

#endif
