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
#ifndef ORES_INBOX_DOMAIN_APPROVAL_KIND_PART_HPP
#define ORES_INBOX_DOMAIN_APPROVAL_KIND_PART_HPP

#include <chrono>
#include <string>
#include <string_view>

namespace ores::inbox::domain {

/**
 * @brief A part that every request of a kind needs.
 *
 * A kind of request names the parts its requests always need, and a part may be
 * named by many kinds. The parts are the state of the thing the request would
 * change, so a kind that governs several independent sections names one part for
 * each, and a request of that kind takes the parts of the sections it touches.
 *
 * Before this table held them, the parts of a request were the parts its policy
 * named, and a kind whose parts are not a policy of columns had nowhere to state
 * them. The engine run settings are the first such kind: its fixings and building
 * groups change the numbers a run computes, and no column policy names them.
 *
 * A kind that names no parts here is decided by the decider permission and the
 * approval count its own row carries, as the role request is.
 */
struct approval_kind_part final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief Code of the kind of approval request.
     *
     * References ores_inbox_approval_kinds_tbl.code.
     */
    std::string kind_code;

    /**
     * @brief Code of the part whose approval every request of the kind needs.
     *
     * References ores_inbox_approval_parts_tbl.code.
     */
    std::string part_code;

    /**
     * @brief Username of the person who last modified this approval kind part.
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
    friend bool operator==(const approval_kind_part&, const approval_kind_part&) = default;
};

/**
 * @brief Dispatch-key identifier for approval_kind_part, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const approval_kind_part&) {
    return "ores.inbox.approval_kind_part";
}

}

#endif
