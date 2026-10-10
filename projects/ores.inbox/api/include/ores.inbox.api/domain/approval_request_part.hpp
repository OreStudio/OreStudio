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
#ifndef ORES_INBOX_DOMAIN_APPROVAL_REQUEST_PART_HPP
#define ORES_INBOX_DOMAIN_APPROVAL_REQUEST_PART_HPP

#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::inbox::domain {

/**
 * @brief A part whose approval an approval request needs.
 *
 * A request needs the approval of one or more parts, and a part may be needed by
 * many requests. Each row names one part a request needs, and is written when the
 * request is raised. A decision carries the part it answers, so a request is
 * approved when every part named here has an approval, in the order the parts give.
 *
 * A request with no rows here is a request of a kind that names one decider
 * permission and a count, as the role request is, and it is decided as before.
 */
struct approval_request_part final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief ID of the approval request.
     *
     * References ores_inbox_approval_requests_tbl.id.
     */
    boost::uuids::uuid request_id;

    /**
     * @brief Code of the part whose approval the request needs.
     *
     * References ores_inbox_approval_parts_tbl.code.
     */
    std::string part_code;

    /**
     * @brief Username of the person who last modified this approval request part.
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
    friend bool operator==(const approval_request_part&, const approval_request_part&) = default;
};

/**
 * @brief Dispatch-key identifier for approval_request_part, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const approval_request_part&) {
    return "ores.inbox.approval_request_part";
}

}

#endif
