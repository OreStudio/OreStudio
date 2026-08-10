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
#ifndef ORES_REFDATA_API_DOMAIN_DIARY_ENTRY_TYPE_HPP
#define ORES_REFDATA_API_DOMAIN_DIARY_ENTRY_TYPE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Classification of a calendar_event entry's kind.
 *
 * Reference data classifying what a [[id:B20050A5-1245-4944-A328-2A0893C92AEC][calendar_event]]
 * entry represents. Values include: holiday, central_bank_meeting, data_release, other. Lets a
 * single calendar_events table hold heterogeneous diary entries -- a central bank's meeting
 * schedule, a statistical office's release calendar, an open-ended memo entry -- without collapsing
 * their distinct semantics.
 *
 * The vocabulary is deliberately open-ended: consumers (e.g. the tenor
 * resolution engine's event-lookup schedules) key off specific codes,
 * but new codes can be added without schema change.
 */
struct diary_entry_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique diary entry type code.
     *
     * Examples: 'holiday', 'central_bank_meeting', 'data_release', 'other'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the diary entry type.
     */
    std::string name;

    /**
     * @brief Detailed description of the diary entry type.
     */
    std::string description;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this diary entry type.
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
 * @brief Dispatch-key identifier for diary_entry_type, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const diary_entry_type&) {
    return "ores.refdata.diary_entry_type";
}

}

#endif
