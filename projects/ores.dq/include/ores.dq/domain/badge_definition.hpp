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
#ifndef ORES_DQ_DOMAIN_BADGE_DEFINITION_HPP
#define ORES_DQ_DOMAIN_BADGE_DEFINITION_HPP

#include <chrono>
#include <optional>
#include <string>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::dq::domain {

/**
 * @brief Visual definition for a badge, including colours, label, and severity.
 *
 * The badge catalogue. Each entry defines the complete visual presentation
 * of a badge: display label, tooltip, background colour, text colour,
 * severity level, and an optional Bootstrap CSS class hint for Wt.
 * 
 * Badge definitions are the single source of truth for all badge visual
 * metadata across Qt and Wt. They are loaded at client startup as
 * reference data and looked up at render time via BadgeCache.
 */
struct badge_definition final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique badge definition code.
     *
     * Examples: 'active', 'locked', 'fsm_draft', 'login_online'.
     */
    std::string code;

    /**
     * @brief Display label shown inside the badge.
     */
    std::string name;

    /**
     * @brief Tooltip text explaining what this badge represents.
     */
    std::string description;

    /**
     * @brief Badge background colour as a hex string.
     *
     * Example: '#22c55e'.
     */
    std::string background_colour;

    /**
     * @brief Badge text colour as a hex string.
     *
     * Default is '#ffffff' (white). May vary for light-background badges.
     */
    std::string text_colour;

    /**
     * @brief Severity classification for this badge.
     *
     * Soft FK to ores_dq_badge_severities_tbl. Bootstrap-aligned codes: secondary, info, success, warning, danger, primary.
     */
    std::string severity_code;

    /**
     * @brief Optional Bootstrap CSS class hint for Wt rendering.
     *
     * Example: 'badge bg-success'. Qt ignores this field. Nullable — Wt falls back to inline style from background_colour/text_colour when absent.
     */
    std::string css_class;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this badge definition.
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

}

#endif
