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
#ifndef ORES_QT_BADGE_COLORS_HPP
#define ORES_QT_BADGE_COLORS_HPP

#include <QString>
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/EntityItemDelegate.hpp"

namespace ores::qt {

/**
 * @brief Standard badge colour resolver for entity status fields.
 *
 * Maps common status values (Active, Inactive, Closed, Frozen, Pending)
 * to consistent colours across all entity list views.
 */
inline badge_color_pair resolve_status_badge_color(const QString& value) {
    const auto upper = value.toUpper();
    if (upper == "ACTIVE")
        return {QColor(0x22, 0xC5, 0x5E), Qt::white};   // Green
    if (upper == "INACTIVE" || upper == "CLOSED")
        return {QColor(0x6B, 0x72, 0x80), Qt::white};   // Gray
    if (upper == "FROZEN")
        return {QColor(0xEA, 0xB3, 0x08), Qt::white};   // Amber
    if (upper == "PENDING")
        return {QColor(0x3B, 0x82, 0xF6), Qt::white};   // Blue
    return {QColor(0x6B, 0x72, 0x80), Qt::white};        // Default gray
}

/**
 * @brief Badge colour resolver for portfolio list views.
 *
 * Handles Virtual/Physical type badges (purple/blue) in addition to
 * the standard status values.
 */
inline badge_color_pair resolve_portfolio_badge_color(const QString& value) {
    const auto upper = value.toUpper();
    if (upper == "VIRTUAL")
        return {badge_colors::type_virtual, badge_colors::text};
    if (upper == "PHYSICAL")
        return {badge_colors::type_physical, badge_colors::text};
    return resolve_status_badge_color(value);
}

/**
 * @brief Badge colour resolver for book list views.
 *
 * Handles Trading/Banking type badges (purple/blue) in addition to
 * the standard status values.
 */
inline badge_color_pair resolve_book_badge_color(const QString& value) {
    const auto upper = value.toUpper();
    if (upper == "TRADING")
        return {badge_colors::type_virtual, badge_colors::text};
    if (upper == "BANKING")
        return {badge_colors::type_physical, badge_colors::text};
    return resolve_status_badge_color(value);
}

/**
 * @brief Badge colour resolver for compute task State and Outcome columns.
 */
inline badge_color_pair resolve_compute_task_badge_color(const QString& value) {
    const auto upper = value.toUpper();
    // Outcome values
    if (upper == "SUCCESS")
        return {badge_colors::online,    badge_colors::text};  // Green
    if (upper == "FAILED")
        return {badge_colors::locked,    badge_colors::text};  // Red
    if (upper == "NO REPLY")
        return {badge_colors::old,       badge_colors::text};  // Amber
    if (upper == "PENDING")
        return {badge_colors::recent,    badge_colors::text};  // Blue
    // State values
    if (upper == "RUNNING")
        return {badge_colors::treatment_cleaned, badge_colors::text};  // Sky blue
    if (upper == "UNSENT")
        return {badge_colors::recent,    badge_colors::text};  // Blue
    if (upper == "DONE")
        return {badge_colors::default_bg, badge_colors::text}; // Gray
    if (upper == "INACTIVE")
        return {badge_colors::disabled,  badge_colors::text};  // Gray
    return {badge_colors::default_bg, badge_colors::text};     // Default gray
}

/**
 * @brief Badge colour resolver for report concurrency policy.
 *
 * Maps policy codes (skip/queue/fail) to badge colours.
 */
inline badge_color_pair resolve_concurrency_policy_badge_color(const QString& value) {
    const auto upper = value.toUpper();
    if (upper == "SKIP")
        return {badge_colors::old,    badge_colors::text};  // Amber
    if (upper == "QUEUE")
        return {badge_colors::recent, badge_colors::text};  // Blue
    if (upper == "FAIL")
        return {badge_colors::locked, badge_colors::text};  // Red
    return {badge_colors::default_bg, badge_colors::text};
}

/**
 * @brief Badge colour resolver for report definition lifecycle state.
 *
 * Maps FSM state names (draft/active/suspended/archived) to badge colours.
 */
inline badge_color_pair resolve_report_definition_badge_color(const QString& value) {
    const auto upper = value.toUpper();
    if (upper == "ACTIVE")
        return {badge_colors::online,   badge_colors::text};  // Green
    if (upper == "SUSPENDED")
        return {badge_colors::old,      badge_colors::text};  // Amber
    if (upper == "ARCHIVED")
        return {badge_colors::locked,   badge_colors::text};  // Red
    if (upper == "DRAFT")
        return {badge_colors::disabled, badge_colors::text};  // Gray
    return {badge_colors::default_bg, badge_colors::text};
}

}

#endif
