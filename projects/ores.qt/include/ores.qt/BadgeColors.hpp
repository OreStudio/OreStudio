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

}

#endif
