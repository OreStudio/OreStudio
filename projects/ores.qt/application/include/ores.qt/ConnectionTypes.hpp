/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_CONNECTION_TYPES_HPP
#define ORES_QT_CONNECTION_TYPES_HPP

#include <functional>
#include <vector>
#include <QString>
#include <QColor>

namespace ores::qt {

/**
 * @brief Callback type for testing connections.
 *
 * Returns empty string on success, error message on failure.
 */
using TestConnectionCallback = std::function<QString(
    const QString& host, int port, const QString& username, const QString& password)>;

/**
 * @brief Predefined tag colors for consistent UI display.
 */
inline const std::vector<QColor> tag_colors = {
    QColor(59, 130, 246),   // Blue
    QColor(34, 197, 94),    // Green
    QColor(234, 179, 8),    // Amber
    QColor(239, 68, 68),    // Red
    QColor(168, 85, 247),   // Purple
    QColor(236, 72, 153),   // Pink
    QColor(20, 184, 166),   // Teal
    QColor(249, 115, 22),   // Orange
};

/**
 * @brief Get a color for a tag based on its name.
 */
inline QColor colorForTag(const QString& name) {
    uint hash = qHash(name);
    return tag_colors[hash % tag_colors.size()];
}

}

#endif
