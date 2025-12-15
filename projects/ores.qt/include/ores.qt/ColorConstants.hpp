/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_COLOR_CONSTANTS_HPP
#define ORES_QT_COLOR_CONSTANTS_HPP

#include <QColor>

namespace ores::qt {

/**
 * @brief Centralized color constants for consistent UI styling.
 *
 * These colors are used across the application for visual indicators.
 * Change them here to update the appearance globally.
 */
struct color_constants {
    /**
     * @brief Color for indicating stale/changed data that needs attention.
     *
     * Used for: reload button stale indicator, recently changed row highlighting.
     * Yellow/gold - visible in both light and dark themes.
     */
    static inline const QColor stale_indicator{255, 215, 0};

    /**
     * @brief Standard icon color for toolbar icons in dark theme.
     */
    static inline const QColor icon_color{220, 220, 220};
};

}

#endif
