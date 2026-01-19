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

    /**
     * @brief Color for indicating synthetic/generated data that hasn't been saved.
     *
     * Used for: generated currency row highlighting in grids.
     * Blue - distinguishes from yellow (stale/updated) indicator.
     */
    static inline const QColor synthetic_indicator{100, 149, 237};  // Cornflower blue

    /**
     * @brief Log level colors for telemetry viewer badges and filters.
     */
    static inline const QColor level_trace{107, 114, 128};   // Gray
    static inline const QColor level_debug{59, 130, 246};    // Blue
    static inline const QColor level_info{34, 197, 94};      // Green
    static inline const QColor level_warn{234, 179, 8};      // Amber
    static inline const QColor level_error{239, 68, 68};     // Red
    static inline const QColor level_text{255, 255, 255};    // White text on badges

    /**
     * @brief Color for indicating active sessions in tree view.
     */
    static inline const QColor active_session{34, 197, 94};  // Green (same as INFO)
};

/**
 * @brief Common badge colors for item delegates.
 *
 * Used across various delegates for consistent badge styling.
 */
struct badge_colors {
    // Common text color for all badges
    static inline const QColor text{255, 255, 255};

    // Default/unknown state
    static inline const QColor default_bg{107, 114, 128};  // Gray

    // Boolean/enabled state badges
    static inline const QColor enabled{34, 197, 94};       // Green
    static inline const QColor disabled{107, 114, 128};    // Gray
    static inline const QColor yes{34, 197, 94};           // Green
    static inline const QColor no{107, 114, 128};          // Gray

    // Status indicators
    static inline const QColor online{34, 197, 94};        // Green
    static inline const QColor recent{59, 130, 246};       // Blue
    static inline const QColor old{234, 179, 8};           // Amber
    static inline const QColor never{107, 114, 128};       // Gray
    static inline const QColor locked{239, 68, 68};        // Red
    static inline const QColor unlocked{107, 114, 128};    // Gray

    // Data quality dimension badges
    static inline const QColor origin_primary{59, 130, 246};   // Blue
    static inline const QColor origin_derived{139, 92, 246};   // Purple
    static inline const QColor nature_actual{34, 197, 94};     // Green
    static inline const QColor nature_estimated{234, 179, 8};  // Amber
    static inline const QColor nature_simulated{236, 72, 153}; // Pink
    static inline const QColor treatment_raw{107, 114, 128};     // Gray
    static inline const QColor treatment_cleaned{14, 165, 233};  // Sky blue
    static inline const QColor treatment_enriched{168, 85, 247}; // Purple
};

// Backward compatibility alias
using dimension_badge_colors = badge_colors;

}

#endif
