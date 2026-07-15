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
    static inline const QColor synthetic_indicator{100, 149, 237}; // Cornflower blue

    /**
     * @brief Log level colors for telemetry viewer badges and filters.
     */
    static inline const QColor level_trace{107, 114, 128}; // Gray
    static inline const QColor level_debug{59, 130, 246};  // Blue
    static inline const QColor level_info{34, 197, 94};    // Green
    static inline const QColor level_warn{234, 179, 8};    // Amber
    static inline const QColor level_error{239, 68, 68};   // Red
    static inline const QColor level_text{255, 255, 255};  // White text on badges

    /**
     * @brief Color for indicating active sessions in tree view.
     */
    static inline const QColor active_session{34, 197, 94}; // Green (same as INFO)

    /**
     * @brief Fallback colors for unresolved badge definitions.
     *
     * Used when BadgeCache has no definition for a code domain value.
     * Deliberately NOT gray: in the UX language gray is reserved for
     * inactive/off/no/negative states, so a missing badge definition
     * must look like a gap to fix, not like an inactive record. Orange
     * is otherwise unused in the badge palette.
     */
    static inline const QColor badge_fallback{249, 115, 22};       // Orange (#f97316)
    static inline const QColor badge_fallback_text{255, 255, 255}; // White

    /**
     * @brief GitHub dark-theme diff palette, used for the history
     * changes tab. Line background is a faint tint (alpha ~.15);
     * intra-line/token highlight is a stronger shade (alpha ~.40) —
     * the new side uses a fractionally darker green for the
     * highlight than the line background, matching GitHub's own
     * palette, not just an alpha bump on the same RGB.
     */
    static inline const QColor diff_old_line_bg{248, 81, 73, 38};
    static inline const QColor diff_old_span_bg{248, 81, 73, 102};
    static inline const QColor diff_new_line_bg{63, 185, 80, 38};
    static inline const QColor diff_new_span_bg{46, 160, 67, 102};

    /**
     * @brief History timeline card palette: distinguishes the
     * provenance lines (who/why a version was recorded) from each
     * other and from the diff pane's changed-fields, so "Performed
     * By", the reason-code badge, and the free-text commentary are
     * each visually distinct at a glance rather than reading as one
     * undifferentiated block of grey text.
     */
    static inline const QColor timeline_performed_by{100, 149, 237}; // Cornflower blue
    static inline const QColor timeline_reason_bg{234, 179, 8, 40};  // Amber, faint chip fill
    static inline const QColor timeline_reason_text{234, 179, 8};    // Amber
    static inline const QColor timeline_commentary{163, 148, 217};   // Muted violet
};

}

#endif
