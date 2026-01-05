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
#ifndef ORES_QT_MDI_UTILS_HPP
#define ORES_QT_MDI_UTILS_HPP

class QWidget;

namespace ores::qt {

/**
 * @brief Utility functions for MDI window operations.
 */
class MdiUtils final {
public:
    MdiUtils() = delete;

    /**
     * @brief Mark the parent MDI subwindow as having stale data.
     *
     * Traverses up the widget hierarchy to find the parent QMdiSubWindow
     * and appends "(Data Changed)" to its title if not already present.
     *
     * @param widget The widget whose parent MDI window should be marked.
     * @return true if the title was updated, false if no MDI parent found
     *         or title already marked.
     */
    static bool markParentWindowAsStale(QWidget* widget);

    /**
     * @brief Clear the stale marker from the parent MDI subwindow.
     *
     * Removes "(Data Changed)" from the window title if present.
     *
     * @param widget The widget whose parent MDI window should be cleared.
     * @return true if the title was updated, false if no MDI parent found
     *         or title was not marked.
     */
    static bool clearParentWindowStaleMarker(QWidget* widget);

private:
    static constexpr const char* stale_marker = " (Data Changed)";
};

}

#endif
