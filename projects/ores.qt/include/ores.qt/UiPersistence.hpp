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
#ifndef ORES_QT_UI_PERSISTENCE_HPP
#define ORES_QT_UI_PERSISTENCE_HPP

#include <QSize>
#include <QString>

class QWidget;
class QSplitter;
class QHeaderView;

namespace ores::qt {

/**
 * @brief Static utility for persisting UI widget state via QSettings.
 *
 * All methods use QSettings("OreStudio", "OreStudio") and store values
 * under the given group name. Methods are composable — callers may save
 * and restore only the subset of state they need.
 *
 * Typical usage:
 * @code
 * // In closeEvent():
 * UiPersistence::saveSize("MyWindow", this);
 * UiPersistence::saveSplitter("MyWindow", splitter_);
 *
 * // In constructor/setup (after widgets are created):
 * savedSize_ = UiPersistence::restoreSize("MyWindow", {800, 500});
 * UiPersistence::restoreSplitter("MyWindow", splitter_);
 * @endcode
 */
class UiPersistence {
public:
    UiPersistence() = delete;

    /**
     * @brief Save widget size to settings.
     */
    static void saveSize(const QString& group, const QWidget* w);

    /**
     * @brief Restore saved widget size, or return defaultSize if none saved.
     */
    static QSize restoreSize(const QString& group,
                             const QSize& defaultSize = {900, 400});

    /**
     * @brief Save splitter position to settings.
     */
    static void saveSplitter(const QString& group, const QSplitter* s);

    /**
     * @brief Restore saved splitter position. No-op if no state is saved.
     */
    static void restoreSplitter(const QString& group, QSplitter* s);

    /**
     * @brief Save table header state (column widths, order, visibility) to settings.
     *
     * @param version  Bump this when the column layout changes to invalidate old state.
     */
    static void saveHeader(const QString& group, const QHeaderView* h,
                           int version = 1);

    /**
     * @brief Restore saved table header state.
     *
     * @return true if state was restored successfully.
     *         false if no saved state or version mismatch — the caller should
     *         apply their default hidden columns.
     */
    static bool restoreHeader(const QString& group, QHeaderView* h,
                              int version = 1);
};

}

#endif
