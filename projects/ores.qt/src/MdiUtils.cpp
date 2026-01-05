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
#include "ores.qt/MdiUtils.hpp"

#include <QWidget>
#include <QMdiSubWindow>

namespace ores::qt {

bool MdiUtils::markParentWindowAsStale(QWidget* widget) {
    if (!widget)
        return false;

    QWidget* parent = widget->parentWidget();
    while (parent) {
        if (auto* mdiSubWindow = qobject_cast<QMdiSubWindow*>(parent)) {
            QString currentTitle = mdiSubWindow->windowTitle();
            if (!currentTitle.contains(stale_marker)) {
                mdiSubWindow->setWindowTitle(currentTitle + stale_marker);
                return true;
            }
            return false; // Already marked
        }
        parent = parent->parentWidget();
    }
    return false; // No MDI parent found
}

bool MdiUtils::clearParentWindowStaleMarker(QWidget* widget) {
    if (!widget)
        return false;

    QWidget* parent = widget->parentWidget();
    while (parent) {
        if (auto* mdiSubWindow = qobject_cast<QMdiSubWindow*>(parent)) {
            QString currentTitle = mdiSubWindow->windowTitle();
            if (currentTitle.contains(stale_marker)) {
                currentTitle.remove(stale_marker);
                mdiSubWindow->setWindowTitle(currentTitle);
                return true;
            }
            return false; // Not marked
        }
        parent = parent->parentWidget();
    }
    return false; // No MDI parent found
}

}
