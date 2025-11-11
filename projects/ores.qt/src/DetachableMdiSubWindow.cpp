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
#include <QMouseEvent>
#include <QContextMenuEvent>
#include <QMenu>
#include <QCloseEvent>
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::utility::log;

DetachableMdiSubWindow::DetachableMdiSubWindow(QWidget* parent)
    : QMdiSubWindow(parent),
      isDetached_(false),
      savedMdiArea_(nullptr) {
}

void DetachableMdiSubWindow::detach() {
    if (isDetached_) {
        BOOST_LOG_SEV(lg(), debug) << "Window already detached";
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Detaching window: " << windowTitle().toStdString();

    // Save current state
    savedMdiArea_ = mdiArea();
    savedMdiPosition_ = pos();
    savedMdiSize_ = size();

    // Get current global position for smooth transition
    QPoint globalPos = mapToGlobal(QPoint(0, 0));

    // Remove from MDI area
    setParent(nullptr);

    // Convert to top-level window
    setWindowFlags(Qt::Window);

    // Position at same screen location
    move(globalPos);

    isDetached_ = true;

    // Show as independent window
    show();
    raise();
    activateWindow();

    emit detachedStateChanged(true);

    BOOST_LOG_SEV(lg(), info) << "Window detached successfully";
}

void DetachableMdiSubWindow::reattach() {
    if (!isDetached_) {
        BOOST_LOG_SEV(lg(), debug) << "Window already attached";
        return;
    }

    if (!savedMdiArea_) {
        BOOST_LOG_SEV(lg(), error) << "Cannot reattach: no saved MDI area";
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Reattaching window: " << windowTitle().toStdString();

    // Store the widget before reparenting
    QWidget* contentWidget = widget();

    // Temporarily remove the widget from this subwindow
    setWidget(nullptr);

    // Hide window before changing parent
    hide();

    // Restore parent to MDI area's viewport (where subwindows actually live)
    if (savedMdiArea_->viewport()) {
        setParent(savedMdiArea_->viewport());
    } else {
        setParent(savedMdiArea_);
    }

    // Restore MDI subwindow flags
    setWindowFlags(Qt::SubWindow);

    // Restore the content widget
    setWidget(contentWidget);

    // Restore position and size within MDI
    move(savedMdiPosition_);
    resize(savedMdiSize_);

    isDetached_ = false;

    // Show in MDI area
    show();

    emit detachedStateChanged(false);

    BOOST_LOG_SEV(lg(), info) << "Window reattached successfully";
}

void DetachableMdiSubWindow::contextMenuEvent(QContextMenuEvent* event) {
    // Only show custom menu in title bar area
    if (event->pos().y() <= 30) {
        QMenu menu(this);

        if (isDetached_) {
            QAction* reattachAction = menu.addAction("Reattach");
            connect(reattachAction, &QAction::triggered, this, &DetachableMdiSubWindow::reattach);
        } else {
            QAction* detachAction = menu.addAction("Detach");
            connect(detachAction, &QAction::triggered, this, &DetachableMdiSubWindow::detach);
        }

        menu.exec(event->globalPos());
        event->accept();
        return;
    }

    QMdiSubWindow::contextMenuEvent(event);
}

void DetachableMdiSubWindow::closeEvent(QCloseEvent* event) {
    BOOST_LOG_SEV(lg(), info) << "Closing window: " << windowTitle().toStdString()
                             << " (detached=" << isDetached_ << ")";
    QMdiSubWindow::closeEvent(event);
}

}
