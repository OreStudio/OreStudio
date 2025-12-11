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
#include "ores.qt/DetachableMdiSubWindow.hpp"

#include <iomanip>
#include <sstream>

#include <QMouseEvent>
#include <QContextMenuEvent>
#include <QMenu>
#include <QCloseEvent>

namespace {

/**
 * @brief Helper function to convert QFlags to a readable hexadecimal string
 * for debugging.
 */
std::string flagsToHex(Qt::WindowFlags flags) {
    std::stringstream ss;
    ss << "0x" << std::hex << std::setw(8) << std::setfill('0') << flags.toInt();
    return ss.str();
}

}

namespace ores::qt {

using namespace ores::utility::log;

DetachableMdiSubWindow::DetachableMdiSubWindow(QWidget* parent)
    : QMdiSubWindow(parent), isDetached_(false),
      savedMdiArea_(qobject_cast<QMdiArea*>(parent)) {
    // If parent is not QMdiArea, it will be set when QMdiArea::addSubWindow()
    // is called.
}

void DetachableMdiSubWindow::detach() {
    if (isDetached_) {
        BOOST_LOG_SEV(lg(), debug) << "Window already detached";
        return;
    }

    // Capture the MDI area before detaching.
    // mdiArea() returns the MdiArea if the parent is the viewport or the area itself.
    savedMdiArea_ = mdiArea();
    if (!savedMdiArea_) {
        BOOST_LOG_SEV(lg(), error) << "Cannot detach: not currently in an MDI area";
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Detaching window: "
                              << windowTitle().toStdString();

    // Save current state relative to MDI area
    savedMdiPosition_ = pos();
    savedMdiSize_ = size();

    // Get current global position for smooth transition
    QPoint globalPos = mapToGlobal(QPoint(0, 0));

    // Properly remove from MDI area first
    savedMdiArea_->removeSubWindow(this);

    BOOST_LOG_SEV(lg(), debug) << "Detaching: Flags before reset: "
                               << flagsToHex(windowFlags());

    // Now set window flags for standalone window
    const Qt::WindowFlags detachableFlags =
        Qt::Window |
        Qt::WindowTitleHint |
        Qt::WindowSystemMenuHint |
        Qt::WindowMinMaxButtonsHint |
        Qt::WindowCloseButtonHint;
    setWindowFlags(detachableFlags);

    BOOST_LOG_SEV(lg(), debug) << "Detaching: Flags after setting them: "
                               << flagsToHex(windowFlags());

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

void DetachableMdiSubWindow::contextMenuEvent(QContextMenuEvent* event) {
    // Only show custom menu in title bar area
    if (event->pos().y() <= 30) {
        QMenu menu(this);

        if (isDetached_) {
            // When detached, show informational message instead of reattach option
            QAction* infoAction = menu.addAction("Close window to reopen as MDI");
            infoAction->setEnabled(false);  // Disabled, just informational
        } else {
            QAction* detachAction = menu.addAction("Detach");
            connect(detachAction, &QAction::triggered, this,
                &DetachableMdiSubWindow::detach);
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
