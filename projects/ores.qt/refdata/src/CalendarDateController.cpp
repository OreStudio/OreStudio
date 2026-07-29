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
#include "ores.qt/CalendarDateController.hpp"
#include "ores.qt/CalendarDateMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/messaging/calendar_date_protocol.hpp"
#include <QFutureWatcher>
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <QtConcurrent>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

CalendarDateController::CalendarDateController(QMainWindow* mainWindow,
                                               QMdiArea* mdiArea,
                                               ClientManager* clientManager,
                                               const QString& username,
                                               QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, std::string_view{}, parent)
    , calendarCode_()
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "CalendarDateController created";
}

void CalendarDateController::showListWindow() {
    showListWindow(QString());
}

void CalendarDateController::openForParent(const QString& calendarCode) {
    showListWindow(calendarCode);
}

void CalendarDateController::showListWindow(const QString& calendarCode) {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called for calendarCode="
                               << calendarCode.toStdString();
    calendarCode_ = calendarCode;

    const QString key = build_window_key("list", "calendar_dates_" + calendarCode);
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new CalendarDateMdiWindow(clientManager_, username_, calendarCode_);

    // Connect signals
    connect(listWindow_,
            &CalendarDateMdiWindow::statusChanged,
            this,
            &CalendarDateController::statusMessage);
    connect(listWindow_,
            &CalendarDateMdiWindow::errorOccurred,
            this,
            &CalendarDateController::errorMessage);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Browse Holidays");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Table, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);
    listMdiSubWindow_->setGeometryKey(key);
    UiPersistence::restoreMdiGeometry(key, listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_,
            &QObject::destroyed,
            this,
            [self = QPointer<CalendarDateController>(this),
             key,
             destroyedSubWindow = listMdiSubWindow_]() {
                if (!self)
                    return;
                self->untrack_window(key);
                // A different parent's window may have opened (and become the
                // tracked listWindow_/listMdiSubWindow_) since this one did --
                // only clear the pointers if they still reference the window
                // actually being destroyed, so closing an older parent-scoped
                // window can't orphan a still-open, more-recently-opened one.
                if (self->listMdiSubWindow_ == destroyedSubWindow) {
                    self->listWindow_ = nullptr;
                    self->listMdiSubWindow_ = nullptr;
                }
            });

    BOOST_LOG_SEV(lg(), debug) << "Calendar Date list window created";
}

void CalendarDateController::closeAllWindows() {
    BOOST_LOG_SEV(lg(), debug) << "closeAllWindows called";

    // Close all managed windows
    QList<QString> keys = managed_windows_.keys();
    for (const QString& key : keys) {
        if (auto* window = managed_windows_.value(key)) {
            window->close();
        }
    }
    managed_windows_.clear();

    listWindow_ = nullptr;
    listMdiSubWindow_ = nullptr;
}

void CalendarDateController::reloadListWindow() {
    // EntityController's listWindow()/reloadListWindow() contract tracks
    // a single window per controller. With multiple parent-scoped list
    // windows open concurrently (e.g. two different parents' lists),
    // only the most-recently-opened one is reloaded/marked stale here --
    // a known limitation of that single-window contract, not something
    // this knob's templates alone can fix without widening
    // EntityController itself to track N windows.
    if (listWindow_) {
        listWindow_->reload();
    }
}


EntityListMdiWindow* CalendarDateController::listWindow() const {
    return listWindow_;
}

}
