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
#include "ores.qt/CalendarController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/CalendarDetailDialog.hpp"
#include "ores.qt/CalendarHistoryDialog.hpp"
#include "ores.qt/CalendarMdiWindow.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.qt/VersionNavigationHelper.hpp"
#include "ores.refdata.api/eventing/calendar_changed_event.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view calendar_event_name =
    eventing::domain::event_traits<refdata::eventing::calendar_changed_event>::name;
}

CalendarController::CalendarController(QMainWindow* mainWindow,
                                       QMdiArea* mdiArea,
                                       ClientManager* clientManager,
                                       ChangeReasonCache* changeReasonCache,
                                       const QString& username,
                                       BadgeCache* badgeCache,
                                       QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, calendar_event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , badgeCache_(badgeCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "CalendarController created";
}

void CalendarController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "calendars");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new CalendarMdiWindow(clientManager_, username_, badgeCache_);

    // Connect signals
    connect(
        listWindow_, &CalendarMdiWindow::statusChanged, this, &CalendarController::statusMessage);
    connect(
        listWindow_, &CalendarMdiWindow::errorOccurred, this, &CalendarController::errorMessage);
    connect(listWindow_,
            &CalendarMdiWindow::showCalendarDetails,
            this,
            &CalendarController::onShowDetails);
    connect(listWindow_,
            &CalendarMdiWindow::addNewRequested,
            this,
            &CalendarController::onAddNewRequested);
    connect(listWindow_,
            &CalendarMdiWindow::showCalendarHistory,
            this,
            &CalendarController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Calendars");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::CalendarClock, IconUtils::DefaultIconColor));
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
            [self = QPointer<CalendarController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Calendar list window created";
}

void CalendarController::closeAllWindows() {
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

void CalendarController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void CalendarController::onShowDetails(const refdata::domain::calendar& calendar) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << calendar.code;
    showDetailWindow(calendar);
}

void CalendarController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new calendar requested";
    showAddWindow();
}


void CalendarController::onShowHistory(const refdata::domain::calendar& calendar) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << calendar.code;
    showHistoryWindow(QString::fromStdString(calendar.code));
}

void CalendarController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new calendar";

    auto* detailDialog = new CalendarDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &CalendarDetailDialog::statusMessage,
            this,
            &CalendarController::statusMessage);
    connect(
        detailDialog, &CalendarDetailDialog::errorMessage, this, &CalendarController::errorMessage);
    connect(detailDialog,
            &CalendarDetailDialog::calendarSaved,
            this,
            [self = QPointer<CalendarController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Calendar saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Calendar");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::CalendarClock, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CalendarController::showDetailWindow(const refdata::domain::calendar& calendar) {

    const QString identifier = QString::fromStdString(calendar.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << calendar.code;

    auto* detailDialog = new CalendarDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setCalendar(calendar);

    connect(detailDialog,
            &CalendarDetailDialog::statusMessage,
            this,
            &CalendarController::statusMessage);
    connect(
        detailDialog, &CalendarDetailDialog::errorMessage, this, &CalendarController::errorMessage);
    connect(detailDialog,
            &CalendarDetailDialog::calendarSaved,
            this,
            [self = QPointer<CalendarController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Calendar saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &CalendarDetailDialog::calendarDeleted,
            this,
            [self = QPointer<CalendarController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Calendar deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Calendar: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::CalendarClock, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<CalendarController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CalendarController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for calendar: " << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog = new CalendarHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog,
            &CalendarHistoryDialog::statusChanged,
            this,
            [self = QPointer<CalendarController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &CalendarHistoryDialog::errorOccurred,
            this,
            [self = QPointer<CalendarController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &CalendarHistoryDialog::revertVersionRequested,
            this,
            &CalendarController::onRevertVersion);
    connect(historyDialog,
            &CalendarHistoryDialog::openVersionRequested,
            this,
            &CalendarController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Calendar History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<CalendarController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void CalendarController::onOpenVersion(const refdata::domain::calendar& calendar,
                                       int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for calendar: " << calendar.code;

    const QString code = QString::fromStdString(calendar.code);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new CalendarDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    // A single version-nav toolbar (first/prev/next/last) only means something
    // if the dialog has the *full* history to navigate, not just this one
    // version. When onOpenVersion's sender is the HistoryDialog that
    // requested it, pull that history across; otherwise fall back to a plain
    // read-only single-version display.
    if (!wireVersionHistory<CalendarHistoryDialog>(sender(), detailDialog, versionNumber)) {
        detailDialog->setCalendar(calendar);
        detailDialog->setReadOnly(true, versionNumber);
    }
    connect(detailDialog,
            &CalendarDetailDialog::revertRequested,
            this,
            &CalendarController::onRevertVersion);

    connect(detailDialog,
            &CalendarDetailDialog::statusMessage,
            this,
            [self = QPointer<CalendarController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &CalendarDetailDialog::errorMessage,
            this,
            [self = QPointer<CalendarController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Calendar: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CalendarController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void CalendarController::onRevertVersion(const refdata::domain::calendar& calendar) {
    BOOST_LOG_SEV(lg(), info) << "Reverting calendar to version: " << calendar.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new CalendarDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_calendar = calendar;
    reverted_calendar.version = 0;
    detailDialog->setCalendar(reverted_calendar);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &CalendarDetailDialog::statusMessage,
            this,
            &CalendarController::statusMessage);
    connect(
        detailDialog, &CalendarDetailDialog::errorMessage, this, &CalendarController::errorMessage);
    connect(detailDialog,
            &CalendarDetailDialog::calendarSaved,
            this,
            [self = QPointer<CalendarController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Calendar reverted: " << code.toStdString();
                emit self->statusMessage(QString("Calendar '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Calendar: %1").arg(QString::fromStdString(calendar.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* CalendarController::listWindow() const {
    return listWindow_;
}

void CalendarController::notifyOpenDialogs(const QStringList& entityIds) {
    for (auto it = managed_windows_.begin(); it != managed_windows_.end(); ++it) {
        auto* window = it.value();
        if (!window)
            continue;

        if (it.key().startsWith("details.")) {
            if (auto* dialog = qobject_cast<DetailDialogBase*>(window->widget())) {
                if (entityIds.isEmpty() || entityIds.contains(dialog->code())) {
                    dialog->markAsStale();
                }
            }
        } else if (it.key().startsWith("history.")) {
            if (auto* dialog = qobject_cast<HistoryDialogBase*>(window->widget())) {
                if (entityIds.isEmpty() || entityIds.contains(dialog->code())) {
                    dialog->markAsStale();
                }
            }
        }
    }
}

}
