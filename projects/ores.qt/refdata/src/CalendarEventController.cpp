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
#include "ores.qt/CalendarEventController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/CalendarEventDetailDialog.hpp"
#include "ores.qt/CalendarEventMdiWindow.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/HistoryDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/calendar_event_changed_event.hpp"
#include "ores.refdata.api/messaging/calendar_event_protocol.hpp"
#include <QFutureWatcher>
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view evt_event_name =
    eventing::domain::event_traits<refdata::eventing::calendar_event_changed_event>::name;
}

CalendarEventController::CalendarEventController(QMainWindow* mainWindow,
                                                 QMdiArea* mdiArea,
                                                 ClientManager* clientManager,
                                                 ChangeReasonCache* changeReasonCache,
                                                 const QString& username,
                                                 QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, evt_event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "CalendarEventController created";
}

void CalendarEventController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "calendar_events");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new CalendarEventMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &CalendarEventMdiWindow::statusChanged,
            this,
            &CalendarEventController::statusMessage);
    connect(listWindow_,
            &CalendarEventMdiWindow::errorOccurred,
            this,
            &CalendarEventController::errorMessage);
    connect(listWindow_,
            &CalendarEventMdiWindow::showEventDetails,
            this,
            &CalendarEventController::onShowDetails);
    connect(listWindow_,
            &CalendarEventMdiWindow::addNewRequested,
            this,
            &CalendarEventController::onAddNewRequested);
    connect(listWindow_,
            &CalendarEventMdiWindow::showEventHistory,
            this,
            &CalendarEventController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Calendar Events");
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
            [self = QPointer<CalendarEventController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Calendar Event list window created";
}

void CalendarEventController::closeAllWindows() {
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

void CalendarEventController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void CalendarEventController::onShowDetails(const refdata::domain::calendar_event& evt) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << boost::uuids::to_string(evt.id);
    showDetailWindow(evt);
}

void CalendarEventController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new calendar event requested";
    showAddWindow();
}


void CalendarEventController::onShowHistory(const refdata::domain::calendar_event& evt) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << boost::uuids::to_string(evt.id);
    showHistoryWindow(evt);
}

void CalendarEventController::wireDetailDialogCommon(CalendarEventDetailDialog* detailDialog) {
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());

    connect(detailDialog,
            &CalendarEventDetailDialog::statusMessage,
            this,
            &CalendarEventController::statusMessage);
    connect(detailDialog,
            &CalendarEventDetailDialog::errorMessage,
            this,
            &CalendarEventController::errorMessage);
}

void CalendarEventController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new calendar event";

    auto* detailDialog = new CalendarEventDetailDialog(mainWindow_);
    wireDetailDialogCommon(detailDialog);
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &CalendarEventDetailDialog::evtSaved,
            this,
            [self = QPointer<CalendarEventController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Calendar Event saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Calendar Event");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::CalendarClock, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CalendarEventController::showDetailWindow(const refdata::domain::calendar_event& evt) {

    const QString identifier = QString::fromStdString(boost::uuids::to_string(evt.id));
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << boost::uuids::to_string(evt.id);

    auto* detailDialog = new CalendarEventDetailDialog(mainWindow_);
    wireDetailDialogCommon(detailDialog);
    detailDialog->setCreateMode(false);
    detailDialog->setEvent(evt);

    connect(detailDialog,
            &CalendarEventDetailDialog::evtSaved,
            this,
            [self = QPointer<CalendarEventController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Calendar Event saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &CalendarEventDetailDialog::evtDeleted,
            this,
            [self = QPointer<CalendarEventController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Calendar Event deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Calendar Event: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::CalendarClock, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<CalendarEventController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CalendarEventController::showHistoryWindow(const refdata::domain::calendar_event& evt) {
    const QString code = QString::fromStdString(boost::uuids::to_string(evt.id));
    BOOST_LOG_SEV(lg(), info) << "Opening history window for calendar event: "
                              << boost::uuids::to_string(evt.id);

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << boost::uuids::to_string(evt.id);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << boost::uuids::to_string(evt.id);

    const QString entityId = QString::fromStdString(boost::uuids::to_string(evt.id));
    auto* historyDialog =
        new HistoryDialog(std::string(entity_type_of(refdata::domain::calendar_event{})),
                          entityId.toStdString(),
                          clientManager_,
                          mainWindow_);

    connect(historyDialog,
            &HistoryDialog::statusChanged,
            this,
            [self = QPointer<CalendarEventController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::errorOccurred,
            this,
            [self = QPointer<CalendarEventController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::revertVersionRequested,
            this,
            [self = QPointer<CalendarEventController>(this)](
                const QString& /*entityType*/, const QString& entityId, int version) {
                if (!self)
                    return;
                self->onRevertHistoryVersion(entityId, version);
            });
    connect(historyDialog,
            &HistoryDialog::openVersionRequested,
            this,
            [self = QPointer<CalendarEventController>(this)](
                const QString& /*entityType*/, const QString& entityId, int version) {
                if (!self)
                    return;
                self->onOpenHistoryVersion(entityId, version);
            });

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Calendar Event History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<CalendarEventController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void CalendarEventController::onOpenVersion(const refdata::domain::calendar_event& evt,
                                            int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for calendar event: " << boost::uuids::to_string(evt.id);

    const QString code = QString::fromStdString(boost::uuids::to_string(evt.id));
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new CalendarEventDetailDialog(mainWindow_);
    wireDetailDialogCommon(detailDialog);
    detailDialog->setEvent(evt);
    detailDialog->setReadOnly(true);

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Calendar Event: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CalendarEventController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void CalendarEventController::fetchCalendarEventHistory(
    const QString& entityId,
    std::function<void(std::expected<std::vector<refdata::domain::calendar_event>, QString>)>
        callback) {
    refdata::messaging::get_calendar_event_history_request request;
    request.id = entityId.toStdString();

    using FetchResult = std::expected<std::vector<refdata::domain::calendar_event>, QString>;

    QPointer<CalendarEventController> self = this;
    QPointer<ClientManager> clientManager = clientManager_;
    auto future = QtConcurrent::run([clientManager, request = std::move(request)]() -> FetchResult {
        if (!clientManager || !clientManager->isConnected())
            return std::unexpected(QString("Not connected to server"));
        auto result = clientManager->process_authenticated_request(std::move(request));
        if (!result)
            return std::unexpected(QString::fromStdString(result.error()));
        if (!result->success)
            return std::unexpected(QString::fromStdString(result->message));
        return std::move(result->history);
    });

    auto* watcher = new QFutureWatcher<FetchResult>(this);
    connect(watcher,
            &QFutureWatcher<FetchResult>::finished,
            this,
            [self, watcher, callback = std::move(callback)]() mutable {
                auto result = watcher->result();
                watcher->deleteLater();
                if (!self)
                    return;
                callback(std::move(result));
            });
    watcher->setFuture(future);
}

void CalendarEventController::onOpenHistoryVersion(const QString& entityId, int versionNumber) {
    QPointer<CalendarEventController> self = this;
    fetchCalendarEventHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::calendar_event>, QString> result) {
            if (!self)
                return;
            if (!result) {
                emit self->errorMessage(QString("Failed to load history for '%1': %2")
                                            .arg(entityId)
                                            .arg(result.error()));
                return;
            }
            const auto& history = *result;
            const auto it = std::find_if(history.begin(), history.end(), [&](const auto& v) {
                return v.version == versionNumber;
            });
            if (it == history.end()) {
                emit self->errorMessage(
                    QString("Version %1 not found for '%2'").arg(versionNumber).arg(entityId));
                return;
            }
            self->onOpenVersion(*it, versionNumber);
        });
}

void CalendarEventController::onRevertHistoryVersion(const QString& entityId, int versionNumber) {
    QPointer<CalendarEventController> self = this;
    fetchCalendarEventHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::calendar_event>, QString> result) {
            if (!self)
                return;
            if (!result) {
                emit self->errorMessage(QString("Failed to load history for '%1': %2")
                                            .arg(entityId)
                                            .arg(result.error()));
                return;
            }
            const auto& history = *result;
            const auto it = std::find_if(history.begin(), history.end(), [&](const auto& v) {
                return v.version == versionNumber;
            });
            if (it == history.end()) {
                emit self->errorMessage(
                    QString("Version %1 not found for '%2'").arg(versionNumber).arg(entityId));
                return;
            }
            self->onRevertVersion(*it);
        });
}

void CalendarEventController::onRevertVersion(const refdata::domain::calendar_event& evt) {
    BOOST_LOG_SEV(lg(), info) << "Reverting calendar event to version: " << evt.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new CalendarEventDetailDialog(mainWindow_);
    wireDetailDialogCommon(detailDialog);
    auto reverted_evt = evt;
    reverted_evt.version = 0;
    detailDialog->setEvent(reverted_evt);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &CalendarEventDetailDialog::evtSaved,
            this,
            [self = QPointer<CalendarEventController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Calendar Event reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Calendar Event '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Calendar Event: %1")
                                     .arg(QString::fromStdString(boost::uuids::to_string(evt.id))));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* CalendarEventController::listWindow() const {
    return listWindow_;
}

void CalendarEventController::notifyOpenDialogs(const QStringList& entityIds) {
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
