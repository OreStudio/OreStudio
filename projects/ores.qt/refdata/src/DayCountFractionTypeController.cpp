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
#include "ores.qt/DayCountFractionTypeController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DayCountFractionTypeDetailDialog.hpp"
#include "ores.qt/DayCountFractionTypeMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/HistoryDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/day_count_fraction_type_changed_event.hpp"
#include "ores.refdata.api/messaging/day_count_fraction_type_protocol.hpp"
#include <QFutureWatcher>
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <QtConcurrent>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view type_event_name =
    eventing::domain::event_traits<refdata::eventing::day_count_fraction_type_changed_event>::name;
}

DayCountFractionTypeController::DayCountFractionTypeController(QMainWindow* mainWindow,
                                                               QMdiArea* mdiArea,
                                                               ClientManager* clientManager,
                                                               ChangeReasonCache* changeReasonCache,
                                                               const QString& username,
                                                               QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, type_event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "DayCountFractionTypeController created";
}

void DayCountFractionTypeController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "day_count_fraction_types");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new DayCountFractionTypeMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &DayCountFractionTypeMdiWindow::statusChanged,
            this,
            &DayCountFractionTypeController::statusMessage);
    connect(listWindow_,
            &DayCountFractionTypeMdiWindow::errorOccurred,
            this,
            &DayCountFractionTypeController::errorMessage);
    connect(listWindow_,
            &DayCountFractionTypeMdiWindow::showTypeDetails,
            this,
            &DayCountFractionTypeController::onShowDetails);
    connect(listWindow_,
            &DayCountFractionTypeMdiWindow::addNewRequested,
            this,
            &DayCountFractionTypeController::onAddNewRequested);
    connect(listWindow_,
            &DayCountFractionTypeMdiWindow::showTypeHistory,
            this,
            &DayCountFractionTypeController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Day Count Fraction Types");
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
            [self = QPointer<DayCountFractionTypeController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Day Count Fraction Type list window created";
}

void DayCountFractionTypeController::closeAllWindows() {
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

void DayCountFractionTypeController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void DayCountFractionTypeController::onShowDetails(
    const refdata::domain::day_count_fraction_type& type) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << type.code;
    showDetailWindow(type);
}

void DayCountFractionTypeController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new day count fraction type requested";
    showAddWindow();
}


void DayCountFractionTypeController::onShowHistory(
    const refdata::domain::day_count_fraction_type& type) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << type.code;
    showHistoryWindow(QString::fromStdString(type.code));
}

void DayCountFractionTypeController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new day count fraction type";

    auto* detailDialog = new DayCountFractionTypeDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &DayCountFractionTypeDetailDialog::statusMessage,
            this,
            &DayCountFractionTypeController::statusMessage);
    connect(detailDialog,
            &DayCountFractionTypeDetailDialog::errorMessage,
            this,
            &DayCountFractionTypeController::errorMessage);
    connect(detailDialog,
            &DayCountFractionTypeDetailDialog::typeSaved,
            this,
            [self = QPointer<DayCountFractionTypeController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Day Count Fraction Type saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Day Count Fraction Type");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::CalendarClock, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void DayCountFractionTypeController::showDetailWindow(
    const refdata::domain::day_count_fraction_type& type) {

    const QString identifier = QString::fromStdString(type.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << type.code;

    auto* detailDialog = new DayCountFractionTypeDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setType(type);

    connect(detailDialog,
            &DayCountFractionTypeDetailDialog::statusMessage,
            this,
            &DayCountFractionTypeController::statusMessage);
    connect(detailDialog,
            &DayCountFractionTypeDetailDialog::errorMessage,
            this,
            &DayCountFractionTypeController::errorMessage);
    connect(detailDialog,
            &DayCountFractionTypeDetailDialog::typeSaved,
            this,
            [self = QPointer<DayCountFractionTypeController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Day Count Fraction Type saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &DayCountFractionTypeDetailDialog::typeDeleted,
            this,
            [self = QPointer<DayCountFractionTypeController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Day Count Fraction Type deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Day Count Fraction Type: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::CalendarClock, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<DayCountFractionTypeController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void DayCountFractionTypeController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for day count fraction type: "
                              << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog =
        new HistoryDialog(std::string(entity_type_of(refdata::domain::day_count_fraction_type{})),
                          code.toStdString(),
                          clientManager_,
                          mainWindow_);

    connect(historyDialog,
            &HistoryDialog::statusChanged,
            this,
            [self = QPointer<DayCountFractionTypeController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::errorOccurred,
            this,
            [self = QPointer<DayCountFractionTypeController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::revertVersionRequested,
            this,
            [self = QPointer<DayCountFractionTypeController>(this)](
                const QString& /*entityType*/, const QString& entityId, int version) {
                if (!self)
                    return;
                self->onRevertHistoryVersion(entityId, version);
            });
    connect(historyDialog,
            &HistoryDialog::openVersionRequested,
            this,
            [self = QPointer<DayCountFractionTypeController>(this)](
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
    historyWindow->setWindowTitle(QString("Day Count Fraction Type History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<DayCountFractionTypeController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void DayCountFractionTypeController::onOpenVersion(
    const refdata::domain::day_count_fraction_type& type, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for day count fraction type: " << type.code;

    const QString code = QString::fromStdString(type.code);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new DayCountFractionTypeDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setType(type);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &DayCountFractionTypeDetailDialog::statusMessage,
            this,
            [self = QPointer<DayCountFractionTypeController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &DayCountFractionTypeDetailDialog::errorMessage,
            this,
            [self = QPointer<DayCountFractionTypeController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Day Count Fraction Type: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<DayCountFractionTypeController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void DayCountFractionTypeController::fetchDayCountFractionTypeHistory(
    const QString& entityId,
    std::function<void(
        std::expected<std::vector<refdata::domain::day_count_fraction_type>, QString>)> callback) {
    refdata::messaging::get_day_count_fraction_type_history_request request;
    request.code = entityId.toStdString();

    using FetchResult =
        std::expected<std::vector<refdata::domain::day_count_fraction_type>, QString>;

    QPointer<DayCountFractionTypeController> self = this;
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

void DayCountFractionTypeController::onOpenHistoryVersion(const QString& entityId,
                                                          int versionNumber) {
    QPointer<DayCountFractionTypeController> self = this;
    fetchDayCountFractionTypeHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::day_count_fraction_type>, QString> result) {
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

void DayCountFractionTypeController::onRevertHistoryVersion(const QString& entityId,
                                                            int versionNumber) {
    QPointer<DayCountFractionTypeController> self = this;
    fetchDayCountFractionTypeHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::day_count_fraction_type>, QString> result) {
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

void DayCountFractionTypeController::onRevertVersion(
    const refdata::domain::day_count_fraction_type& type) {
    BOOST_LOG_SEV(lg(), info) << "Reverting day count fraction type to version: " << type.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new DayCountFractionTypeDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_type = type;
    reverted_type.version = 0;
    detailDialog->setType(reverted_type);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &DayCountFractionTypeDetailDialog::statusMessage,
            this,
            &DayCountFractionTypeController::statusMessage);
    connect(detailDialog,
            &DayCountFractionTypeDetailDialog::errorMessage,
            this,
            &DayCountFractionTypeController::errorMessage);
    connect(detailDialog,
            &DayCountFractionTypeDetailDialog::typeSaved,
            this,
            [self = QPointer<DayCountFractionTypeController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Day Count Fraction Type reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Day Count Fraction Type '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Day Count Fraction Type: %1").arg(QString::fromStdString(type.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* DayCountFractionTypeController::listWindow() const {
    return listWindow_;
}

void DayCountFractionTypeController::notifyOpenDialogs(const QStringList& entityIds) {
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
