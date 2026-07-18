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
#include "ores.qt/BusinessUnitController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/BusinessUnitDetailDialog.hpp"
#include "ores.qt/BusinessUnitMdiWindow.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/HistoryDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/business_unit_changed_event.hpp"
#include "ores.refdata.api/messaging/business_unit_protocol.hpp"
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
constexpr std::string_view business_unit_event_name =
    eventing::domain::event_traits<refdata::eventing::business_unit_changed_event>::name;
}

BusinessUnitController::BusinessUnitController(QMainWindow* mainWindow,
                                               QMdiArea* mdiArea,
                                               ClientManager* clientManager,
                                               ImageCache* imageCache,
                                               ChangeReasonCache* changeReasonCache,
                                               const QString& username,
                                               QObject* parent)
    : EntityController(
          mainWindow, mdiArea, clientManager, username, business_unit_event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {
    setImageCache(imageCache);

    BOOST_LOG_SEV(lg(), debug) << "BusinessUnitController created";
}

void BusinessUnitController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "business_units");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new BusinessUnitMdiWindow(clientManager_, username_, imageCache_);

    // Connect signals
    connect(listWindow_,
            &BusinessUnitMdiWindow::statusChanged,
            this,
            &BusinessUnitController::statusMessage);
    connect(listWindow_,
            &BusinessUnitMdiWindow::errorOccurred,
            this,
            &BusinessUnitController::errorMessage);
    connect(listWindow_,
            &BusinessUnitMdiWindow::showUnitDetails,
            this,
            &BusinessUnitController::onShowDetails);
    connect(listWindow_,
            &BusinessUnitMdiWindow::addNewRequested,
            this,
            &BusinessUnitController::onAddNewRequested);
    connect(listWindow_,
            &BusinessUnitMdiWindow::showUnitHistory,
            this,
            &BusinessUnitController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Business Units");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Building, IconUtils::DefaultIconColor));
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
            [self = QPointer<BusinessUnitController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Business Unit list window created";
}

void BusinessUnitController::closeAllWindows() {
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

void BusinessUnitController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void BusinessUnitController::onShowDetails(const refdata::domain::business_unit& business_unit) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << business_unit.unit_code;
    showDetailWindow(business_unit);
}

void BusinessUnitController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new business unit requested";
    showAddWindow();
}

void BusinessUnitController::openAdd() {
    showAddWindow();
}
void BusinessUnitController::openEdit(const refdata::domain::business_unit& business_unit) {
    showDetailWindow(business_unit);
}
void BusinessUnitController::openHistory(const refdata::domain::business_unit& business_unit) {
    showHistoryWindow(business_unit);
}

void BusinessUnitController::onShowHistory(const refdata::domain::business_unit& business_unit) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << business_unit.unit_code;
    showHistoryWindow(business_unit);
}

void BusinessUnitController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new business unit";

    auto* detailDialog = new BusinessUnitDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &BusinessUnitDetailDialog::statusMessage,
            this,
            &BusinessUnitController::statusMessage);
    connect(detailDialog,
            &BusinessUnitDetailDialog::errorMessage,
            this,
            &BusinessUnitController::errorMessage);
    connect(detailDialog,
            &BusinessUnitDetailDialog::business_unitSaved,
            this,
            [self = QPointer<BusinessUnitController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Business Unit saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Business Unit");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Building, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void BusinessUnitController::showDetailWindow(const refdata::domain::business_unit& business_unit) {

    const QString identifier = QString::fromStdString(business_unit.unit_code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << business_unit.unit_code;

    auto* detailDialog = new BusinessUnitDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setUnit(business_unit);

    connect(detailDialog,
            &BusinessUnitDetailDialog::statusMessage,
            this,
            &BusinessUnitController::statusMessage);
    connect(detailDialog,
            &BusinessUnitDetailDialog::errorMessage,
            this,
            &BusinessUnitController::errorMessage);
    connect(detailDialog,
            &BusinessUnitDetailDialog::business_unitSaved,
            this,
            [self = QPointer<BusinessUnitController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Business Unit saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &BusinessUnitDetailDialog::business_unitDeleted,
            this,
            [self = QPointer<BusinessUnitController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Business Unit deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Business Unit: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Building, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<BusinessUnitController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void BusinessUnitController::showHistoryWindow(
    const refdata::domain::business_unit& business_unit) {
    const QString code = QString::fromStdString(business_unit.unit_code);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for business unit: "
                              << business_unit.unit_code;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << business_unit.unit_code;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << business_unit.unit_code;

    const QString entityId = QString::fromStdString(boost::uuids::to_string(business_unit.id));
    auto* historyDialog =
        new HistoryDialog(std::string(entity_type_of(refdata::domain::business_unit{})),
                          entityId.toStdString(),
                          clientManager_,
                          mainWindow_);

    connect(historyDialog,
            &HistoryDialog::statusChanged,
            this,
            [self = QPointer<BusinessUnitController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::errorOccurred,
            this,
            [self = QPointer<BusinessUnitController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::revertVersionRequested,
            this,
            [self = QPointer<BusinessUnitController>(this)](
                const QString& /*entityType*/, const QString& entityId, int version) {
                if (!self)
                    return;
                self->onRevertHistoryVersion(entityId, version);
            });
    connect(historyDialog,
            &HistoryDialog::openVersionRequested,
            this,
            [self = QPointer<BusinessUnitController>(this)](
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
    historyWindow->setWindowTitle(QString("Business Unit History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<BusinessUnitController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void BusinessUnitController::onOpenVersion(const refdata::domain::business_unit& business_unit,
                                           int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for business unit: " << business_unit.unit_code;

    const QString code = QString::fromStdString(business_unit.unit_code);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new BusinessUnitDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setUnit(business_unit);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &BusinessUnitDetailDialog::statusMessage,
            this,
            [self = QPointer<BusinessUnitController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &BusinessUnitDetailDialog::errorMessage,
            this,
            [self = QPointer<BusinessUnitController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Business Unit: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<BusinessUnitController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void BusinessUnitController::fetchBusinessUnitHistory(
    const QString& entityId,
    std::function<void(std::expected<std::vector<refdata::domain::business_unit>, QString>)>
        callback) {
    refdata::messaging::get_business_unit_history_request request;
    request.id = entityId.toStdString();

    using FetchResult = std::expected<std::vector<refdata::domain::business_unit>, QString>;

    QPointer<BusinessUnitController> self = this;
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

void BusinessUnitController::onOpenHistoryVersion(const QString& entityId, int versionNumber) {
    QPointer<BusinessUnitController> self = this;
    fetchBusinessUnitHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::business_unit>, QString> result) {
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

void BusinessUnitController::onRevertHistoryVersion(const QString& entityId, int versionNumber) {
    QPointer<BusinessUnitController> self = this;
    fetchBusinessUnitHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::business_unit>, QString> result) {
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

void BusinessUnitController::onRevertVersion(const refdata::domain::business_unit& business_unit) {
    BOOST_LOG_SEV(lg(), info) << "Reverting business unit to version: " << business_unit.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new BusinessUnitDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_business_unit = business_unit;
    reverted_business_unit.version = 0;
    detailDialog->setUnit(reverted_business_unit);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &BusinessUnitDetailDialog::statusMessage,
            this,
            &BusinessUnitController::statusMessage);
    connect(detailDialog,
            &BusinessUnitDetailDialog::errorMessage,
            this,
            &BusinessUnitController::errorMessage);
    connect(detailDialog,
            &BusinessUnitDetailDialog::business_unitSaved,
            this,
            [self = QPointer<BusinessUnitController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Business Unit reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Business Unit '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Business Unit: %1").arg(QString::fromStdString(business_unit.unit_code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* BusinessUnitController::listWindow() const {
    return listWindow_;
}

void BusinessUnitController::notifyOpenDialogs(const QStringList& entityIds) {
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
