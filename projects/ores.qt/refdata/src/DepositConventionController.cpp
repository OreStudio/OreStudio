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
#include "ores.qt/DepositConventionController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/DepositConventionDetailDialog.hpp"
#include "ores.qt/DepositConventionMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/HistoryDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/deposit_convention_changed_event.hpp"
#include "ores.refdata.api/messaging/deposit_convention_protocol.hpp"
#include <QFutureWatcher>
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <QtConcurrent>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view dc_event_name =
    eventing::domain::event_traits<refdata::eventing::deposit_convention_changed_event>::name;
}

DepositConventionController::DepositConventionController(QMainWindow* mainWindow,
                                                         QMdiArea* mdiArea,
                                                         ClientManager* clientManager,
                                                         const QString& username,
                                                         QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, dc_event_name, parent)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "DepositConventionController created";
}

void DepositConventionController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "deposit_conventions");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new DepositConventionMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &DepositConventionMdiWindow::statusChanged,
            this,
            &DepositConventionController::statusMessage);
    connect(listWindow_,
            &DepositConventionMdiWindow::errorOccurred,
            this,
            &DepositConventionController::errorMessage);
    connect(listWindow_,
            &DepositConventionMdiWindow::showConventionDetails,
            this,
            &DepositConventionController::onShowDetails);
    connect(listWindow_,
            &DepositConventionMdiWindow::addNewRequested,
            this,
            &DepositConventionController::onAddNewRequested);
    connect(listWindow_,
            &DepositConventionMdiWindow::showConventionHistory,
            this,
            &DepositConventionController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Deposit Conventions");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));
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
            [self = QPointer<DepositConventionController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Deposit Convention list window created";
}

void DepositConventionController::closeAllWindows() {
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

void DepositConventionController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void DepositConventionController::onShowDetails(const refdata::domain::deposit_convention& dc) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << dc.id;
    showDetailWindow(dc);
}

void DepositConventionController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new deposit convention requested";
    showAddWindow();
}


void DepositConventionController::onShowHistory(const refdata::domain::deposit_convention& dc) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << dc.id;
    showHistoryWindow(QString::fromStdString(dc.id));
}

void DepositConventionController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new deposit convention";

    auto* detailDialog = new DepositConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &DepositConventionDetailDialog::statusMessage,
            this,
            &DepositConventionController::statusMessage);
    connect(detailDialog,
            &DepositConventionDetailDialog::errorMessage,
            this,
            &DepositConventionController::errorMessage);
    connect(detailDialog,
            &DepositConventionDetailDialog::dcSaved,
            this,
            [self = QPointer<DepositConventionController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Deposit Convention saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Deposit Convention");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void DepositConventionController::showDetailWindow(const refdata::domain::deposit_convention& dc) {

    const QString identifier = QString::fromStdString(dc.id);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << dc.id;

    auto* detailDialog = new DepositConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setConvention(dc);

    connect(detailDialog,
            &DepositConventionDetailDialog::statusMessage,
            this,
            &DepositConventionController::statusMessage);
    connect(detailDialog,
            &DepositConventionDetailDialog::errorMessage,
            this,
            &DepositConventionController::errorMessage);
    connect(detailDialog,
            &DepositConventionDetailDialog::dcSaved,
            this,
            [self = QPointer<DepositConventionController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Deposit Convention saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &DepositConventionDetailDialog::dcDeleted,
            this,
            [self = QPointer<DepositConventionController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Deposit Convention deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Deposit Convention: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<DepositConventionController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void DepositConventionController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for deposit convention: "
                              << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog =
        new HistoryDialog(std::string(entity_type_of(refdata::domain::deposit_convention{})),
                          code.toStdString(),
                          clientManager_,
                          mainWindow_);

    connect(historyDialog,
            &HistoryDialog::statusChanged,
            this,
            [self = QPointer<DepositConventionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::errorOccurred,
            this,
            [self = QPointer<DepositConventionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::revertVersionRequested,
            this,
            [self = QPointer<DepositConventionController>(this)](
                const QString& /*entityType*/, const QString& entityId, int version) {
                if (!self)
                    return;
                self->onRevertHistoryVersion(entityId, version);
            });
    connect(historyDialog,
            &HistoryDialog::openVersionRequested,
            this,
            [self = QPointer<DepositConventionController>(this)](
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
    historyWindow->setWindowTitle(QString("Deposit Convention History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<DepositConventionController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void DepositConventionController::onOpenVersion(const refdata::domain::deposit_convention& dc,
                                                int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for deposit convention: " << dc.id;

    const QString code = QString::fromStdString(dc.id);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new DepositConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setConvention(dc);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &DepositConventionDetailDialog::statusMessage,
            this,
            [self = QPointer<DepositConventionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &DepositConventionDetailDialog::errorMessage,
            this,
            [self = QPointer<DepositConventionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Deposit Convention: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<DepositConventionController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void DepositConventionController::fetchDepositConventionHistory(
    const QString& entityId,
    std::function<void(std::expected<std::vector<refdata::domain::deposit_convention>, QString>)>
        callback) {
    refdata::messaging::get_deposit_convention_history_request request;
    request.id = entityId.toStdString();

    using FetchResult = std::expected<std::vector<refdata::domain::deposit_convention>, QString>;

    QPointer<DepositConventionController> self = this;
    QPointer<ClientManager> clientManager = clientManager_;
    auto future = QtConcurrent::run([clientManager, request = std::move(request)]() -> FetchResult {
        if (!clientManager || !clientManager->isConnected())
            return std::unexpected(QString("Not connected to server"));
        auto result = clientManager->process_authenticated_request(std::move(request));
        if (!result)
            return std::unexpected(QString::fromStdString(result.error()));
        if (!result->success)
            return std::unexpected(QString::fromStdString(result->message));
        return std::move(result->deposit_conventions);
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

void DepositConventionController::onOpenHistoryVersion(const QString& entityId, int versionNumber) {
    QPointer<DepositConventionController> self = this;
    fetchDepositConventionHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::deposit_convention>, QString> result) {
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

void DepositConventionController::onRevertHistoryVersion(const QString& entityId,
                                                         int versionNumber) {
    QPointer<DepositConventionController> self = this;
    fetchDepositConventionHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::deposit_convention>, QString> result) {
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

void DepositConventionController::onRevertVersion(const refdata::domain::deposit_convention& dc) {
    BOOST_LOG_SEV(lg(), info) << "Reverting deposit convention to version: " << dc.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new DepositConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_dc = dc;
    reverted_dc.version = 0;
    detailDialog->setConvention(reverted_dc);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &DepositConventionDetailDialog::statusMessage,
            this,
            &DepositConventionController::statusMessage);
    connect(detailDialog,
            &DepositConventionDetailDialog::errorMessage,
            this,
            &DepositConventionController::errorMessage);
    connect(detailDialog,
            &DepositConventionDetailDialog::dcSaved,
            this,
            [self = QPointer<DepositConventionController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Deposit Convention reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Deposit Convention '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Deposit Convention: %1").arg(QString::fromStdString(dc.id)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* DepositConventionController::listWindow() const {
    return listWindow_;
}

void DepositConventionController::notifyOpenDialogs(const QStringList& entityIds) {
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
