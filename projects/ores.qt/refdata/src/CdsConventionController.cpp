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
#include "ores.qt/CdsConventionController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/CdsConventionDetailDialog.hpp"
#include "ores.qt/CdsConventionMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/HistoryDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/cds_convention_changed_event.hpp"
#include "ores.refdata.api/messaging/cds_convention_protocol.hpp"
#include <QFutureWatcher>
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <QtConcurrent>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view cc_event_name =
    eventing::domain::event_traits<refdata::eventing::cds_convention_changed_event>::name;
}

CdsConventionController::CdsConventionController(QMainWindow* mainWindow,
                                                 QMdiArea* mdiArea,
                                                 ClientManager* clientManager,
                                                 const QString& username,
                                                 QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, cc_event_name, parent)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "CdsConventionController created";
}

void CdsConventionController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "cds_conventions");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new CdsConventionMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &CdsConventionMdiWindow::statusChanged,
            this,
            &CdsConventionController::statusMessage);
    connect(listWindow_,
            &CdsConventionMdiWindow::errorOccurred,
            this,
            &CdsConventionController::errorMessage);
    connect(listWindow_,
            &CdsConventionMdiWindow::showConventionDetails,
            this,
            &CdsConventionController::onShowDetails);
    connect(listWindow_,
            &CdsConventionMdiWindow::addNewRequested,
            this,
            &CdsConventionController::onAddNewRequested);
    connect(listWindow_,
            &CdsConventionMdiWindow::showConventionHistory,
            this,
            &CdsConventionController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("CDS Conventions");
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
            [self = QPointer<CdsConventionController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "CDS Convention list window created";
}

void CdsConventionController::closeAllWindows() {
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

void CdsConventionController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void CdsConventionController::onShowDetails(const refdata::domain::cds_convention& cc) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << cc.id;
    showDetailWindow(cc);
}

void CdsConventionController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new CDS convention requested";
    showAddWindow();
}


void CdsConventionController::onShowHistory(const refdata::domain::cds_convention& cc) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << cc.id;
    showHistoryWindow(QString::fromStdString(cc.id));
}

void CdsConventionController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new CDS convention";

    auto* detailDialog = new CdsConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &CdsConventionDetailDialog::statusMessage,
            this,
            &CdsConventionController::statusMessage);
    connect(detailDialog,
            &CdsConventionDetailDialog::errorMessage,
            this,
            &CdsConventionController::errorMessage);
    connect(detailDialog,
            &CdsConventionDetailDialog::ccSaved,
            this,
            [self = QPointer<CdsConventionController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "CDS Convention saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New CDS Convention");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CdsConventionController::showDetailWindow(const refdata::domain::cds_convention& cc) {

    const QString identifier = QString::fromStdString(cc.id);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << cc.id;

    auto* detailDialog = new CdsConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setConvention(cc);

    connect(detailDialog,
            &CdsConventionDetailDialog::statusMessage,
            this,
            &CdsConventionController::statusMessage);
    connect(detailDialog,
            &CdsConventionDetailDialog::errorMessage,
            this,
            &CdsConventionController::errorMessage);
    connect(detailDialog,
            &CdsConventionDetailDialog::ccSaved,
            this,
            [self = QPointer<CdsConventionController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "CDS Convention saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &CdsConventionDetailDialog::ccDeleted,
            this,
            [self = QPointer<CdsConventionController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "CDS Convention deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("CDS Convention: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<CdsConventionController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CdsConventionController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for CDS convention: "
                              << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog =
        new HistoryDialog(std::string(entity_type_of(refdata::domain::cds_convention{})),
                          code.toStdString(),
                          clientManager_,
                          mainWindow_);

    connect(historyDialog,
            &HistoryDialog::statusChanged,
            this,
            [self = QPointer<CdsConventionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::errorOccurred,
            this,
            [self = QPointer<CdsConventionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::revertVersionRequested,
            this,
            [self = QPointer<CdsConventionController>(this)](
                const QString& /*entityType*/, const QString& entityId, int version) {
                if (!self)
                    return;
                self->onRevertHistoryVersion(entityId, version);
            });
    connect(historyDialog,
            &HistoryDialog::openVersionRequested,
            this,
            [self = QPointer<CdsConventionController>(this)](
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
    historyWindow->setWindowTitle(QString("CDS Convention History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<CdsConventionController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void CdsConventionController::onOpenVersion(const refdata::domain::cds_convention& cc,
                                            int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for CDS convention: " << cc.id;

    const QString code = QString::fromStdString(cc.id);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new CdsConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setConvention(cc);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &CdsConventionDetailDialog::statusMessage,
            this,
            [self = QPointer<CdsConventionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &CdsConventionDetailDialog::errorMessage,
            this,
            [self = QPointer<CdsConventionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("CDS Convention: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CdsConventionController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void CdsConventionController::fetchCdsConventionHistory(
    const QString& entityId,
    std::function<void(std::expected<std::vector<refdata::domain::cds_convention>, QString>)>
        callback) {
    refdata::messaging::get_cds_convention_history_request request;
    request.id = entityId.toStdString();

    using FetchResult = std::expected<std::vector<refdata::domain::cds_convention>, QString>;

    QPointer<CdsConventionController> self = this;
    QPointer<ClientManager> clientManager = clientManager_;
    auto future = QtConcurrent::run([clientManager, request = std::move(request)]() -> FetchResult {
        if (!clientManager || !clientManager->isConnected())
            return std::unexpected(QString("Not connected to server"));
        auto result = clientManager->process_authenticated_request(std::move(request));
        if (!result)
            return std::unexpected(QString::fromStdString(result.error()));
        if (!result->success)
            return std::unexpected(QString::fromStdString(result->message));
        return std::move(result->cds_conventions);
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

void CdsConventionController::onOpenHistoryVersion(const QString& entityId, int versionNumber) {
    QPointer<CdsConventionController> self = this;
    fetchCdsConventionHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::cds_convention>, QString> result) {
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

void CdsConventionController::onRevertHistoryVersion(const QString& entityId, int versionNumber) {
    QPointer<CdsConventionController> self = this;
    fetchCdsConventionHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::cds_convention>, QString> result) {
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

void CdsConventionController::onRevertVersion(const refdata::domain::cds_convention& cc) {
    BOOST_LOG_SEV(lg(), info) << "Reverting CDS convention to version: " << cc.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new CdsConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_cc = cc;
    reverted_cc.version = 0;
    detailDialog->setConvention(reverted_cc);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &CdsConventionDetailDialog::statusMessage,
            this,
            &CdsConventionController::statusMessage);
    connect(detailDialog,
            &CdsConventionDetailDialog::errorMessage,
            this,
            &CdsConventionController::errorMessage);
    connect(detailDialog,
            &CdsConventionDetailDialog::ccSaved,
            this,
            [self = QPointer<CdsConventionController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "CDS Convention reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("CDS Convention '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert CDS Convention: %1").arg(QString::fromStdString(cc.id)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* CdsConventionController::listWindow() const {
    return listWindow_;
}

void CdsConventionController::notifyOpenDialogs(const QStringList& entityIds) {
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
