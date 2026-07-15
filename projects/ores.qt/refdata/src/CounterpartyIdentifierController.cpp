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
#include "ores.qt/CounterpartyIdentifierController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/CounterpartyIdentifierDetailDialog.hpp"
#include "ores.qt/CounterpartyIdentifierMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/HistoryDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/counterparty_identifier_changed_event.hpp"
#include "ores.refdata.api/messaging/counterparty_identifier_protocol.hpp"
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
constexpr std::string_view counterpartyIdentifier_event_name =
    eventing::domain::event_traits<refdata::eventing::counterparty_identifier_changed_event>::name;
}

CounterpartyIdentifierController::CounterpartyIdentifierController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    ChangeReasonCache* changeReasonCache,
    const QString& username,
    QObject* parent)
    : EntityController(
          mainWindow, mdiArea, clientManager, username, counterpartyIdentifier_event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "CounterpartyIdentifierController created";
}

void CounterpartyIdentifierController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "counterparty_identifiers");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new CounterpartyIdentifierMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &CounterpartyIdentifierMdiWindow::statusChanged,
            this,
            &CounterpartyIdentifierController::statusMessage);
    connect(listWindow_,
            &CounterpartyIdentifierMdiWindow::errorOccurred,
            this,
            &CounterpartyIdentifierController::errorMessage);
    connect(listWindow_,
            &CounterpartyIdentifierMdiWindow::showIdentifierDetails,
            this,
            &CounterpartyIdentifierController::onShowDetails);
    connect(listWindow_,
            &CounterpartyIdentifierMdiWindow::addNewRequested,
            this,
            &CounterpartyIdentifierController::onAddNewRequested);
    connect(listWindow_,
            &CounterpartyIdentifierMdiWindow::showIdentifierHistory,
            this,
            &CounterpartyIdentifierController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Counterparty Identifiers");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Key, IconUtils::DefaultIconColor));
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
            [self = QPointer<CounterpartyIdentifierController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Counterparty Identifier list window created";
}

void CounterpartyIdentifierController::closeAllWindows() {
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

void CounterpartyIdentifierController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void CounterpartyIdentifierController::onShowDetails(
    const refdata::domain::counterparty_identifier& counterpartyIdentifier) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << counterpartyIdentifier.id_value;
    showDetailWindow(counterpartyIdentifier);
}

void CounterpartyIdentifierController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new counterparty identifier requested";
    showAddWindow();
}


void CounterpartyIdentifierController::onShowHistory(
    const refdata::domain::counterparty_identifier& counterpartyIdentifier) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << counterpartyIdentifier.id_value;
    showHistoryWindow(counterpartyIdentifier);
}

void CounterpartyIdentifierController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new counterparty identifier";

    auto* detailDialog = new CounterpartyIdentifierDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &CounterpartyIdentifierDetailDialog::statusMessage,
            this,
            &CounterpartyIdentifierController::statusMessage);
    connect(detailDialog,
            &CounterpartyIdentifierDetailDialog::errorMessage,
            this,
            &CounterpartyIdentifierController::errorMessage);
    connect(detailDialog,
            &CounterpartyIdentifierDetailDialog::counterpartyIdentifierSaved,
            this,
            [self = QPointer<CounterpartyIdentifierController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Counterparty Identifier saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Counterparty Identifier");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Key, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CounterpartyIdentifierController::showDetailWindow(
    const refdata::domain::counterparty_identifier& counterpartyIdentifier) {

    const QString identifier = QString::fromStdString(counterpartyIdentifier.id_value);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << counterpartyIdentifier.id_value;

    auto* detailDialog = new CounterpartyIdentifierDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setIdentifier(counterpartyIdentifier);

    connect(detailDialog,
            &CounterpartyIdentifierDetailDialog::statusMessage,
            this,
            &CounterpartyIdentifierController::statusMessage);
    connect(detailDialog,
            &CounterpartyIdentifierDetailDialog::errorMessage,
            this,
            &CounterpartyIdentifierController::errorMessage);
    connect(detailDialog,
            &CounterpartyIdentifierDetailDialog::counterpartyIdentifierSaved,
            this,
            [self = QPointer<CounterpartyIdentifierController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Counterparty Identifier saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &CounterpartyIdentifierDetailDialog::counterpartyIdentifierDeleted,
            this,
            [self = QPointer<CounterpartyIdentifierController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Counterparty Identifier deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Counterparty Identifier: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Key, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<CounterpartyIdentifierController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CounterpartyIdentifierController::showHistoryWindow(
    const refdata::domain::counterparty_identifier& counterpartyIdentifier) {
    const QString code = QString::fromStdString(counterpartyIdentifier.id_value);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for counterparty identifier: "
                              << counterpartyIdentifier.id_value;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << counterpartyIdentifier.id_value;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << counterpartyIdentifier.id_value;

    const QString entityId =
        QString::fromStdString(boost::uuids::to_string(counterpartyIdentifier.id));
    auto* historyDialog =
        new HistoryDialog(std::string(entity_type_of(refdata::domain::counterparty_identifier{})),
                          entityId.toStdString(),
                          clientManager_,
                          mainWindow_);

    connect(historyDialog,
            &HistoryDialog::statusChanged,
            this,
            [self = QPointer<CounterpartyIdentifierController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::errorOccurred,
            this,
            [self = QPointer<CounterpartyIdentifierController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &HistoryDialog::revertVersionRequested,
            this,
            [self = QPointer<CounterpartyIdentifierController>(this)](
                const QString& /*entityType*/, const QString& entityId, int version) {
                if (!self)
                    return;
                self->onRevertHistoryVersion(entityId, version);
            });
    connect(historyDialog,
            &HistoryDialog::openVersionRequested,
            this,
            [self = QPointer<CounterpartyIdentifierController>(this)](
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
    historyWindow->setWindowTitle(QString("Counterparty Identifier History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<CounterpartyIdentifierController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void CounterpartyIdentifierController::onOpenVersion(
    const refdata::domain::counterparty_identifier& counterpartyIdentifier, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for counterparty identifier: "
                              << counterpartyIdentifier.id_value;

    const QString code = QString::fromStdString(counterpartyIdentifier.id_value);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new CounterpartyIdentifierDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setIdentifier(counterpartyIdentifier);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &CounterpartyIdentifierDetailDialog::statusMessage,
            this,
            [self = QPointer<CounterpartyIdentifierController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &CounterpartyIdentifierDetailDialog::errorMessage,
            this,
            [self = QPointer<CounterpartyIdentifierController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Counterparty Identifier: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CounterpartyIdentifierController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void CounterpartyIdentifierController::fetchCounterpartyIdentifierHistory(
    const QString& entityId,
    std::function<void(
        std::expected<std::vector<refdata::domain::counterparty_identifier>, QString>)> callback) {
    refdata::messaging::get_counterparty_identifier_history_request request;
    request.id = entityId.toStdString();

    using FetchResult =
        std::expected<std::vector<refdata::domain::counterparty_identifier>, QString>;

    QPointer<CounterpartyIdentifierController> self = this;
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

void CounterpartyIdentifierController::onOpenHistoryVersion(const QString& entityId,
                                                            int versionNumber) {
    QPointer<CounterpartyIdentifierController> self = this;
    fetchCounterpartyIdentifierHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::counterparty_identifier>, QString> result) {
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

void CounterpartyIdentifierController::onRevertHistoryVersion(const QString& entityId,
                                                              int versionNumber) {
    QPointer<CounterpartyIdentifierController> self = this;
    fetchCounterpartyIdentifierHistory(
        entityId,
        [self, entityId, versionNumber](
            std::expected<std::vector<refdata::domain::counterparty_identifier>, QString> result) {
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

void CounterpartyIdentifierController::onRevertVersion(
    const refdata::domain::counterparty_identifier& counterpartyIdentifier) {
    BOOST_LOG_SEV(lg(), info) << "Reverting counterparty identifier to version: "
                              << counterpartyIdentifier.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new CounterpartyIdentifierDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_counterpartyIdentifier = counterpartyIdentifier;
    reverted_counterpartyIdentifier.version = 0;
    detailDialog->setIdentifier(reverted_counterpartyIdentifier);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &CounterpartyIdentifierDetailDialog::statusMessage,
            this,
            &CounterpartyIdentifierController::statusMessage);
    connect(detailDialog,
            &CounterpartyIdentifierDetailDialog::errorMessage,
            this,
            &CounterpartyIdentifierController::errorMessage);
    connect(detailDialog,
            &CounterpartyIdentifierDetailDialog::counterpartyIdentifierSaved,
            this,
            [self = QPointer<CounterpartyIdentifierController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Counterparty Identifier reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Counterparty Identifier '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Counterparty Identifier: %1")
                                     .arg(QString::fromStdString(counterpartyIdentifier.id_value)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* CounterpartyIdentifierController::listWindow() const {
    return listWindow_;
}

void CounterpartyIdentifierController::notifyOpenDialogs(const QStringList& entityIds) {
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
