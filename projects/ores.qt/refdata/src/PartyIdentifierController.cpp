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
#include "ores.qt/PartyIdentifierController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/PartyIdentifierDetailDialog.hpp"
#include "ores.qt/PartyIdentifierHistoryDialog.hpp"
#include "ores.qt/PartyIdentifierMdiWindow.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/party_identifier_changed_event.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view partyIdentifier_event_name =
    eventing::domain::event_traits<refdata::eventing::party_identifier_changed_event>::name;
}

PartyIdentifierController::PartyIdentifierController(QMainWindow* mainWindow,
                                                     QMdiArea* mdiArea,
                                                     ClientManager* clientManager,
                                                     ChangeReasonCache* changeReasonCache,
                                                     const QString& username,
                                                     QObject* parent)
    : EntityController(
          mainWindow, mdiArea, clientManager, username, partyIdentifier_event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "PartyIdentifierController created";
}

void PartyIdentifierController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "party_identifiers");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new PartyIdentifierMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &PartyIdentifierMdiWindow::statusChanged,
            this,
            &PartyIdentifierController::statusMessage);
    connect(listWindow_,
            &PartyIdentifierMdiWindow::errorOccurred,
            this,
            &PartyIdentifierController::errorMessage);
    connect(listWindow_,
            &PartyIdentifierMdiWindow::showIdentifierDetails,
            this,
            &PartyIdentifierController::onShowDetails);
    connect(listWindow_,
            &PartyIdentifierMdiWindow::addNewRequested,
            this,
            &PartyIdentifierController::onAddNewRequested);
    connect(listWindow_,
            &PartyIdentifierMdiWindow::showIdentifierHistory,
            this,
            &PartyIdentifierController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Party Identifiers");
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
            [self = QPointer<PartyIdentifierController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Party Identifier list window created";
}

void PartyIdentifierController::closeAllWindows() {
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

void PartyIdentifierController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void PartyIdentifierController::onShowDetails(
    const refdata::domain::party_identifier& partyIdentifier) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << partyIdentifier.id_value;
    showDetailWindow(partyIdentifier);
}

void PartyIdentifierController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new party identifier requested";
    showAddWindow();
}


void PartyIdentifierController::onShowHistory(
    const refdata::domain::party_identifier& partyIdentifier) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << partyIdentifier.id_value;
    showHistoryWindow(partyIdentifier);
}

void PartyIdentifierController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new party identifier";

    auto* detailDialog = new PartyIdentifierDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &PartyIdentifierDetailDialog::statusMessage,
            this,
            &PartyIdentifierController::statusMessage);
    connect(detailDialog,
            &PartyIdentifierDetailDialog::errorMessage,
            this,
            &PartyIdentifierController::errorMessage);
    connect(detailDialog,
            &PartyIdentifierDetailDialog::partyIdentifierSaved,
            this,
            [self = QPointer<PartyIdentifierController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Party Identifier saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Party Identifier");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Key, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PartyIdentifierController::showDetailWindow(
    const refdata::domain::party_identifier& partyIdentifier) {

    const QString identifier = QString::fromStdString(partyIdentifier.id_value);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << partyIdentifier.id_value;

    auto* detailDialog = new PartyIdentifierDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setIdentifier(partyIdentifier);

    connect(detailDialog,
            &PartyIdentifierDetailDialog::statusMessage,
            this,
            &PartyIdentifierController::statusMessage);
    connect(detailDialog,
            &PartyIdentifierDetailDialog::errorMessage,
            this,
            &PartyIdentifierController::errorMessage);
    connect(detailDialog,
            &PartyIdentifierDetailDialog::partyIdentifierSaved,
            this,
            [self = QPointer<PartyIdentifierController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Party Identifier saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &PartyIdentifierDetailDialog::partyIdentifierDeleted,
            this,
            [self = QPointer<PartyIdentifierController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Party Identifier deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Party Identifier: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Key, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<PartyIdentifierController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PartyIdentifierController::showHistoryWindow(
    const refdata::domain::party_identifier& partyIdentifier) {
    const QString code = QString::fromStdString(partyIdentifier.id_value);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for party identifier: "
                              << partyIdentifier.id_value;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << partyIdentifier.id_value;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << partyIdentifier.id_value;

    auto* historyDialog =
        new PartyIdentifierHistoryDialog(partyIdentifier.id, code, clientManager_, mainWindow_);

    connect(historyDialog,
            &PartyIdentifierHistoryDialog::statusChanged,
            this,
            [self = QPointer<PartyIdentifierController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &PartyIdentifierHistoryDialog::errorOccurred,
            this,
            [self = QPointer<PartyIdentifierController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &PartyIdentifierHistoryDialog::revertVersionRequested,
            this,
            &PartyIdentifierController::onRevertVersion);
    connect(historyDialog,
            &PartyIdentifierHistoryDialog::openVersionRequested,
            this,
            &PartyIdentifierController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Party Identifier History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<PartyIdentifierController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void PartyIdentifierController::onOpenVersion(
    const refdata::domain::party_identifier& partyIdentifier, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for party identifier: " << partyIdentifier.id_value;

    const QString code = QString::fromStdString(partyIdentifier.id_value);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new PartyIdentifierDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setIdentifier(partyIdentifier);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &PartyIdentifierDetailDialog::statusMessage,
            this,
            [self = QPointer<PartyIdentifierController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &PartyIdentifierDetailDialog::errorMessage,
            this,
            [self = QPointer<PartyIdentifierController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Party Identifier: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<PartyIdentifierController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void PartyIdentifierController::onRevertVersion(
    const refdata::domain::party_identifier& partyIdentifier) {
    BOOST_LOG_SEV(lg(), info) << "Reverting party identifier to version: "
                              << partyIdentifier.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new PartyIdentifierDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_partyIdentifier = partyIdentifier;
    reverted_partyIdentifier.version = 0;
    detailDialog->setIdentifier(reverted_partyIdentifier);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &PartyIdentifierDetailDialog::statusMessage,
            this,
            &PartyIdentifierController::statusMessage);
    connect(detailDialog,
            &PartyIdentifierDetailDialog::errorMessage,
            this,
            &PartyIdentifierController::errorMessage);
    connect(detailDialog,
            &PartyIdentifierDetailDialog::partyIdentifierSaved,
            this,
            [self = QPointer<PartyIdentifierController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Party Identifier reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Party Identifier '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Party Identifier: %1")
                                     .arg(QString::fromStdString(partyIdentifier.id_value)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* PartyIdentifierController::listWindow() const {
    return listWindow_;
}

void PartyIdentifierController::notifyOpenDialogs(const QStringList& entityIds) {
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
