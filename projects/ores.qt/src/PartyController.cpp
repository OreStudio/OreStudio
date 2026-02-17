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
#include "ores.qt/PartyController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/PartyMdiWindow.hpp"
#include "ores.qt/EntityDetailDialog.hpp"
#include "ores.qt/PartyDetailOperations.hpp"
#include "ores.qt/PartyHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    auto make_party_ops() {
        return std::make_shared<party_detail_operations>();
    }
}

PartyController::PartyController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    ImageCache* imageCache,
    ChangeReasonCache* changeReasonCache,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      imageCache_(imageCache),
      changeReasonCache_(changeReasonCache),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "PartyController created";
}

void PartyController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "parties");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new PartyMdiWindow(clientManager_, imageCache_, username_);

    // Connect signals
    connect(listWindow_, &PartyMdiWindow::statusChanged,
            this, &PartyController::statusMessage);
    connect(listWindow_, &PartyMdiWindow::errorOccurred,
            this, &PartyController::errorMessage);
    connect(listWindow_, &PartyMdiWindow::showPartyDetails,
            this, &PartyController::onShowDetails);
    connect(listWindow_, &PartyMdiWindow::addNewRequested,
            this, &PartyController::onAddNewRequested);
    connect(listWindow_, &PartyMdiWindow::showPartyHistory,
            this, &PartyController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Parties");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Organization, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<PartyController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Party list window created";
}

void PartyController::closeAllWindows() {
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

void PartyController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void PartyController::onShowDetails(
    const refdata::domain::party& party) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << party.short_code;
    showDetailWindow(party);
}

void PartyController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new party requested";
    showAddWindow();
}

void PartyController::onShowHistory(
    const refdata::domain::party& party) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << party.short_code;
    showHistoryWindow(party);
}

void PartyController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new party";

    auto* detailDialog = new EntityDetailDialog(make_party_ops(), mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setImageCache(imageCache_);
    if (changeReasonCache_) {
        detailDialog->setChangeReasonCache(changeReasonCache_);
    }
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &EntityDetailDialog::statusMessage,
            this, &PartyController::statusMessage);
    connect(detailDialog, &EntityDetailDialog::errorMessage,
            this, &PartyController::errorMessage);
    connect(detailDialog, &EntityDetailDialog::entitySaved,
            this, [self = QPointer<PartyController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Party saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Party");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Organization, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PartyController::showDetailWindow(
    const refdata::domain::party& party) {

    const QString identifier = QString::fromStdString(party.short_code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << party.short_code;

    auto* detailDialog = new EntityDetailDialog(make_party_ops(), mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setImageCache(imageCache_);
    if (changeReasonCache_) {
        detailDialog->setChangeReasonCache(changeReasonCache_);
    }
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setEntityData(to_entity_data(party));

    connect(detailDialog, &EntityDetailDialog::statusMessage,
            this, &PartyController::statusMessage);
    connect(detailDialog, &EntityDetailDialog::errorMessage,
            this, &PartyController::errorMessage);
    connect(detailDialog, &EntityDetailDialog::entitySaved,
            this, [self = QPointer<PartyController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Party saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &EntityDetailDialog::entityDeleted,
            this, [self = QPointer<PartyController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Party deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Party: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Organization, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<PartyController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PartyController::showHistoryWindow(
    const refdata::domain::party& party) {
    const QString code = QString::fromStdString(party.short_code);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for party: "
                              << party.short_code;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << party.short_code;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << party.short_code;

    auto* historyDialog = new PartyHistoryDialog(
        party.id, code, clientManager_, mainWindow_);

    connect(historyDialog, &PartyHistoryDialog::statusChanged,
            this, [self = QPointer<PartyController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &PartyHistoryDialog::errorOccurred,
            this, [self = QPointer<PartyController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &PartyHistoryDialog::revertVersionRequested,
            this, &PartyController::onRevertVersion);
    connect(historyDialog, &PartyHistoryDialog::openVersionRequested,
            this, &PartyController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Party History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<PartyController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void PartyController::onOpenVersion(
    const refdata::domain::party& party, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for party: " << party.short_code;

    const QString code = QString::fromStdString(party.short_code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new EntityDetailDialog(make_party_ops(), mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setImageCache(imageCache_);
    if (changeReasonCache_) {
        detailDialog->setChangeReasonCache(changeReasonCache_);
    }
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setEntityData(to_entity_data(party));
    detailDialog->setReadOnly(true);

    connect(detailDialog, &EntityDetailDialog::statusMessage,
            this, [self = QPointer<PartyController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &EntityDetailDialog::errorMessage,
            this, [self = QPointer<PartyController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Party: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<PartyController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void PartyController::onRevertVersion(
    const refdata::domain::party& party) {
    BOOST_LOG_SEV(lg(), info) << "Reverting party to version: "
                              << party.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new EntityDetailDialog(make_party_ops(), mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setImageCache(imageCache_);
    if (changeReasonCache_) {
        detailDialog->setChangeReasonCache(changeReasonCache_);
    }
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setEntityData(to_entity_data(party));
    detailDialog->setCreateMode(false);

    connect(detailDialog, &EntityDetailDialog::statusMessage,
            this, &PartyController::statusMessage);
    connect(detailDialog, &EntityDetailDialog::errorMessage,
            this, &PartyController::errorMessage);
    connect(detailDialog, &EntityDetailDialog::entitySaved,
            this, [self = QPointer<PartyController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Party reverted: " << code.toStdString();
        emit self->statusMessage(QString("Party '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Party: %1")
        .arg(QString::fromStdString(party.short_code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* PartyController::listWindow() const {
    return listWindow_;
}

}
