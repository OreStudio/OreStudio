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
#include "ores.qt/PartyIdSchemeController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/PartyIdSchemeMdiWindow.hpp"
#include "ores.qt/PartyIdSchemeDetailDialog.hpp"
#include "ores.qt/PartyIdSchemeHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

PartyIdSchemeController::PartyIdSchemeController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "PartyIdSchemeController created";
}

void PartyIdSchemeController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "party_id_schemes");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new PartyIdSchemeMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &PartyIdSchemeMdiWindow::statusChanged,
            this, &PartyIdSchemeController::statusMessage);
    connect(listWindow_, &PartyIdSchemeMdiWindow::errorOccurred,
            this, &PartyIdSchemeController::errorMessage);
    connect(listWindow_, &PartyIdSchemeMdiWindow::showSchemeDetails,
            this, &PartyIdSchemeController::onShowDetails);
    connect(listWindow_, &PartyIdSchemeMdiWindow::addNewRequested,
            this, &PartyIdSchemeController::onAddNewRequested);
    connect(listWindow_, &PartyIdSchemeMdiWindow::showSchemeHistory,
            this, &PartyIdSchemeController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Party ID Schemes");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Key, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<PartyIdSchemeController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Party ID Scheme list window created";
}

void PartyIdSchemeController::closeAllWindows() {
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

void PartyIdSchemeController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void PartyIdSchemeController::onShowDetails(
    const refdata::domain::party_id_scheme& scheme) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << scheme.code;
    showDetailWindow(scheme);
}

void PartyIdSchemeController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new party ID scheme requested";
    showAddWindow();
}

void PartyIdSchemeController::onShowHistory(
    const refdata::domain::party_id_scheme& scheme) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << scheme.code;
    showHistoryWindow(QString::fromStdString(scheme.code));
}

void PartyIdSchemeController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new party ID scheme";

    auto* detailDialog = new PartyIdSchemeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &PartyIdSchemeDetailDialog::statusMessage,
            this, &PartyIdSchemeController::statusMessage);
    connect(detailDialog, &PartyIdSchemeDetailDialog::errorMessage,
            this, &PartyIdSchemeController::errorMessage);
    connect(detailDialog, &PartyIdSchemeDetailDialog::schemeSaved,
            this, [self = QPointer<PartyIdSchemeController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Party ID Scheme saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Party ID Scheme");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Key, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PartyIdSchemeController::showDetailWindow(
    const refdata::domain::party_id_scheme& scheme) {

    const QString identifier = QString::fromStdString(scheme.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << scheme.code;

    auto* detailDialog = new PartyIdSchemeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setScheme(scheme);

    connect(detailDialog, &PartyIdSchemeDetailDialog::statusMessage,
            this, &PartyIdSchemeController::statusMessage);
    connect(detailDialog, &PartyIdSchemeDetailDialog::errorMessage,
            this, &PartyIdSchemeController::errorMessage);
    connect(detailDialog, &PartyIdSchemeDetailDialog::schemeSaved,
            this, [self = QPointer<PartyIdSchemeController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Party ID Scheme saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &PartyIdSchemeDetailDialog::schemeDeleted,
            this, [self = QPointer<PartyIdSchemeController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Party ID Scheme deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Party ID Scheme: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Key, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<PartyIdSchemeController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PartyIdSchemeController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for party ID scheme: "
                              << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << code.toStdString();

    auto* historyDialog = new PartyIdSchemeHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog, &PartyIdSchemeHistoryDialog::statusChanged,
            this, [self = QPointer<PartyIdSchemeController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &PartyIdSchemeHistoryDialog::errorOccurred,
            this, [self = QPointer<PartyIdSchemeController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &PartyIdSchemeHistoryDialog::revertVersionRequested,
            this, &PartyIdSchemeController::onRevertVersion);
    connect(historyDialog, &PartyIdSchemeHistoryDialog::openVersionRequested,
            this, &PartyIdSchemeController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Party ID Scheme History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<PartyIdSchemeController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void PartyIdSchemeController::onOpenVersion(
    const refdata::domain::party_id_scheme& scheme, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for party ID scheme: " << scheme.code;

    const QString code = QString::fromStdString(scheme.code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new PartyIdSchemeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setScheme(scheme);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &PartyIdSchemeDetailDialog::statusMessage,
            this, [self = QPointer<PartyIdSchemeController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &PartyIdSchemeDetailDialog::errorMessage,
            this, [self = QPointer<PartyIdSchemeController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Party ID Scheme: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<PartyIdSchemeController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void PartyIdSchemeController::onRevertVersion(
    const refdata::domain::party_id_scheme& scheme) {
    BOOST_LOG_SEV(lg(), info) << "Reverting party ID scheme to version: "
                              << scheme.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new PartyIdSchemeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setScheme(scheme);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &PartyIdSchemeDetailDialog::statusMessage,
            this, &PartyIdSchemeController::statusMessage);
    connect(detailDialog, &PartyIdSchemeDetailDialog::errorMessage,
            this, &PartyIdSchemeController::errorMessage);
    connect(detailDialog, &PartyIdSchemeDetailDialog::schemeSaved,
            this, [self = QPointer<PartyIdSchemeController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Party ID Scheme reverted: " << code.toStdString();
        emit self->statusMessage(QString("Party ID Scheme '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Party ID Scheme: %1")
        .arg(QString::fromStdString(scheme.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* PartyIdSchemeController::listWindow() const {
    return listWindow_;
}

}
