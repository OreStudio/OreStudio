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
#include "ores.qt/BondInstrumentController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/BondInstrumentMdiWindow.hpp"
#include "ores.qt/BondInstrumentDetailDialog.hpp"
#include "ores.qt/BondInstrumentHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

BondInstrumentController::BondInstrumentController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "BondInstrumentController created";
}

void BondInstrumentController::showListWindow() {
    const QString key = build_window_key("list", "bond_instruments");
    if (try_reuse_window(key)) return;

    listWindow_ = new BondInstrumentMdiWindow(clientManager_, username_);

    connect(listWindow_, &BondInstrumentMdiWindow::statusChanged,
            this, &BondInstrumentController::statusMessage);
    connect(listWindow_, &BondInstrumentMdiWindow::errorOccurred,
            this, &BondInstrumentController::errorMessage);
    connect(listWindow_, &BondInstrumentMdiWindow::showBondInstrumentDetails,
            this, &BondInstrumentController::onShowDetails);
    connect(listWindow_, &BondInstrumentMdiWindow::addNewRequested,
            this, &BondInstrumentController::onAddNewRequested);
    connect(listWindow_, &BondInstrumentMdiWindow::showBondInstrumentHistory,
            this, &BondInstrumentController::onShowHistory);

    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Bond Instruments");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowTrending, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    connect(listMdiSubWindow_, &QObject::destroyed,
            this, [self = QPointer<BondInstrumentController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });
}

void BondInstrumentController::closeAllWindows() {
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

void BondInstrumentController::reloadListWindow() {
    if (listWindow_) listWindow_->reload();
}

void BondInstrumentController::openEdit(const trading::domain::bond_instrument& v) {
    showDetailWindow(v);
}

void BondInstrumentController::onShowDetails(
    const trading::domain::bond_instrument& v) {
    showDetailWindow(v);
}

void BondInstrumentController::onAddNewRequested() {
    showAddWindow();
}

void BondInstrumentController::onShowHistory(
    const trading::domain::bond_instrument& v) {
    showHistoryWindow(
        QString::fromStdString(boost::uuids::to_string(v.id)));
}

void BondInstrumentController::showAddWindow() {
    auto* detailDialog = new BondInstrumentDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &BondInstrumentDetailDialog::statusMessage,
            this, &BondInstrumentController::statusMessage);
    connect(detailDialog, &BondInstrumentDetailDialog::errorMessage,
            this, &BondInstrumentController::errorMessage);
    connect(detailDialog, &BondInstrumentDetailDialog::bondInstrumentSaved,
            this, [self = QPointer<BondInstrumentController>(this)](
                const QString& /*id*/) {
        if (!self) return;
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Bond Instrument");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowTrending, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);
    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void BondInstrumentController::showDetailWindow(
    const trading::domain::bond_instrument& v) {

    const QString identifier =
        QString::fromStdString(boost::uuids::to_string(v.id));
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) return;

    auto* detailDialog = new BondInstrumentDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setBondInstrument(v);

    connect(detailDialog, &BondInstrumentDetailDialog::statusMessage,
            this, &BondInstrumentController::statusMessage);
    connect(detailDialog, &BondInstrumentDetailDialog::errorMessage,
            this, &BondInstrumentController::errorMessage);
    connect(detailDialog, &BondInstrumentDetailDialog::bondInstrumentSaved,
            this, [self = QPointer<BondInstrumentController>(this)](
                const QString& /*id*/) {
        if (!self) return;
        self->handleEntitySaved();
    });
    connect(detailDialog, &BondInstrumentDetailDialog::bondInstrumentDeleted,
            this, [self = QPointer<BondInstrumentController>(this), key](
                const QString& /*id*/) {
        if (!self) return;
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Bond Instrument: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowTrending, IconUtils::DefaultIconColor));

    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<BondInstrumentController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() { if (self) self->untrack_window(key); });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void BondInstrumentController::showHistoryWindow(const QString& id) {
    const QString windowKey = build_window_key("history", id);
    if (try_reuse_window(windowKey)) return;

    auto* historyDialog =
        new BondInstrumentHistoryDialog(id, clientManager_, mainWindow_);

    connect(historyDialog, &BondInstrumentHistoryDialog::statusChanged,
            this, [self = QPointer<BondInstrumentController>(this)](
                const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &BondInstrumentHistoryDialog::errorOccurred,
            this, [self = QPointer<BondInstrumentController>(this)](
                const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &BondInstrumentHistoryDialog::revertVersionRequested,
            this, &BondInstrumentController::onRevertVersion);
    connect(historyDialog, &BondInstrumentHistoryDialog::openVersionRequested,
            this, &BondInstrumentController::onOpenVersion);

    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(
        QString("Bond Instrument History: %1").arg(id));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<BondInstrumentController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() { if (self) self->untrack_window(windowKey); });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void BondInstrumentController::onOpenVersion(
    const trading::domain::bond_instrument& v, int versionNumber) {
    const QString id =
        QString::fromStdString(boost::uuids::to_string(v.id));
    const QString windowKey = build_window_key("version",
        QString("%1_v%2").arg(id).arg(versionNumber));

    if (try_reuse_window(windowKey)) return;

    auto* detailDialog = new BondInstrumentDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setBondInstrument(v);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &BondInstrumentDetailDialog::statusMessage,
            this, [self = QPointer<BondInstrumentController>(this)](
                const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &BondInstrumentDetailDialog::errorMessage,
            this, [self = QPointer<BondInstrumentController>(this)](
                const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Bond Instrument: %1 (Version %2)").arg(id).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<BondInstrumentController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() { if (self) self->untrack_window(windowKey); });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void BondInstrumentController::onRevertVersion(
    const trading::domain::bond_instrument& v) {
    auto* detailDialog = new BondInstrumentDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setBondInstrument(v);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &BondInstrumentDetailDialog::statusMessage,
            this, &BondInstrumentController::statusMessage);
    connect(detailDialog, &BondInstrumentDetailDialog::errorMessage,
            this, &BondInstrumentController::errorMessage);
    connect(detailDialog, &BondInstrumentDetailDialog::bondInstrumentSaved,
            this, [self = QPointer<BondInstrumentController>(this)](
                const QString& id) {
        if (!self) return;
        emit self->statusMessage(
            QString("Bond instrument '%1' reverted successfully").arg(id));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Bond Instrument: %1")
            .arg(QString::fromStdString(boost::uuids::to_string(v.id))));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);
    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* BondInstrumentController::listWindow() const {
    return listWindow_;
}

}
