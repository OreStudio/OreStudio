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
#include "ores.qt/EquityInstrumentController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/EquityInstrumentMdiWindow.hpp"
#include "ores.qt/EquityInstrumentDetailDialog.hpp"
#include "ores.qt/EquityInstrumentHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

EquityInstrumentController::EquityInstrumentController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "EquityInstrumentController created";
}

void EquityInstrumentController::showListWindow() {
    const QString key = build_window_key("list", "equity_instruments");
    if (try_reuse_window(key)) return;

    listWindow_ = new EquityInstrumentMdiWindow(clientManager_, username_);

    connect(listWindow_, &EquityInstrumentMdiWindow::statusChanged,
            this, &EquityInstrumentController::statusMessage);
    connect(listWindow_, &EquityInstrumentMdiWindow::errorOccurred,
            this, &EquityInstrumentController::errorMessage);
    connect(listWindow_, &EquityInstrumentMdiWindow::showEquityInstrumentDetails,
            this, &EquityInstrumentController::onShowDetails);
    connect(listWindow_, &EquityInstrumentMdiWindow::addNewRequested,
            this, &EquityInstrumentController::onAddNewRequested);
    connect(listWindow_, &EquityInstrumentMdiWindow::showEquityInstrumentHistory,
            this, &EquityInstrumentController::onShowHistory);

    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Equity Instruments");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowTrending, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    connect(listMdiSubWindow_, &QObject::destroyed,
            this, [self = QPointer<EquityInstrumentController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });
}

void EquityInstrumentController::closeAllWindows() {
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

void EquityInstrumentController::reloadListWindow() {
    if (listWindow_) listWindow_->reload();
}

void EquityInstrumentController::onShowDetails(
    const trading::domain::equity_instrument& v) {
    showDetailWindow(v);
}

void EquityInstrumentController::onAddNewRequested() {
    showAddWindow();
}

void EquityInstrumentController::onShowHistory(
    const trading::domain::equity_instrument& v) {
    showHistoryWindow(
        QString::fromStdString(boost::uuids::to_string(v.id)));
}

void EquityInstrumentController::showAddWindow() {
    auto* detailDialog = new EquityInstrumentDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &EquityInstrumentDetailDialog::statusMessage,
            this, &EquityInstrumentController::statusMessage);
    connect(detailDialog, &EquityInstrumentDetailDialog::errorMessage,
            this, &EquityInstrumentController::errorMessage);
    connect(detailDialog, &EquityInstrumentDetailDialog::equityInstrumentSaved,
            this, [self = QPointer<EquityInstrumentController>(this)](
                const QString& /*id*/) {
        if (!self) return;
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Equity Instrument");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowTrending, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);
    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void EquityInstrumentController::showDetailWindow(
    const trading::domain::equity_instrument& v) {

    const QString identifier =
        QString::fromStdString(boost::uuids::to_string(v.id));
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) return;

    auto* detailDialog = new EquityInstrumentDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setEquityInstrument(v);

    connect(detailDialog, &EquityInstrumentDetailDialog::statusMessage,
            this, &EquityInstrumentController::statusMessage);
    connect(detailDialog, &EquityInstrumentDetailDialog::errorMessage,
            this, &EquityInstrumentController::errorMessage);
    connect(detailDialog, &EquityInstrumentDetailDialog::equityInstrumentSaved,
            this, [self = QPointer<EquityInstrumentController>(this)](
                const QString& /*id*/) {
        if (!self) return;
        self->handleEntitySaved();
    });
    connect(detailDialog, &EquityInstrumentDetailDialog::equityInstrumentDeleted,
            this, [self = QPointer<EquityInstrumentController>(this), key](
                const QString& /*id*/) {
        if (!self) return;
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Equity Instrument: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowTrending, IconUtils::DefaultIconColor));

    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<EquityInstrumentController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() { if (self) self->untrack_window(key); });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void EquityInstrumentController::showHistoryWindow(const QString& id) {
    const QString windowKey = build_window_key("history", id);
    if (try_reuse_window(windowKey)) return;

    auto* historyDialog =
        new EquityInstrumentHistoryDialog(id, clientManager_, mainWindow_);

    connect(historyDialog, &EquityInstrumentHistoryDialog::statusChanged,
            this, [self = QPointer<EquityInstrumentController>(this)](
                const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &EquityInstrumentHistoryDialog::errorOccurred,
            this, [self = QPointer<EquityInstrumentController>(this)](
                const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &EquityInstrumentHistoryDialog::revertVersionRequested,
            this, &EquityInstrumentController::onRevertVersion);
    connect(historyDialog, &EquityInstrumentHistoryDialog::openVersionRequested,
            this, &EquityInstrumentController::onOpenVersion);

    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(
        QString("Equity Instrument History: %1").arg(id));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<EquityInstrumentController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() { if (self) self->untrack_window(windowKey); });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void EquityInstrumentController::onOpenVersion(
    const trading::domain::equity_instrument& v, int versionNumber) {
    const QString id =
        QString::fromStdString(boost::uuids::to_string(v.id));
    const QString windowKey = build_window_key("version",
        QString("%1_v%2").arg(id).arg(versionNumber));

    if (try_reuse_window(windowKey)) return;

    auto* detailDialog = new EquityInstrumentDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setEquityInstrument(v);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &EquityInstrumentDetailDialog::statusMessage,
            this, [self = QPointer<EquityInstrumentController>(this)](
                const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &EquityInstrumentDetailDialog::errorMessage,
            this, [self = QPointer<EquityInstrumentController>(this)](
                const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Equity Instrument: %1 (Version %2)").arg(id).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<EquityInstrumentController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() { if (self) self->untrack_window(windowKey); });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void EquityInstrumentController::onRevertVersion(
    const trading::domain::equity_instrument& v) {
    auto* detailDialog = new EquityInstrumentDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setEquityInstrument(v);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &EquityInstrumentDetailDialog::statusMessage,
            this, &EquityInstrumentController::statusMessage);
    connect(detailDialog, &EquityInstrumentDetailDialog::errorMessage,
            this, &EquityInstrumentController::errorMessage);
    connect(detailDialog, &EquityInstrumentDetailDialog::equityInstrumentSaved,
            this, [self = QPointer<EquityInstrumentController>(this)](
                const QString& id) {
        if (!self) return;
        emit self->statusMessage(
            QString("Equity instrument '%1' reverted successfully").arg(id));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Equity Instrument: %1")
            .arg(QString::fromStdString(boost::uuids::to_string(v.id))));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);
    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* EquityInstrumentController::listWindow() const {
    return listWindow_;
}

}
