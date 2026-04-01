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
#include "ores.qt/ScriptedInstrumentController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ScriptedInstrumentMdiWindow.hpp"
#include "ores.qt/ScriptedInstrumentDetailDialog.hpp"
#include "ores.qt/ScriptedInstrumentHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

ScriptedInstrumentController::ScriptedInstrumentController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "ScriptedInstrumentController created";
}

void ScriptedInstrumentController::showListWindow() {
    const QString key = build_window_key("list", "scripted_instruments");
    if (try_reuse_window(key)) return;

    listWindow_ = new ScriptedInstrumentMdiWindow(clientManager_, username_);

    connect(listWindow_, &ScriptedInstrumentMdiWindow::statusChanged,
            this, &ScriptedInstrumentController::statusMessage);
    connect(listWindow_, &ScriptedInstrumentMdiWindow::errorOccurred,
            this, &ScriptedInstrumentController::errorMessage);
    connect(listWindow_,
            &ScriptedInstrumentMdiWindow::showScriptedInstrumentDetails,
            this, &ScriptedInstrumentController::onShowDetails);
    connect(listWindow_, &ScriptedInstrumentMdiWindow::addNewRequested,
            this, &ScriptedInstrumentController::onAddNewRequested);
    connect(listWindow_,
            &ScriptedInstrumentMdiWindow::showScriptedInstrumentHistory,
            this, &ScriptedInstrumentController::onShowHistory);

    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Scripted Instruments");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowTrending, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    connect(listMdiSubWindow_, &QObject::destroyed,
            this, [self = QPointer<ScriptedInstrumentController>(this),
                   key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });
}

void ScriptedInstrumentController::closeAllWindows() {
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

void ScriptedInstrumentController::reloadListWindow() {
    if (listWindow_) listWindow_->reload();
}

void ScriptedInstrumentController::openEdit(const trading::domain::scripted_instrument& v) {
    showDetailWindow(v);
}

void ScriptedInstrumentController::onShowDetails(
    const trading::domain::scripted_instrument& v) {
    showDetailWindow(v);
}

void ScriptedInstrumentController::onAddNewRequested() {
    showAddWindow();
}

void ScriptedInstrumentController::onShowHistory(
    const trading::domain::scripted_instrument& v) {
    showHistoryWindow(
        QString::fromStdString(boost::uuids::to_string(v.id)));
}

void ScriptedInstrumentController::showAddWindow() {
    auto* detailDialog =
        new ScriptedInstrumentDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &ScriptedInstrumentDetailDialog::statusMessage,
            this, &ScriptedInstrumentController::statusMessage);
    connect(detailDialog, &ScriptedInstrumentDetailDialog::errorMessage,
            this, &ScriptedInstrumentController::errorMessage);
    connect(detailDialog,
            &ScriptedInstrumentDetailDialog::scriptedInstrumentSaved,
            this, [self = QPointer<ScriptedInstrumentController>(this)](
                const QString& /*id*/) {
        if (!self) return;
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Scripted Instrument");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowTrending, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);
    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void ScriptedInstrumentController::showDetailWindow(
    const trading::domain::scripted_instrument& v) {

    const QString identifier =
        QString::fromStdString(boost::uuids::to_string(v.id));
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) return;

    auto* detailDialog =
        new ScriptedInstrumentDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setScriptedInstrument(v);

    connect(detailDialog, &ScriptedInstrumentDetailDialog::statusMessage,
            this, &ScriptedInstrumentController::statusMessage);
    connect(detailDialog, &ScriptedInstrumentDetailDialog::errorMessage,
            this, &ScriptedInstrumentController::errorMessage);
    connect(detailDialog,
            &ScriptedInstrumentDetailDialog::scriptedInstrumentSaved,
            this, [self = QPointer<ScriptedInstrumentController>(this)](
                const QString& /*id*/) {
        if (!self) return;
        self->handleEntitySaved();
    });
    connect(detailDialog,
            &ScriptedInstrumentDetailDialog::scriptedInstrumentDeleted,
            this, [self = QPointer<ScriptedInstrumentController>(this),
                   key](const QString& /*id*/) {
        if (!self) return;
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Scripted Instrument: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowTrending, IconUtils::DefaultIconColor));

    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<ScriptedInstrumentController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() { if (self) self->untrack_window(key); });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void ScriptedInstrumentController::showHistoryWindow(const QString& id) {
    const QString windowKey = build_window_key("history", id);
    if (try_reuse_window(windowKey)) return;

    auto* historyDialog =
        new ScriptedInstrumentHistoryDialog(id, clientManager_, mainWindow_);

    connect(historyDialog,
            &ScriptedInstrumentHistoryDialog::statusChanged,
            this, [self = QPointer<ScriptedInstrumentController>(this)](
                const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog,
            &ScriptedInstrumentHistoryDialog::errorOccurred,
            this, [self = QPointer<ScriptedInstrumentController>(this)](
                const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog,
            &ScriptedInstrumentHistoryDialog::revertVersionRequested,
            this, &ScriptedInstrumentController::onRevertVersion);
    connect(historyDialog,
            &ScriptedInstrumentHistoryDialog::openVersionRequested,
            this, &ScriptedInstrumentController::onOpenVersion);

    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(
        QString("Scripted Instrument History: %1").arg(id));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<ScriptedInstrumentController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) self->untrack_window(windowKey);
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void ScriptedInstrumentController::onOpenVersion(
    const trading::domain::scripted_instrument& v, int versionNumber) {
    const QString id =
        QString::fromStdString(boost::uuids::to_string(v.id));
    const QString windowKey = build_window_key("version",
        QString("%1_v%2").arg(id).arg(versionNumber));

    if (try_reuse_window(windowKey)) return;

    auto* detailDialog =
        new ScriptedInstrumentDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setScriptedInstrument(v);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &ScriptedInstrumentDetailDialog::statusMessage,
            this, [self = QPointer<ScriptedInstrumentController>(this)](
                const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &ScriptedInstrumentDetailDialog::errorMessage,
            this, [self = QPointer<ScriptedInstrumentController>(this)](
                const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Scripted Instrument: %1 (Version %2)")
            .arg(id).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<ScriptedInstrumentController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) self->untrack_window(windowKey);
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void ScriptedInstrumentController::onRevertVersion(
    const trading::domain::scripted_instrument& v) {
    auto* detailDialog =
        new ScriptedInstrumentDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setScriptedInstrument(v);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &ScriptedInstrumentDetailDialog::statusMessage,
            this, &ScriptedInstrumentController::statusMessage);
    connect(detailDialog, &ScriptedInstrumentDetailDialog::errorMessage,
            this, &ScriptedInstrumentController::errorMessage);
    connect(detailDialog,
            &ScriptedInstrumentDetailDialog::scriptedInstrumentSaved,
            this, [self = QPointer<ScriptedInstrumentController>(this)](
                const QString& id) {
        if (!self) return;
        emit self->statusMessage(
            QString("Scripted instrument '%1' reverted successfully").arg(id));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Scripted Instrument: %1")
            .arg(QString::fromStdString(boost::uuids::to_string(v.id))));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);
    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* ScriptedInstrumentController::listWindow() const {
    return listWindow_;
}

}
