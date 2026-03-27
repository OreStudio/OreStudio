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
#include "ores.qt/InstrumentController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/InstrumentMdiWindow.hpp"
#include "ores.qt/InstrumentDetailDialog.hpp"
#include "ores.qt/InstrumentHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

InstrumentController::InstrumentController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "InstrumentController created";
}

void InstrumentController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "instruments");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new InstrumentMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &InstrumentMdiWindow::statusChanged,
            this, &InstrumentController::statusMessage);
    connect(listWindow_, &InstrumentMdiWindow::errorOccurred,
            this, &InstrumentController::errorMessage);
    connect(listWindow_, &InstrumentMdiWindow::showInstrumentDetails,
            this, &InstrumentController::onShowDetails);
    connect(listWindow_, &InstrumentMdiWindow::addNewRequested,
            this, &InstrumentController::onAddNewRequested);
    connect(listWindow_, &InstrumentMdiWindow::showInstrumentHistory,
            this, &InstrumentController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Instruments");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Tag, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed,
            this, [self = QPointer<InstrumentController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Instrument list window created";
}

void InstrumentController::closeAllWindows() {
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

void InstrumentController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void InstrumentController::onShowDetails(
    const trading::domain::instrument& v) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: "
                               << boost::uuids::to_string(v.id);
    showDetailWindow(v);
}

void InstrumentController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new instrument requested";
    showAddWindow();
}

void InstrumentController::onShowHistory(
    const trading::domain::instrument& v) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: "
                               << boost::uuids::to_string(v.id);
    showHistoryWindow(
        QString::fromStdString(boost::uuids::to_string(v.id)));
}

void InstrumentController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new instrument";

    auto* detailDialog = new InstrumentDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &InstrumentDetailDialog::statusMessage,
            this, &InstrumentController::statusMessage);
    connect(detailDialog, &InstrumentDetailDialog::errorMessage,
            this, &InstrumentController::errorMessage);
    connect(detailDialog, &InstrumentDetailDialog::instrumentSaved,
            this, [self = QPointer<InstrumentController>(this)](const QString& id) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Instrument saved: " << id.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Instrument");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Tag, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void InstrumentController::showDetailWindow(
    const trading::domain::instrument& v) {

    const QString identifier =
        QString::fromStdString(boost::uuids::to_string(v.id));
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: "
                               << boost::uuids::to_string(v.id);

    auto* detailDialog = new InstrumentDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setInstrument(v);

    connect(detailDialog, &InstrumentDetailDialog::statusMessage,
            this, &InstrumentController::statusMessage);
    connect(detailDialog, &InstrumentDetailDialog::errorMessage,
            this, &InstrumentController::errorMessage);
    connect(detailDialog, &InstrumentDetailDialog::instrumentSaved,
            this, [self = QPointer<InstrumentController>(this)](const QString& id) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Instrument saved: " << id.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &InstrumentDetailDialog::instrumentDeleted,
            this, [self = QPointer<InstrumentController>(this),
                   key](const QString& id) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Instrument deleted: " << id.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Instrument: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Tag, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<InstrumentController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void InstrumentController::showHistoryWindow(const QString& id) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for instrument: "
                              << id.toStdString();

    const QString windowKey = build_window_key("history", id);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << id.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << id.toStdString();

    auto* historyDialog =
        new InstrumentHistoryDialog(id, clientManager_, mainWindow_);

    connect(historyDialog, &InstrumentHistoryDialog::statusChanged,
            this, [self = QPointer<InstrumentController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &InstrumentHistoryDialog::errorOccurred,
            this, [self = QPointer<InstrumentController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &InstrumentHistoryDialog::revertVersionRequested,
            this, &InstrumentController::onRevertVersion);
    connect(historyDialog, &InstrumentHistoryDialog::openVersionRequested,
            this, &InstrumentController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(
        QString("Instrument History: %1").arg(id));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<InstrumentController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void InstrumentController::onOpenVersion(
    const trading::domain::instrument& v, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for instrument: "
                              << boost::uuids::to_string(v.id);

    const QString id = QString::fromStdString(boost::uuids::to_string(v.id));
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(id).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new InstrumentDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setInstrument(v);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &InstrumentDetailDialog::statusMessage,
            this, [self = QPointer<InstrumentController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &InstrumentDetailDialog::errorMessage,
            this, [self = QPointer<InstrumentController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Instrument: %1 (Version %2)").arg(id).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<InstrumentController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void InstrumentController::onRevertVersion(
    const trading::domain::instrument& v) {
    BOOST_LOG_SEV(lg(), info) << "Reverting instrument to version: "
                              << v.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new InstrumentDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setInstrument(v);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &InstrumentDetailDialog::statusMessage,
            this, &InstrumentController::statusMessage);
    connect(detailDialog, &InstrumentDetailDialog::errorMessage,
            this, &InstrumentController::errorMessage);
    connect(detailDialog, &InstrumentDetailDialog::instrumentSaved,
            this, [self = QPointer<InstrumentController>(this)](const QString& id) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Instrument reverted: " << id.toStdString();
        emit self->statusMessage(
            QString("Instrument '%1' reverted successfully").arg(id));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Instrument: %1")
            .arg(QString::fromStdString(boost::uuids::to_string(v.id))));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* InstrumentController::listWindow() const {
    return listWindow_;
}

}
