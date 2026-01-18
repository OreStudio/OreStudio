/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/OriginDimensionController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/OriginDimensionMdiWindow.hpp"
#include "ores.qt/OriginDimensionDetailDialog.hpp"
#include "ores.qt/OriginDimensionHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

OriginDimensionController::OriginDimensionController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QList<DetachableMdiSubWindow*>& allDetachableWindows,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr),
      allDetachableWindows_(allDetachableWindows) {

    BOOST_LOG_SEV(lg(), debug) << "OriginDimensionController created";
}

OriginDimensionController::~OriginDimensionController() {
    BOOST_LOG_SEV(lg(), debug) << "OriginDimensionController destroyed";
}

void OriginDimensionController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "origin_dimensions");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new OriginDimensionMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &OriginDimensionMdiWindow::statusChanged,
            this, &OriginDimensionController::statusMessage);
    connect(listWindow_, &OriginDimensionMdiWindow::errorOccurred,
            this, &OriginDimensionController::errorMessage);
    connect(listWindow_, &OriginDimensionMdiWindow::showDimensionDetails,
            this, &OriginDimensionController::onShowDetails);
    connect(listWindow_, &OriginDimensionMdiWindow::addNewRequested,
            this, &OriginDimensionController::onAddNewRequested);
    connect(listWindow_, &OriginDimensionMdiWindow::showDimensionHistory,
            this, &OriginDimensionController::onShowHistory);

    // Create MDI subwindow
    const QColor iconColor(220, 220, 220);
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Origin Dimensions");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_database_20_regular.svg", iconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    allDetachableWindows_.append(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [this, key]() {
        untrack_window(key);
        allDetachableWindows_.removeOne(listMdiSubWindow_);
        listWindow_ = nullptr;
        listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Origin dimension list window created";
}

void OriginDimensionController::closeAllWindows() {
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

void OriginDimensionController::onShowDetails(
    const dq::domain::origin_dimension& dimension) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << dimension.code;
    showDetailWindow(dimension);
}

void OriginDimensionController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new origin dimension requested";
    showAddWindow();
}

void OriginDimensionController::onShowHistory(const QString& code) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << code.toStdString();
    showHistoryWindow(code);
}

void OriginDimensionController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new origin dimension";

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new OriginDimensionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &OriginDimensionDetailDialog::statusMessage,
            this, &OriginDimensionController::statusMessage);
    connect(detailDialog, &OriginDimensionDetailDialog::errorMessage,
            this, &OriginDimensionController::errorMessage);
    connect(detailDialog, &OriginDimensionDetailDialog::dimensionSaved,
            this, [this](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Origin dimension saved: " << code.toStdString();
        if (listWindow_) {
            listWindow_->reload();
        }
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Origin Dimension");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_database_20_regular.svg", iconColor));

    allDetachableWindows_.append(detailWindow);
    QPointer<OriginDimensionController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, detailWindow]() {
        if (self)
            self->allDetachableWindows_.removeAll(detailWindow);
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void OriginDimensionController::showDetailWindow(
    const dq::domain::origin_dimension& dimension) {

    const QString identifier = QString::fromStdString(dimension.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << dimension.code;

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new OriginDimensionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setDimension(dimension);

    connect(detailDialog, &OriginDimensionDetailDialog::statusMessage,
            this, &OriginDimensionController::statusMessage);
    connect(detailDialog, &OriginDimensionDetailDialog::errorMessage,
            this, &OriginDimensionController::errorMessage);
    connect(detailDialog, &OriginDimensionDetailDialog::dimensionSaved,
            this, [this](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Origin dimension saved: " << code.toStdString();
        if (listWindow_) {
            listWindow_->reload();
        }
    });
    connect(detailDialog, &OriginDimensionDetailDialog::dimensionDeleted,
            this, [this, key](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Origin dimension deleted: " << code.toStdString();
        if (listWindow_) {
            listWindow_->reload();
        }
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Origin Dimension: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_database_20_regular.svg", iconColor));

    // Track window
    track_window(key, detailWindow);
    allDetachableWindows_.append(detailWindow);

    QPointer<OriginDimensionController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, detailWindow, key]() {
        if (self) {
            self->untrack_window(key);
            self->allDetachableWindows_.removeAll(detailWindow);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void OriginDimensionController::onNotificationReceived(
    const QString& eventType, const QDateTime& timestamp,
    const QStringList& entityIds) {
    Q_UNUSED(eventType);
    Q_UNUSED(timestamp);
    Q_UNUSED(entityIds);
    // Event handling can be added later if needed
}

void OriginDimensionController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for origin dimension: "
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
    const QColor iconColor(220, 220, 220);

    auto* historyDialog = new OriginDimensionHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog, &OriginDimensionHistoryDialog::statusChanged,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(historyDialog, &OriginDimensionHistoryDialog::errorOccurred,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });
    connect(historyDialog, &OriginDimensionHistoryDialog::revertVersionRequested,
            this, &OriginDimensionController::onRevertVersion);
    connect(historyDialog, &OriginDimensionHistoryDialog::openVersionRequested,
            this, &OriginDimensionController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Origin Dimension History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    // Track this history window
    track_window(windowKey, historyWindow);

    allDetachableWindows_.append(historyWindow);
    QPointer<OriginDimensionController> self = this;
    QPointer<DetachableMdiSubWindow> windowPtr = historyWindow;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowPtr, windowKey]() {
        if (self) {
            self->allDetachableWindows_.removeAll(windowPtr.data());
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void OriginDimensionController::onOpenVersion(
    const dq::domain::origin_dimension& dimension, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for origin dimension: " << dimension.code;

    const QString code = QString::fromStdString(dimension.code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new OriginDimensionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setDimension(dimension);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &OriginDimensionDetailDialog::statusMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(detailDialog, &OriginDimensionDetailDialog::errorMessage,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Origin Dimension: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    track_window(windowKey, detailWindow);
    allDetachableWindows_.append(detailWindow);

    QPointer<OriginDimensionController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, detailWindow, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
            self->allDetachableWindows_.removeAll(detailWindow);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void OriginDimensionController::onRevertVersion(
    const dq::domain::origin_dimension& dimension) {
    BOOST_LOG_SEV(lg(), info) << "Reverting origin dimension to version: "
                              << dimension.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new OriginDimensionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setDimension(dimension);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &OriginDimensionDetailDialog::statusMessage,
            this, &OriginDimensionController::statusMessage);
    connect(detailDialog, &OriginDimensionDetailDialog::errorMessage,
            this, &OriginDimensionController::errorMessage);
    connect(detailDialog, &OriginDimensionDetailDialog::dimensionSaved,
            this, [this](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Origin dimension reverted: " << code.toStdString();
        emit statusMessage(QString("Origin dimension '%1' reverted successfully").arg(code));
        if (listWindow_) {
            listWindow_->reload();
        }
    });

    const QColor iconColor(220, 220, 220);
    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Origin Dimension: %1")
        .arg(QString::fromStdString(dimension.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_rotate_counterclockwise_20_regular.svg", iconColor));

    allDetachableWindows_.append(detailWindow);
    QPointer<OriginDimensionController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, detailWindow]() {
        if (self) {
            self->allDetachableWindows_.removeAll(detailWindow);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

}
