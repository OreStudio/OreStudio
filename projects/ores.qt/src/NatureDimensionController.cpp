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
#include "ores.qt/NatureDimensionController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/NatureDimensionMdiWindow.hpp"
#include "ores.qt/NatureDimensionDetailDialog.hpp"
#include "ores.qt/NatureDimensionHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

NatureDimensionController::NatureDimensionController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "NatureDimensionController created";
}

NatureDimensionController::~NatureDimensionController() {
    BOOST_LOG_SEV(lg(), debug) << "NatureDimensionController destroyed";
}

void NatureDimensionController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "nature_dimensions");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new NatureDimensionMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &NatureDimensionMdiWindow::statusChanged,
            this, &NatureDimensionController::statusMessage);
    connect(listWindow_, &NatureDimensionMdiWindow::errorOccurred,
            this, &NatureDimensionController::errorMessage);
    connect(listWindow_, &NatureDimensionMdiWindow::showDimensionDetails,
            this, &NatureDimensionController::onShowDetails);
    connect(listWindow_, &NatureDimensionMdiWindow::addNewRequested,
            this, &NatureDimensionController::onAddNewRequested);
    connect(listWindow_, &NatureDimensionMdiWindow::showDimensionHistory,
            this, &NatureDimensionController::onShowHistory);

    // Create MDI subwindow
    const QColor iconColor(220, 220, 220);
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Nature Dimensions");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_database_20_regular.svg", iconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [this, key]() {
        untrack_window(key);
        listWindow_ = nullptr;
        listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Nature dimension list window created";
}

void NatureDimensionController::closeAllWindows() {
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

void NatureDimensionController::onShowDetails(
    const dq::domain::nature_dimension& dimension) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << dimension.code;
    showDetailWindow(dimension);
}

void NatureDimensionController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new nature dimension requested";
    showAddWindow();
}

void NatureDimensionController::onShowHistory(const QString& code) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << code.toStdString();
    showHistoryWindow(code);
}

void NatureDimensionController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new nature dimension";

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new NatureDimensionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &NatureDimensionDetailDialog::statusMessage,
            this, &NatureDimensionController::statusMessage);
    connect(detailDialog, &NatureDimensionDetailDialog::errorMessage,
            this, &NatureDimensionController::errorMessage);
    connect(detailDialog, &NatureDimensionDetailDialog::dimensionSaved,
            this, [this](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Nature dimension saved: " << code.toStdString();
        if (listWindow_) {
            listWindow_->reload();
        }
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Nature Dimension");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_database_20_regular.svg", iconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void NatureDimensionController::showDetailWindow(
    const dq::domain::nature_dimension& dimension) {

    const QString identifier = QString::fromStdString(dimension.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << dimension.code;

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new NatureDimensionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setDimension(dimension);

    connect(detailDialog, &NatureDimensionDetailDialog::statusMessage,
            this, &NatureDimensionController::statusMessage);
    connect(detailDialog, &NatureDimensionDetailDialog::errorMessage,
            this, &NatureDimensionController::errorMessage);
    connect(detailDialog, &NatureDimensionDetailDialog::dimensionSaved,
            this, [this](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Nature dimension saved: " << code.toStdString();
        if (listWindow_) {
            listWindow_->reload();
        }
    });
    connect(detailDialog, &NatureDimensionDetailDialog::dimensionDeleted,
            this, [this, key](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Nature dimension deleted: " << code.toStdString();
        if (listWindow_) {
            listWindow_->reload();
        }
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Nature Dimension: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_database_20_regular.svg", iconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<NatureDimensionController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void NatureDimensionController::onNotificationReceived(
    const QString& eventType, const QDateTime& timestamp,
    const QStringList& entityIds) {
    Q_UNUSED(eventType);
    Q_UNUSED(timestamp);
    Q_UNUSED(entityIds);
    // Event handling can be added later if needed
}

void NatureDimensionController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for nature dimension: "
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

    auto* historyDialog = new NatureDimensionHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog, &NatureDimensionHistoryDialog::statusChanged,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(historyDialog, &NatureDimensionHistoryDialog::errorOccurred,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });
    connect(historyDialog, &NatureDimensionHistoryDialog::revertVersionRequested,
            this, &NatureDimensionController::onRevertVersion);
    connect(historyDialog, &NatureDimensionHistoryDialog::openVersionRequested,
            this, &NatureDimensionController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Nature Dimension History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<NatureDimensionController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void NatureDimensionController::onOpenVersion(
    const dq::domain::nature_dimension& dimension, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for nature dimension: " << dimension.code;

    const QString code = QString::fromStdString(dimension.code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new NatureDimensionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setDimension(dimension);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &NatureDimensionDetailDialog::statusMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(detailDialog, &NatureDimensionDetailDialog::errorMessage,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Nature Dimension: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<NatureDimensionController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void NatureDimensionController::onRevertVersion(
    const dq::domain::nature_dimension& dimension) {
    BOOST_LOG_SEV(lg(), info) << "Reverting nature dimension to version: "
                              << dimension.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new NatureDimensionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setDimension(dimension);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &NatureDimensionDetailDialog::statusMessage,
            this, &NatureDimensionController::statusMessage);
    connect(detailDialog, &NatureDimensionDetailDialog::errorMessage,
            this, &NatureDimensionController::errorMessage);
    connect(detailDialog, &NatureDimensionDetailDialog::dimensionSaved,
            this, [this](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Nature dimension reverted: " << code.toStdString();
        emit statusMessage(QString("Nature dimension '%1' reverted successfully").arg(code));
        if (listWindow_) {
            listWindow_->reload();
        }
    });

    const QColor iconColor(220, 220, 220);
    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Nature Dimension: %1")
        .arg(QString::fromStdString(dimension.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_rotate_counterclockwise_20_regular.svg", iconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

}
