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
#include "ores.qt/ResultController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ResultMdiWindow.hpp"
#include "ores.qt/ResultDetailDialog.hpp"
#include "ores.qt/ResultHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

ResultController::ResultController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "ResultController created";
}

void ResultController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "results");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new ResultMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &ResultMdiWindow::statusChanged,
            this, &ResultController::statusMessage);
    connect(listWindow_, &ResultMdiWindow::errorOccurred,
            this, &ResultController::errorMessage);
    connect(listWindow_, &ResultMdiWindow::showResultDetails,
            this, &ResultController::onShowDetails);
    connect(listWindow_, &ResultMdiWindow::addNewRequested,
            this, &ResultController::onAddNewRequested);
    connect(listWindow_, &ResultMdiWindow::showResultHistory,
            this, &ResultController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Results");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Checkmark, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<ResultController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Result list window created";
}

void ResultController::closeAllWindows() {
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

void ResultController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void ResultController::onShowDetails(
    const compute::domain::result& result) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << result.modified_by;
    showDetailWindow(result);
}

void ResultController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new compute result requested";
    showAddWindow();
}

void ResultController::onShowHistory(
    const compute::domain::result& result) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << result.modified_by;
    showHistoryWindow(result);
}

void ResultController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new compute result";

    auto* detailDialog = new ResultDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &ResultDetailDialog::statusMessage,
            this, &ResultController::statusMessage);
    connect(detailDialog, &ResultDetailDialog::errorMessage,
            this, &ResultController::errorMessage);
    connect(detailDialog, &ResultDetailDialog::resultSaved,
            this, [self = QPointer<ResultController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Result saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Result");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Checkmark, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void ResultController::showDetailWindow(
    const compute::domain::result& result) {

    const QString identifier = QString::fromStdString(result.modified_by);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << result.modified_by;

    auto* detailDialog = new ResultDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setResult(result);

    connect(detailDialog, &ResultDetailDialog::statusMessage,
            this, &ResultController::statusMessage);
    connect(detailDialog, &ResultDetailDialog::errorMessage,
            this, &ResultController::errorMessage);
    connect(detailDialog, &ResultDetailDialog::resultSaved,
            this, [self = QPointer<ResultController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Result saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &ResultDetailDialog::resultDeleted,
            this, [self = QPointer<ResultController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Result deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Result: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Checkmark, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<ResultController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void ResultController::showHistoryWindow(
    const compute::domain::result& result) {
    const QString code = QString::fromStdString(result.modified_by);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for compute result: "
                              << result.modified_by;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << result.modified_by;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << result.modified_by;

    auto* historyDialog = new ResultHistoryDialog(
        result.id, code, clientManager_, mainWindow_);

    connect(historyDialog, &ResultHistoryDialog::statusChanged,
            this, [self = QPointer<ResultController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &ResultHistoryDialog::errorOccurred,
            this, [self = QPointer<ResultController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &ResultHistoryDialog::revertVersionRequested,
            this, &ResultController::onRevertVersion);
    connect(historyDialog, &ResultHistoryDialog::openVersionRequested,
            this, &ResultController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Result History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<ResultController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void ResultController::onOpenVersion(
    const compute::domain::result& result, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for compute result: " << result.modified_by;

    const QString code = QString::fromStdString(result.modified_by);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new ResultDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setResult(result);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &ResultDetailDialog::statusMessage,
            this, [self = QPointer<ResultController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &ResultDetailDialog::errorMessage,
            this, [self = QPointer<ResultController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Result: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<ResultController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void ResultController::onRevertVersion(
    const compute::domain::result& result) {
    BOOST_LOG_SEV(lg(), info) << "Reverting compute result to version: "
                              << result.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new ResultDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setResult(result);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &ResultDetailDialog::statusMessage,
            this, &ResultController::statusMessage);
    connect(detailDialog, &ResultDetailDialog::errorMessage,
            this, &ResultController::errorMessage);
    connect(detailDialog, &ResultDetailDialog::resultSaved,
            this, [self = QPointer<ResultController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Result reverted: " << code.toStdString();
        emit self->statusMessage(QString("Result '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Result: %1")
        .arg(QString::fromStdString(result.modified_by)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* ResultController::listWindow() const {
    return listWindow_;
}

}
