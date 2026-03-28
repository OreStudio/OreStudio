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
#include "ores.qt/BadgeSeverityController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/BadgeSeverityMdiWindow.hpp"
#include "ores.qt/BadgeSeverityDetailDialog.hpp"
#include "ores.qt/BadgeSeverityHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

BadgeSeverityController::BadgeSeverityController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "BadgeSeverityController created";
}

void BadgeSeverityController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "badge_severities");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new BadgeSeverityMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &BadgeSeverityMdiWindow::statusChanged,
            this, &BadgeSeverityController::statusMessage);
    connect(listWindow_, &BadgeSeverityMdiWindow::errorOccurred,
            this, &BadgeSeverityController::errorMessage);
    connect(listWindow_, &BadgeSeverityMdiWindow::showSeverityDetails,
            this, &BadgeSeverityController::onShowDetails);
    connect(listWindow_, &BadgeSeverityMdiWindow::addNewRequested,
            this, &BadgeSeverityController::onAddNewRequested);
    connect(listWindow_, &BadgeSeverityMdiWindow::showSeverityHistory,
            this, &BadgeSeverityController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Badge Severities");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Award, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<BadgeSeverityController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Badge Severity list window created";
}

void BadgeSeverityController::closeAllWindows() {
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

void BadgeSeverityController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void BadgeSeverityController::onShowDetails(
    const dq::domain::badge_severity& severity) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << severity.code;
    showDetailWindow(severity);
}

void BadgeSeverityController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new badge severity requested";
    showAddWindow();
}

void BadgeSeverityController::onShowHistory(
    const dq::domain::badge_severity& severity) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << severity.code;
    showHistoryWindow(QString::fromStdString(severity.code));
}

void BadgeSeverityController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new badge severity";

    auto* detailDialog = new BadgeSeverityDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &BadgeSeverityDetailDialog::statusMessage,
            this, &BadgeSeverityController::statusMessage);
    connect(detailDialog, &BadgeSeverityDetailDialog::errorMessage,
            this, &BadgeSeverityController::errorMessage);
    connect(detailDialog, &BadgeSeverityDetailDialog::severitySaved,
            this, [self = QPointer<BadgeSeverityController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Badge Severity saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Badge Severity");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Award, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void BadgeSeverityController::showDetailWindow(
    const dq::domain::badge_severity& severity) {

    const QString identifier = QString::fromStdString(severity.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << severity.code;

    auto* detailDialog = new BadgeSeverityDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setSeverity(severity);

    connect(detailDialog, &BadgeSeverityDetailDialog::statusMessage,
            this, &BadgeSeverityController::statusMessage);
    connect(detailDialog, &BadgeSeverityDetailDialog::errorMessage,
            this, &BadgeSeverityController::errorMessage);
    connect(detailDialog, &BadgeSeverityDetailDialog::severitySaved,
            this, [self = QPointer<BadgeSeverityController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Badge Severity saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &BadgeSeverityDetailDialog::severityDeleted,
            this, [self = QPointer<BadgeSeverityController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Badge Severity deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Badge Severity: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Award, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<BadgeSeverityController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void BadgeSeverityController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for badge severity: "
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

    auto* historyDialog = new BadgeSeverityHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog, &BadgeSeverityHistoryDialog::statusChanged,
            this, [self = QPointer<BadgeSeverityController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &BadgeSeverityHistoryDialog::errorOccurred,
            this, [self = QPointer<BadgeSeverityController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &BadgeSeverityHistoryDialog::revertVersionRequested,
            this, &BadgeSeverityController::onRevertVersion);
    connect(historyDialog, &BadgeSeverityHistoryDialog::openVersionRequested,
            this, &BadgeSeverityController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Badge Severity History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<BadgeSeverityController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void BadgeSeverityController::onOpenVersion(
    const dq::domain::badge_severity& severity, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for badge severity: " << severity.code;

    const QString code = QString::fromStdString(severity.code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new BadgeSeverityDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setSeverity(severity);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &BadgeSeverityDetailDialog::statusMessage,
            this, [self = QPointer<BadgeSeverityController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &BadgeSeverityDetailDialog::errorMessage,
            this, [self = QPointer<BadgeSeverityController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Badge Severity: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<BadgeSeverityController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void BadgeSeverityController::onRevertVersion(
    const dq::domain::badge_severity& severity) {
    BOOST_LOG_SEV(lg(), info) << "Reverting badge severity to version: "
                              << severity.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new BadgeSeverityDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setSeverity(severity);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &BadgeSeverityDetailDialog::statusMessage,
            this, &BadgeSeverityController::statusMessage);
    connect(detailDialog, &BadgeSeverityDetailDialog::errorMessage,
            this, &BadgeSeverityController::errorMessage);
    connect(detailDialog, &BadgeSeverityDetailDialog::severitySaved,
            this, [self = QPointer<BadgeSeverityController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Badge Severity reverted: " << code.toStdString();
        emit self->statusMessage(QString("Badge Severity '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Badge Severity: %1")
        .arg(QString::fromStdString(severity.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* BadgeSeverityController::listWindow() const {
    return listWindow_;
}

}
