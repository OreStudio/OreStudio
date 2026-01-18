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
#include "ores.qt/CodingSchemeController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/CodingSchemeMdiWindow.hpp"
#include "ores.qt/CodingSchemeDetailDialog.hpp"
#include "ores.qt/CodingSchemeHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

CodingSchemeController::CodingSchemeController(
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

    BOOST_LOG_SEV(lg(), debug) << "CodingSchemeController created";
}

CodingSchemeController::~CodingSchemeController() {
    BOOST_LOG_SEV(lg(), debug) << "CodingSchemeController destroyed";
}

void CodingSchemeController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "coding_schemes");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    listWindow_ = new CodingSchemeMdiWindow(clientManager_, username_);

    connect(listWindow_, &CodingSchemeMdiWindow::statusChanged,
            this, &CodingSchemeController::statusMessage);
    connect(listWindow_, &CodingSchemeMdiWindow::errorOccurred,
            this, &CodingSchemeController::errorMessage);
    connect(listWindow_, &CodingSchemeMdiWindow::showSchemeDetails,
            this, &CodingSchemeController::onShowDetails);
    connect(listWindow_, &CodingSchemeMdiWindow::addNewRequested,
            this, &CodingSchemeController::onAddNewRequested);
    connect(listWindow_, &CodingSchemeMdiWindow::showSchemeHistory,
            this, &CodingSchemeController::onShowHistory);

    const QColor iconColor(220, 220, 220);
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Coding Schemes");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_code_20_regular.svg", iconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    track_window(key, listMdiSubWindow_);
    allDetachableWindows_.append(listMdiSubWindow_);

    connect(listMdiSubWindow_, &QObject::destroyed, this, [this, key]() {
        untrack_window(key);
        allDetachableWindows_.removeOne(listMdiSubWindow_);
        listWindow_ = nullptr;
        listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Coding scheme list window created";
}

void CodingSchemeController::closeAllWindows() {
    BOOST_LOG_SEV(lg(), debug) << "closeAllWindows called";

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

void CodingSchemeController::onShowDetails(const dq::domain::coding_scheme& scheme) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << scheme.code;
    showDetailWindow(scheme);
}

void CodingSchemeController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new coding scheme requested";
    showAddWindow();
}

void CodingSchemeController::onShowHistory(const QString& code) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << code.toStdString();
    showHistoryWindow(code);
}

void CodingSchemeController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new coding scheme";

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new CodingSchemeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);
    detailDialog->loadLookupData();

    connect(detailDialog, &CodingSchemeDetailDialog::statusMessage,
            this, &CodingSchemeController::statusMessage);
    connect(detailDialog, &CodingSchemeDetailDialog::errorMessage,
            this, &CodingSchemeController::errorMessage);
    connect(detailDialog, &CodingSchemeDetailDialog::schemeSaved,
            this, [this](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Coding scheme saved: " << code.toStdString();
        if (listWindow_) {
            listWindow_->reload();
        }
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Coding Scheme");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_code_20_regular.svg", iconColor));

    allDetachableWindows_.append(detailWindow);
    QPointer<CodingSchemeController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, detailWindow]() {
        if (self)
            self->allDetachableWindows_.removeAll(detailWindow);
    });

    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CodingSchemeController::showDetailWindow(const dq::domain::coding_scheme& scheme) {
    const QString identifier = QString::fromStdString(scheme.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << scheme.code;

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new CodingSchemeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->loadLookupData();
    detailDialog->setScheme(scheme);

    connect(detailDialog, &CodingSchemeDetailDialog::statusMessage,
            this, &CodingSchemeController::statusMessage);
    connect(detailDialog, &CodingSchemeDetailDialog::errorMessage,
            this, &CodingSchemeController::errorMessage);
    connect(detailDialog, &CodingSchemeDetailDialog::schemeSaved,
            this, [this](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Coding scheme saved: " << code.toStdString();
        if (listWindow_) {
            listWindow_->reload();
        }
    });
    connect(detailDialog, &CodingSchemeDetailDialog::schemeDeleted,
            this, [this, key](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Coding scheme deleted: " << code.toStdString();
        if (listWindow_) {
            listWindow_->reload();
        }
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Coding Scheme: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_code_20_regular.svg", iconColor));

    track_window(key, detailWindow);
    allDetachableWindows_.append(detailWindow);

    QPointer<CodingSchemeController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, detailWindow, key]() {
        if (self) {
            self->untrack_window(key);
            self->allDetachableWindows_.removeAll(detailWindow);
        }
    });

    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CodingSchemeController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for coding scheme: "
                              << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << code.toStdString();
    const QColor iconColor(220, 220, 220);

    auto* historyDialog = new CodingSchemeHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog, &CodingSchemeHistoryDialog::statusChanged,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(historyDialog, &CodingSchemeHistoryDialog::errorOccurred,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });
    connect(historyDialog, &CodingSchemeHistoryDialog::revertVersionRequested,
            this, &CodingSchemeController::onRevertVersion);
    connect(historyDialog, &CodingSchemeHistoryDialog::openVersionRequested,
            this, &CodingSchemeController::onOpenVersion);

    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Coding Scheme History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    track_window(windowKey, historyWindow);

    allDetachableWindows_.append(historyWindow);
    QPointer<CodingSchemeController> self = this;
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

void CodingSchemeController::onOpenVersion(
    const dq::domain::coding_scheme& scheme, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for coding scheme: " << scheme.code;

    const QString code = QString::fromStdString(scheme.code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new CodingSchemeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->loadLookupData();
    detailDialog->setScheme(scheme);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &CodingSchemeDetailDialog::statusMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(detailDialog, &CodingSchemeDetailDialog::errorMessage,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Coding Scheme: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    track_window(windowKey, detailWindow);
    allDetachableWindows_.append(detailWindow);

    QPointer<CodingSchemeController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, detailWindow, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
            self->allDetachableWindows_.removeAll(detailWindow);
        }
    });

    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void CodingSchemeController::onRevertVersion(
    const dq::domain::coding_scheme& scheme) {
    BOOST_LOG_SEV(lg(), info) << "Reverting coding scheme to version: "
                              << scheme.version;

    auto* detailDialog = new CodingSchemeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->loadLookupData();
    detailDialog->setScheme(scheme);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &CodingSchemeDetailDialog::statusMessage,
            this, &CodingSchemeController::statusMessage);
    connect(detailDialog, &CodingSchemeDetailDialog::errorMessage,
            this, &CodingSchemeController::errorMessage);
    connect(detailDialog, &CodingSchemeDetailDialog::schemeSaved,
            this, [this](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Coding scheme reverted: " << code.toStdString();
        emit statusMessage(QString("Coding scheme '%1' reverted successfully").arg(code));
        if (listWindow_) {
            listWindow_->reload();
        }
    });

    const QColor iconColor(220, 220, 220);
    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Coding Scheme: %1")
        .arg(QString::fromStdString(scheme.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_rotate_counterclockwise_20_regular.svg", iconColor));

    allDetachableWindows_.append(detailWindow);
    QPointer<CodingSchemeController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, detailWindow]() {
        if (self) {
            self->allDetachableWindows_.removeAll(detailWindow);
        }
    });

    show_managed_window(detailWindow, listMdiSubWindow_);
}

}
