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
#include "ores.qt/BookStatusController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/BookStatusMdiWindow.hpp"
#include "ores.qt/BookStatusDetailDialog.hpp"
#include "ores.qt/BookStatusHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

BookStatusController::BookStatusController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "BookStatusController created";
}

void BookStatusController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "book_statuses");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new BookStatusMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &BookStatusMdiWindow::statusChanged,
            this, &BookStatusController::statusMessage);
    connect(listWindow_, &BookStatusMdiWindow::errorOccurred,
            this, &BookStatusController::errorMessage);
    connect(listWindow_, &BookStatusMdiWindow::showStatusDetails,
            this, &BookStatusController::onShowDetails);
    connect(listWindow_, &BookStatusMdiWindow::addNewRequested,
            this, &BookStatusController::onAddNewRequested);
    connect(listWindow_, &BookStatusMdiWindow::showStatusHistory,
            this, &BookStatusController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Book Statuses");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Flag, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<BookStatusController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Book Status list window created";
}

void BookStatusController::closeAllWindows() {
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

void BookStatusController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void BookStatusController::onShowDetails(
    const refdata::domain::book_status& status) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << status.code;
    showDetailWindow(status);
}

void BookStatusController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new book status requested";
    showAddWindow();
}

void BookStatusController::onShowHistory(
    const refdata::domain::book_status& status) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << status.code;
    showHistoryWindow(QString::fromStdString(status.code));
}

void BookStatusController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new book status";

    auto* detailDialog = new BookStatusDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &BookStatusDetailDialog::statusMessage,
            this, &BookStatusController::statusMessage);
    connect(detailDialog, &BookStatusDetailDialog::errorMessage,
            this, &BookStatusController::errorMessage);
    connect(detailDialog, &BookStatusDetailDialog::statusSaved,
            this, [self = QPointer<BookStatusController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Book Status saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Book Status");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Flag, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void BookStatusController::showDetailWindow(
    const refdata::domain::book_status& status) {

    const QString identifier = QString::fromStdString(status.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << status.code;

    auto* detailDialog = new BookStatusDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setStatus(status);

    connect(detailDialog, &BookStatusDetailDialog::statusMessage,
            this, &BookStatusController::statusMessage);
    connect(detailDialog, &BookStatusDetailDialog::errorMessage,
            this, &BookStatusController::errorMessage);
    connect(detailDialog, &BookStatusDetailDialog::statusSaved,
            this, [self = QPointer<BookStatusController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Book Status saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &BookStatusDetailDialog::statusDeleted,
            this, [self = QPointer<BookStatusController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Book Status deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Book Status: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Flag, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<BookStatusController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void BookStatusController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for book status: "
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

    auto* historyDialog = new BookStatusHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog, &BookStatusHistoryDialog::statusChanged,
            this, [self = QPointer<BookStatusController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &BookStatusHistoryDialog::errorOccurred,
            this, [self = QPointer<BookStatusController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &BookStatusHistoryDialog::revertVersionRequested,
            this, &BookStatusController::onRevertVersion);
    connect(historyDialog, &BookStatusHistoryDialog::openVersionRequested,
            this, &BookStatusController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Book Status History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<BookStatusController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void BookStatusController::onOpenVersion(
    const refdata::domain::book_status& status, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for book status: " << status.code;

    const QString code = QString::fromStdString(status.code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new BookStatusDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setStatus(status);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &BookStatusDetailDialog::statusMessage,
            this, [self = QPointer<BookStatusController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &BookStatusDetailDialog::errorMessage,
            this, [self = QPointer<BookStatusController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Book Status: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<BookStatusController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void BookStatusController::onRevertVersion(
    const refdata::domain::book_status& status) {
    BOOST_LOG_SEV(lg(), info) << "Reverting book status to version: "
                              << status.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new BookStatusDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setStatus(status);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &BookStatusDetailDialog::statusMessage,
            this, &BookStatusController::statusMessage);
    connect(detailDialog, &BookStatusDetailDialog::errorMessage,
            this, &BookStatusController::errorMessage);
    connect(detailDialog, &BookStatusDetailDialog::statusSaved,
            this, [self = QPointer<BookStatusController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Book Status reverted: " << code.toStdString();
        emit self->statusMessage(QString("Book Status '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Book Status: %1")
        .arg(QString::fromStdString(status.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* BookStatusController::listWindow() const {
    return listWindow_;
}

}
