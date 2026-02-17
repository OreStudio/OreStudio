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
#include "ores.qt/BookController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/BookMdiWindow.hpp"
#include "ores.qt/BookDetailDialog.hpp"
#include "ores.qt/BookHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

BookController::BookController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    ImageCache* imageCache,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      imageCache_(imageCache),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "BookController created";
}

void BookController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "books");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new BookMdiWindow(clientManager_, imageCache_, username_);

    // Connect signals
    connect(listWindow_, &BookMdiWindow::statusChanged,
            this, &BookController::statusMessage);
    connect(listWindow_, &BookMdiWindow::errorOccurred,
            this, &BookController::errorMessage);
    connect(listWindow_, &BookMdiWindow::showBookDetails,
            this, &BookController::onShowDetails);
    connect(listWindow_, &BookMdiWindow::addNewRequested,
            this, &BookController::onAddNewRequested);
    connect(listWindow_, &BookMdiWindow::showBookHistory,
            this, &BookController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Books");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::BookOpen, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<BookController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Book list window created";
}

void BookController::closeAllWindows() {
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

void BookController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void BookController::onShowDetails(
    const refdata::domain::book& book) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << book.name;
    showDetailWindow(book);
}

void BookController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new book requested";
    showAddWindow();
}

void BookController::onShowHistory(
    const refdata::domain::book& book) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << book.name;
    showHistoryWindow(book);
}

void BookController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new book";

    auto* detailDialog = new BookDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &BookDetailDialog::statusMessage,
            this, &BookController::statusMessage);
    connect(detailDialog, &BookDetailDialog::errorMessage,
            this, &BookController::errorMessage);
    connect(detailDialog, &BookDetailDialog::bookSaved,
            this, [self = QPointer<BookController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Book saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Book");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::BookOpen, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void BookController::showDetailWindow(
    const refdata::domain::book& book) {

    const QString identifier = QString::fromStdString(book.name);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << book.name;

    auto* detailDialog = new BookDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setBook(book);

    connect(detailDialog, &BookDetailDialog::statusMessage,
            this, &BookController::statusMessage);
    connect(detailDialog, &BookDetailDialog::errorMessage,
            this, &BookController::errorMessage);
    connect(detailDialog, &BookDetailDialog::bookSaved,
            this, [self = QPointer<BookController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Book saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &BookDetailDialog::bookDeleted,
            this, [self = QPointer<BookController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Book deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Book: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::BookOpen, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<BookController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void BookController::showHistoryWindow(
    const refdata::domain::book& book) {
    const QString code = QString::fromStdString(book.name);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for book: "
                              << book.name;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << book.name;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << book.name;

    auto* historyDialog = new BookHistoryDialog(
        book.id, code, clientManager_, mainWindow_);

    connect(historyDialog, &BookHistoryDialog::statusChanged,
            this, [self = QPointer<BookController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &BookHistoryDialog::errorOccurred,
            this, [self = QPointer<BookController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &BookHistoryDialog::revertVersionRequested,
            this, &BookController::onRevertVersion);
    connect(historyDialog, &BookHistoryDialog::openVersionRequested,
            this, &BookController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Book History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<BookController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void BookController::onOpenVersion(
    const refdata::domain::book& book, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for book: " << book.name;

    const QString code = QString::fromStdString(book.name);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new BookDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setBook(book);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &BookDetailDialog::statusMessage,
            this, [self = QPointer<BookController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &BookDetailDialog::errorMessage,
            this, [self = QPointer<BookController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Book: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<BookController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void BookController::onRevertVersion(
    const refdata::domain::book& book) {
    BOOST_LOG_SEV(lg(), info) << "Reverting book to version: "
                              << book.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new BookDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setBook(book);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &BookDetailDialog::statusMessage,
            this, &BookController::statusMessage);
    connect(detailDialog, &BookDetailDialog::errorMessage,
            this, &BookController::errorMessage);
    connect(detailDialog, &BookDetailDialog::bookSaved,
            this, [self = QPointer<BookController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Book reverted: " << code.toStdString();
        emit self->statusMessage(QString("Book '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Book: %1")
        .arg(QString::fromStdString(book.name)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* BookController::listWindow() const {
    return listWindow_;
}

}
