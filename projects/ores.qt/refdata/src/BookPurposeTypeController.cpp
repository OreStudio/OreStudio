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
#include "ores.qt/BookPurposeTypeController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/BookPurposeTypeDetailDialog.hpp"
#include "ores.qt/BookPurposeTypeHistoryDialog.hpp"
#include "ores.qt/BookPurposeTypeMdiWindow.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/book_purpose_type_changed_event.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view type_event_name =
    eventing::domain::event_traits<refdata::eventing::book_purpose_type_changed_event>::name;
}

BookPurposeTypeController::BookPurposeTypeController(QMainWindow* mainWindow,
                                                     QMdiArea* mdiArea,
                                                     ClientManager* clientManager,
                                                     ChangeReasonCache* changeReasonCache,
                                                     const QString& username,
                                                     QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, type_event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "BookPurposeTypeController created";
}

void BookPurposeTypeController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "book_purpose_types");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new BookPurposeTypeMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &BookPurposeTypeMdiWindow::statusChanged,
            this,
            &BookPurposeTypeController::statusMessage);
    connect(listWindow_,
            &BookPurposeTypeMdiWindow::errorOccurred,
            this,
            &BookPurposeTypeController::errorMessage);
    connect(listWindow_,
            &BookPurposeTypeMdiWindow::showTypeDetails,
            this,
            &BookPurposeTypeController::onShowDetails);
    connect(listWindow_,
            &BookPurposeTypeMdiWindow::addNewRequested,
            this,
            &BookPurposeTypeController::onAddNewRequested);
    connect(listWindow_,
            &BookPurposeTypeMdiWindow::showTypeHistory,
            this,
            &BookPurposeTypeController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Book Purpose Types");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Tag, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);
    listMdiSubWindow_->setGeometryKey(key);
    UiPersistence::restoreMdiGeometry(key, listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_,
            &QObject::destroyed,
            this,
            [self = QPointer<BookPurposeTypeController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Book Purpose Type list window created";
}

void BookPurposeTypeController::closeAllWindows() {
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

void BookPurposeTypeController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void BookPurposeTypeController::onShowDetails(const refdata::domain::book_purpose_type& type) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << type.code;
    showDetailWindow(type);
}

void BookPurposeTypeController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new book purpose type requested";
    showAddWindow();
}


void BookPurposeTypeController::onShowHistory(const refdata::domain::book_purpose_type& type) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << type.code;
    showHistoryWindow(QString::fromStdString(type.code));
}

void BookPurposeTypeController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new book purpose type";

    auto* detailDialog = new BookPurposeTypeDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &BookPurposeTypeDetailDialog::statusMessage,
            this,
            &BookPurposeTypeController::statusMessage);
    connect(detailDialog,
            &BookPurposeTypeDetailDialog::errorMessage,
            this,
            &BookPurposeTypeController::errorMessage);
    connect(detailDialog,
            &BookPurposeTypeDetailDialog::typeSaved,
            this,
            [self = QPointer<BookPurposeTypeController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Book Purpose Type saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Book Purpose Type");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Tag, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void BookPurposeTypeController::showDetailWindow(const refdata::domain::book_purpose_type& type) {

    const QString identifier = QString::fromStdString(type.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << type.code;

    auto* detailDialog = new BookPurposeTypeDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setType(type);

    connect(detailDialog,
            &BookPurposeTypeDetailDialog::statusMessage,
            this,
            &BookPurposeTypeController::statusMessage);
    connect(detailDialog,
            &BookPurposeTypeDetailDialog::errorMessage,
            this,
            &BookPurposeTypeController::errorMessage);
    connect(detailDialog,
            &BookPurposeTypeDetailDialog::typeSaved,
            this,
            [self = QPointer<BookPurposeTypeController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Book Purpose Type saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &BookPurposeTypeDetailDialog::typeDeleted,
            this,
            [self = QPointer<BookPurposeTypeController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Book Purpose Type deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Book Purpose Type: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Tag, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<BookPurposeTypeController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void BookPurposeTypeController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for book purpose type: "
                              << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog = new BookPurposeTypeHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog,
            &BookPurposeTypeHistoryDialog::statusChanged,
            this,
            [self = QPointer<BookPurposeTypeController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &BookPurposeTypeHistoryDialog::errorOccurred,
            this,
            [self = QPointer<BookPurposeTypeController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &BookPurposeTypeHistoryDialog::revertVersionRequested,
            this,
            &BookPurposeTypeController::onRevertVersion);
    connect(historyDialog,
            &BookPurposeTypeHistoryDialog::openVersionRequested,
            this,
            &BookPurposeTypeController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Book Purpose Type History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<BookPurposeTypeController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void BookPurposeTypeController::onOpenVersion(const refdata::domain::book_purpose_type& type,
                                              int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for book purpose type: " << type.code;

    const QString code = QString::fromStdString(type.code);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new BookPurposeTypeDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setType(type);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &BookPurposeTypeDetailDialog::statusMessage,
            this,
            [self = QPointer<BookPurposeTypeController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &BookPurposeTypeDetailDialog::errorMessage,
            this,
            [self = QPointer<BookPurposeTypeController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Book Purpose Type: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<BookPurposeTypeController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void BookPurposeTypeController::onRevertVersion(const refdata::domain::book_purpose_type& type) {
    BOOST_LOG_SEV(lg(), info) << "Reverting book purpose type to version: " << type.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new BookPurposeTypeDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_type = type;
    reverted_type.version = 0;
    detailDialog->setType(reverted_type);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &BookPurposeTypeDetailDialog::statusMessage,
            this,
            &BookPurposeTypeController::statusMessage);
    connect(detailDialog,
            &BookPurposeTypeDetailDialog::errorMessage,
            this,
            &BookPurposeTypeController::errorMessage);
    connect(detailDialog,
            &BookPurposeTypeDetailDialog::typeSaved,
            this,
            [self = QPointer<BookPurposeTypeController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Book Purpose Type reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Book Purpose Type '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Book Purpose Type: %1").arg(QString::fromStdString(type.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* BookPurposeTypeController::listWindow() const {
    return listWindow_;
}

void BookPurposeTypeController::notifyOpenDialogs(const QStringList& entityIds) {
    for (auto it = managed_windows_.begin(); it != managed_windows_.end(); ++it) {
        auto* window = it.value();
        if (!window)
            continue;

        if (it.key().startsWith("details.")) {
            if (auto* dialog = qobject_cast<DetailDialogBase*>(window->widget())) {
                if (entityIds.isEmpty() || entityIds.contains(dialog->code())) {
                    dialog->markAsStale();
                }
            }
        } else if (it.key().startsWith("history.")) {
            if (auto* dialog = qobject_cast<HistoryDialogBase*>(window->widget())) {
                if (entityIds.isEmpty() || entityIds.contains(dialog->code())) {
                    dialog->markAsStale();
                }
            }
        }
    }
}

}
