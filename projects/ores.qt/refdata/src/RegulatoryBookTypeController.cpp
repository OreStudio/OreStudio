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
#include "ores.qt/RegulatoryBookTypeController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/RegulatoryBookTypeDetailDialog.hpp"
#include "ores.qt/RegulatoryBookTypeHistoryDialog.hpp"
#include "ores.qt/RegulatoryBookTypeMdiWindow.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/regulatory_book_type_changed_event.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view type_event_name =
    eventing::domain::event_traits<refdata::eventing::regulatory_book_type_changed_event>::name;
}

RegulatoryBookTypeController::RegulatoryBookTypeController(QMainWindow* mainWindow,
                                                           QMdiArea* mdiArea,
                                                           ClientManager* clientManager,
                                                           const QString& username,
                                                           QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, type_event_name, parent)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "RegulatoryBookTypeController created";
}

void RegulatoryBookTypeController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "regulatory_book_types");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new RegulatoryBookTypeMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &RegulatoryBookTypeMdiWindow::statusChanged,
            this,
            &RegulatoryBookTypeController::statusMessage);
    connect(listWindow_,
            &RegulatoryBookTypeMdiWindow::errorOccurred,
            this,
            &RegulatoryBookTypeController::errorMessage);
    connect(listWindow_,
            &RegulatoryBookTypeMdiWindow::showTypeDetails,
            this,
            &RegulatoryBookTypeController::onShowDetails);
    connect(listWindow_,
            &RegulatoryBookTypeMdiWindow::addNewRequested,
            this,
            &RegulatoryBookTypeController::onAddNewRequested);
    connect(listWindow_,
            &RegulatoryBookTypeMdiWindow::showTypeHistory,
            this,
            &RegulatoryBookTypeController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Regulatory Book Types");
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
            [self = QPointer<RegulatoryBookTypeController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Regulatory Book Type list window created";
}

void RegulatoryBookTypeController::closeAllWindows() {
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

void RegulatoryBookTypeController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void RegulatoryBookTypeController::onShowDetails(
    const refdata::domain::regulatory_book_type& type) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << type.code;
    showDetailWindow(type);
}

void RegulatoryBookTypeController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new regulatory book type requested";
    showAddWindow();
}


void RegulatoryBookTypeController::onShowHistory(
    const refdata::domain::regulatory_book_type& type) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << type.code;
    showHistoryWindow(QString::fromStdString(type.code));
}

void RegulatoryBookTypeController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new regulatory book type";

    auto* detailDialog = new RegulatoryBookTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &RegulatoryBookTypeDetailDialog::statusMessage,
            this,
            &RegulatoryBookTypeController::statusMessage);
    connect(detailDialog,
            &RegulatoryBookTypeDetailDialog::errorMessage,
            this,
            &RegulatoryBookTypeController::errorMessage);
    connect(detailDialog,
            &RegulatoryBookTypeDetailDialog::typeSaved,
            this,
            [self = QPointer<RegulatoryBookTypeController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Regulatory Book Type saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Regulatory Book Type");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Tag, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void RegulatoryBookTypeController::showDetailWindow(
    const refdata::domain::regulatory_book_type& type) {

    const QString identifier = QString::fromStdString(type.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << type.code;

    auto* detailDialog = new RegulatoryBookTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setType(type);

    connect(detailDialog,
            &RegulatoryBookTypeDetailDialog::statusMessage,
            this,
            &RegulatoryBookTypeController::statusMessage);
    connect(detailDialog,
            &RegulatoryBookTypeDetailDialog::errorMessage,
            this,
            &RegulatoryBookTypeController::errorMessage);
    connect(detailDialog,
            &RegulatoryBookTypeDetailDialog::typeSaved,
            this,
            [self = QPointer<RegulatoryBookTypeController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Regulatory Book Type saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &RegulatoryBookTypeDetailDialog::typeDeleted,
            this,
            [self = QPointer<RegulatoryBookTypeController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Regulatory Book Type deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Regulatory Book Type: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Tag, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<RegulatoryBookTypeController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void RegulatoryBookTypeController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for regulatory book type: "
                              << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog = new RegulatoryBookTypeHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog,
            &RegulatoryBookTypeHistoryDialog::statusChanged,
            this,
            [self = QPointer<RegulatoryBookTypeController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &RegulatoryBookTypeHistoryDialog::errorOccurred,
            this,
            [self = QPointer<RegulatoryBookTypeController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &RegulatoryBookTypeHistoryDialog::revertVersionRequested,
            this,
            &RegulatoryBookTypeController::onRevertVersion);
    connect(historyDialog,
            &RegulatoryBookTypeHistoryDialog::openVersionRequested,
            this,
            &RegulatoryBookTypeController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Regulatory Book Type History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<RegulatoryBookTypeController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void RegulatoryBookTypeController::onOpenVersion(const refdata::domain::regulatory_book_type& type,
                                                 int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for regulatory book type: " << type.code;

    const QString code = QString::fromStdString(type.code);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new RegulatoryBookTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setType(type);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &RegulatoryBookTypeDetailDialog::statusMessage,
            this,
            [self = QPointer<RegulatoryBookTypeController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &RegulatoryBookTypeDetailDialog::errorMessage,
            this,
            [self = QPointer<RegulatoryBookTypeController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Regulatory Book Type: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<RegulatoryBookTypeController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void RegulatoryBookTypeController::onRevertVersion(
    const refdata::domain::regulatory_book_type& type) {
    BOOST_LOG_SEV(lg(), info) << "Reverting regulatory book type to version: " << type.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new RegulatoryBookTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_type = type;
    reverted_type.version = 0;
    detailDialog->setType(reverted_type);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &RegulatoryBookTypeDetailDialog::statusMessage,
            this,
            &RegulatoryBookTypeController::statusMessage);
    connect(detailDialog,
            &RegulatoryBookTypeDetailDialog::errorMessage,
            this,
            &RegulatoryBookTypeController::errorMessage);
    connect(detailDialog,
            &RegulatoryBookTypeDetailDialog::typeSaved,
            this,
            [self = QPointer<RegulatoryBookTypeController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info)
                    << "Regulatory Book Type reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Regulatory Book Type '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Regulatory Book Type: %1").arg(QString::fromStdString(type.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* RegulatoryBookTypeController::listWindow() const {
    return listWindow_;
}

}
