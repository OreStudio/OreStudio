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
#include "ores.qt/PurposeTypeController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/PurposeTypeDetailDialog.hpp"
#include "ores.qt/PurposeTypeHistoryDialog.hpp"
#include "ores.qt/PurposeTypeMdiWindow.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/purpose_type_changed_event.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view type_event_name =
    eventing::domain::event_traits<refdata::eventing::purpose_type_changed_event>::name;
}

PurposeTypeController::PurposeTypeController(QMainWindow* mainWindow,
                                             QMdiArea* mdiArea,
                                             ClientManager* clientManager,
                                             ChangeReasonCache* changeReasonCache,
                                             const QString& username,
                                             QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, type_event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "PurposeTypeController created";
}

void PurposeTypeController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "purpose_types");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new PurposeTypeMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &PurposeTypeMdiWindow::statusChanged,
            this,
            &PurposeTypeController::statusMessage);
    connect(listWindow_,
            &PurposeTypeMdiWindow::errorOccurred,
            this,
            &PurposeTypeController::errorMessage);
    connect(listWindow_,
            &PurposeTypeMdiWindow::showTypeDetails,
            this,
            &PurposeTypeController::onShowDetails);
    connect(listWindow_,
            &PurposeTypeMdiWindow::addNewRequested,
            this,
            &PurposeTypeController::onAddNewRequested);
    connect(listWindow_,
            &PurposeTypeMdiWindow::showTypeHistory,
            this,
            &PurposeTypeController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Purpose Types");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Flag, IconUtils::DefaultIconColor));
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
            [self = QPointer<PurposeTypeController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Purpose Type list window created";
}

void PurposeTypeController::closeAllWindows() {
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

void PurposeTypeController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void PurposeTypeController::onShowDetails(const refdata::domain::purpose_type& type) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << type.code;
    showDetailWindow(type);
}

void PurposeTypeController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new purpose type requested";
    showAddWindow();
}

void PurposeTypeController::onShowHistory(const refdata::domain::purpose_type& type) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << type.code;
    showHistoryWindow(QString::fromStdString(type.code));
}

void PurposeTypeController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new purpose type";

    auto* detailDialog = new PurposeTypeDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &PurposeTypeDetailDialog::statusMessage,
            this,
            &PurposeTypeController::statusMessage);
    connect(detailDialog,
            &PurposeTypeDetailDialog::errorMessage,
            this,
            &PurposeTypeController::errorMessage);
    connect(detailDialog,
            &PurposeTypeDetailDialog::typeSaved,
            this,
            [self = QPointer<PurposeTypeController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Purpose Type saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Purpose Type");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Flag, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PurposeTypeController::showDetailWindow(const refdata::domain::purpose_type& type) {

    const QString identifier = QString::fromStdString(type.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << type.code;

    auto* detailDialog = new PurposeTypeDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setType(type);

    connect(detailDialog,
            &PurposeTypeDetailDialog::statusMessage,
            this,
            &PurposeTypeController::statusMessage);
    connect(detailDialog,
            &PurposeTypeDetailDialog::errorMessage,
            this,
            &PurposeTypeController::errorMessage);
    connect(detailDialog,
            &PurposeTypeDetailDialog::typeSaved,
            this,
            [self = QPointer<PurposeTypeController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Purpose Type saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &PurposeTypeDetailDialog::typeDeleted,
            this,
            [self = QPointer<PurposeTypeController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Purpose Type deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Purpose Type: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Flag, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<PurposeTypeController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PurposeTypeController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for purpose type: " << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog = new PurposeTypeHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog,
            &PurposeTypeHistoryDialog::statusChanged,
            this,
            [self = QPointer<PurposeTypeController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &PurposeTypeHistoryDialog::errorOccurred,
            this,
            [self = QPointer<PurposeTypeController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &PurposeTypeHistoryDialog::revertVersionRequested,
            this,
            &PurposeTypeController::onRevertVersion);
    connect(historyDialog,
            &PurposeTypeHistoryDialog::openVersionRequested,
            this,
            &PurposeTypeController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Purpose Type History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<PurposeTypeController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void PurposeTypeController::onOpenVersion(const refdata::domain::purpose_type& type,
                                          int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for purpose type: " << type.code;

    const QString code = QString::fromStdString(type.code);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new PurposeTypeDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setType(type);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &PurposeTypeDetailDialog::statusMessage,
            this,
            [self = QPointer<PurposeTypeController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &PurposeTypeDetailDialog::errorMessage,
            this,
            [self = QPointer<PurposeTypeController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Purpose Type: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<PurposeTypeController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void PurposeTypeController::onRevertVersion(const refdata::domain::purpose_type& type) {
    BOOST_LOG_SEV(lg(), info) << "Reverting purpose type to version: " << type.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new PurposeTypeDetailDialog(mainWindow_);
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
            &PurposeTypeDetailDialog::statusMessage,
            this,
            &PurposeTypeController::statusMessage);
    connect(detailDialog,
            &PurposeTypeDetailDialog::errorMessage,
            this,
            &PurposeTypeController::errorMessage);
    connect(detailDialog,
            &PurposeTypeDetailDialog::typeSaved,
            this,
            [self = QPointer<PurposeTypeController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Purpose Type reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Purpose Type '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Purpose Type: %1").arg(QString::fromStdString(type.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* PurposeTypeController::listWindow() const {
    return listWindow_;
}

}
