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
#include "ores.qt/CodingSchemeAuthorityTypeController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/CodingSchemeAuthorityTypeMdiWindow.hpp"
#include "ores.qt/CodingSchemeAuthorityTypeDetailDialog.hpp"
#include "ores.qt/CodingSchemeAuthorityTypeHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.dq/eventing/coding_scheme_authority_type_changed_event.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    constexpr std::string_view coding_scheme_authority_type_event_name =
        eventing::domain::event_traits<
            dq::eventing::coding_scheme_authority_type_changed_event>::name;
}

CodingSchemeAuthorityTypeController::CodingSchemeAuthorityTypeController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "CodingSchemeAuthorityTypeController created";

    if (clientManager_) {
        connect(clientManager_, &ClientManager::notificationReceived,
                this, &CodingSchemeAuthorityTypeController::onNotificationReceived);

        connect(clientManager_, &ClientManager::connected,
                this, [this]() {
            BOOST_LOG_SEV(lg(), info) << "Subscribing to coding scheme authority type change events";
            clientManager_->subscribeToEvent(std::string{coding_scheme_authority_type_event_name});
        });

        connect(clientManager_, &ClientManager::reconnected,
                this, [this]() {
            BOOST_LOG_SEV(lg(), info) << "Re-subscribing to coding scheme authority type change events";
            clientManager_->subscribeToEvent(std::string{coding_scheme_authority_type_event_name});
        });

        if (clientManager_->isConnected()) {
            BOOST_LOG_SEV(lg(), info) << "Already connected, subscribing to events";
            clientManager_->subscribeToEvent(std::string{coding_scheme_authority_type_event_name});
        }
    }
}

CodingSchemeAuthorityTypeController::~CodingSchemeAuthorityTypeController() {
    BOOST_LOG_SEV(lg(), debug) << "CodingSchemeAuthorityTypeController destroyed";

    if (clientManager_) {
        BOOST_LOG_SEV(lg(), debug) << "Unsubscribing from coding scheme authority type change events";
        clientManager_->unsubscribeFromEvent(std::string{coding_scheme_authority_type_event_name});
    }
}

void CodingSchemeAuthorityTypeController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "coding_scheme_authority_types");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    listWindow_ = new CodingSchemeAuthorityTypeMdiWindow(clientManager_, username_);

    connect(listWindow_, &CodingSchemeAuthorityTypeMdiWindow::statusChanged,
            this, &CodingSchemeAuthorityTypeController::statusMessage);
    connect(listWindow_, &CodingSchemeAuthorityTypeMdiWindow::errorOccurred,
            this, &CodingSchemeAuthorityTypeController::errorMessage);
    connect(listWindow_, &CodingSchemeAuthorityTypeMdiWindow::showAuthorityTypeDetails,
            this, &CodingSchemeAuthorityTypeController::onShowDetails);
    connect(listWindow_, &CodingSchemeAuthorityTypeMdiWindow::addNewRequested,
            this, &CodingSchemeAuthorityTypeController::onAddNewRequested);
    connect(listWindow_, &CodingSchemeAuthorityTypeMdiWindow::showAuthorityTypeHistory,
            this, &CodingSchemeAuthorityTypeController::onShowHistory);

    const QColor iconColor(220, 220, 220);
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Coding Scheme Authority Types");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_tag_20_regular.svg", iconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    connect(listMdiSubWindow_, &QObject::destroyed, this, [this, key]() {
        untrack_window(key);
        listWindow_ = nullptr;
        listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Coding scheme authority type list window created";
}

void CodingSchemeAuthorityTypeController::closeAllWindows() {
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

void CodingSchemeAuthorityTypeController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void CodingSchemeAuthorityTypeController::onShowDetails(
    const dq::domain::coding_scheme_authority_type& authorityType) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << authorityType.code;
    showDetailWindow(authorityType);
}

void CodingSchemeAuthorityTypeController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new authority type requested";
    showAddWindow();
}

void CodingSchemeAuthorityTypeController::onShowHistory(const QString& code) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << code.toStdString();
    showHistoryWindow(code);
}

void CodingSchemeAuthorityTypeController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new authority type";

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new CodingSchemeAuthorityTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &CodingSchemeAuthorityTypeDetailDialog::statusMessage,
            this, &CodingSchemeAuthorityTypeController::statusMessage);
    connect(detailDialog, &CodingSchemeAuthorityTypeDetailDialog::errorMessage,
            this, &CodingSchemeAuthorityTypeController::errorMessage);
    connect(detailDialog, &CodingSchemeAuthorityTypeDetailDialog::authorityTypeSaved,
            this, [this](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Authority type saved: " << code.toStdString();
        handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Authority Type");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_tag_20_regular.svg", iconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CodingSchemeAuthorityTypeController::showDetailWindow(
    const dq::domain::coding_scheme_authority_type& authorityType) {

    const QString identifier = QString::fromStdString(authorityType.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << authorityType.code;

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new CodingSchemeAuthorityTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setAuthorityType(authorityType);

    connect(detailDialog, &CodingSchemeAuthorityTypeDetailDialog::statusMessage,
            this, &CodingSchemeAuthorityTypeController::statusMessage);
    connect(detailDialog, &CodingSchemeAuthorityTypeDetailDialog::errorMessage,
            this, &CodingSchemeAuthorityTypeController::errorMessage);
    connect(detailDialog, &CodingSchemeAuthorityTypeDetailDialog::authorityTypeSaved,
            this, [this](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Authority type saved: " << code.toStdString();
        handleEntitySaved();
    });
    connect(detailDialog, &CodingSchemeAuthorityTypeDetailDialog::authorityTypeDeleted,
            this, [this, key](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Authority type deleted: " << code.toStdString();
        handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Authority Type: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_tag_20_regular.svg", iconColor));

    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CodingSchemeAuthorityTypeController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CodingSchemeAuthorityTypeController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for authority type: "
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

    auto* historyDialog = new CodingSchemeAuthorityTypeHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog, &CodingSchemeAuthorityTypeHistoryDialog::statusChanged,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(historyDialog, &CodingSchemeAuthorityTypeHistoryDialog::errorOccurred,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });
    connect(historyDialog, &CodingSchemeAuthorityTypeHistoryDialog::revertVersionRequested,
            this, &CodingSchemeAuthorityTypeController::onRevertVersion);
    connect(historyDialog, &CodingSchemeAuthorityTypeHistoryDialog::openVersionRequested,
            this, &CodingSchemeAuthorityTypeController::onOpenVersion);

    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Authority Type History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<CodingSchemeAuthorityTypeController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void CodingSchemeAuthorityTypeController::onOpenVersion(
    const dq::domain::coding_scheme_authority_type& authorityType, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for authority type: " << authorityType.code;

    const QString code = QString::fromStdString(authorityType.code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new CodingSchemeAuthorityTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setAuthorityType(authorityType);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &CodingSchemeAuthorityTypeDetailDialog::statusMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(detailDialog, &CodingSchemeAuthorityTypeDetailDialog::errorMessage,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Authority Type: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CodingSchemeAuthorityTypeController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void CodingSchemeAuthorityTypeController::onRevertVersion(
    const dq::domain::coding_scheme_authority_type& authorityType) {
    BOOST_LOG_SEV(lg(), info) << "Reverting authority type to version: "
                              << authorityType.version;

    auto* detailDialog = new CodingSchemeAuthorityTypeDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setAuthorityType(authorityType);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &CodingSchemeAuthorityTypeDetailDialog::statusMessage,
            this, &CodingSchemeAuthorityTypeController::statusMessage);
    connect(detailDialog, &CodingSchemeAuthorityTypeDetailDialog::errorMessage,
            this, &CodingSchemeAuthorityTypeController::errorMessage);
    connect(detailDialog, &CodingSchemeAuthorityTypeDetailDialog::authorityTypeSaved,
            this, [this](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Authority type reverted: " << code.toStdString();
        emit statusMessage(QString("Authority type '%1' reverted successfully").arg(code));
        handleEntitySaved();
    });

    const QColor iconColor(220, 220, 220);
    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Authority Type: %1")
        .arg(QString::fromStdString(authorityType.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_rotate_counterclockwise_20_regular.svg", iconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CodingSchemeAuthorityTypeController::onNotificationReceived(
    const QString& eventType, const QDateTime& timestamp,
    const QStringList& entityIds) {

    if (eventType != QString::fromStdString(std::string{coding_scheme_authority_type_event_name})) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Received coding scheme authority type change notification at "
                              << timestamp.toString(Qt::ISODate).toStdString()
                              << " with " << entityIds.size() << " IDs";

    if (listWindow_) {
        listWindow_->markAsStale();
        BOOST_LOG_SEV(lg(), debug) << "Marked coding scheme authority type list as stale";
    }
}

}
