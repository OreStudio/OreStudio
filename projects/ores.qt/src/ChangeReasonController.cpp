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
#include "ores.qt/ChangeReasonController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ChangeReasonMdiWindow.hpp"
#include "ores.qt/ChangeReasonDetailDialog.hpp"
#include "ores.qt/ChangeReasonHistoryDialog.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.dq/eventing/change_reason_changed_event.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    // Event type name for change reason changes
    constexpr std::string_view reason_event_name =
        eventing::domain::event_traits<
            dq::eventing::change_reason_changed_event>::name;
}

ChangeReasonController::ChangeReasonController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    ChangeReasonCache* changeReasonCache,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr),
      changeReasonCache_(changeReasonCache) {

    BOOST_LOG_SEV(lg(), debug) << "ChangeReasonController created";

    // Connect to notification signal from ClientManager
    if (clientManager_) {
        connect(clientManager_, &ClientManager::notificationReceived,
                this, &ChangeReasonController::onNotificationReceived);

        // Subscribe to events when connected
        connect(clientManager_, &ClientManager::connected,
                this, [this]() {
            BOOST_LOG_SEV(lg(), info) << "Subscribing to reason change events";
            clientManager_->subscribeToEvent(std::string{reason_event_name});
        });

        // Re-subscribe after reconnection
        connect(clientManager_, &ClientManager::reconnected,
                this, [this]() {
            BOOST_LOG_SEV(lg(), info) << "Re-subscribing to reason change events";
            clientManager_->subscribeToEvent(std::string{reason_event_name});
        });

        // If already connected, subscribe now
        if (clientManager_->isConnected()) {
            BOOST_LOG_SEV(lg(), info) << "Already connected, subscribing to events";
            clientManager_->subscribeToEvent(std::string{reason_event_name});
        }
    }
}

ChangeReasonController::~ChangeReasonController() {
    BOOST_LOG_SEV(lg(), debug) << "ChangeReasonController destroyed";

    // Unsubscribe from events
    if (clientManager_) {
        BOOST_LOG_SEV(lg(), debug) << "Unsubscribing from reason change events";
        clientManager_->unsubscribeFromEvent(std::string{reason_event_name});
    }
}

void ChangeReasonController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "change_reasons");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new ChangeReasonMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &ChangeReasonMdiWindow::statusChanged,
            this, &ChangeReasonController::statusMessage);
    connect(listWindow_, &ChangeReasonMdiWindow::errorOccurred,
            this, &ChangeReasonController::errorMessage);
    connect(listWindow_, &ChangeReasonMdiWindow::showReasonDetails,
            this, &ChangeReasonController::onShowDetails);
    connect(listWindow_, &ChangeReasonMdiWindow::addNewRequested,
            this, &ChangeReasonController::onAddNewRequested);
    connect(listWindow_, &ChangeReasonMdiWindow::showReasonHistory,
            this, &ChangeReasonController::onShowHistory);

    // Create MDI subwindow
    const QColor iconColor(220, 220, 220);
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Change Reasons");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_note_edit_20_regular.svg", iconColor));
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

    BOOST_LOG_SEV(lg(), debug) << "Reason list window created";
}

void ChangeReasonController::closeAllWindows() {
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

void ChangeReasonController::onShowDetails(
    const dq::domain::change_reason& reason) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << reason.code;
    showDetailWindow(reason);
}

void ChangeReasonController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new change reason requested";
    showAddWindow();
}

void ChangeReasonController::onShowHistory(const QString& code) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << code.toStdString();
    showHistoryWindow(code);
}

void ChangeReasonController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new change reason";

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new ChangeReasonDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());

    // Load categories for the combo box
    if (changeReasonCache_ && changeReasonCache_->isLoaded()) {
        detailDialog->setCategories(changeReasonCache_->allCategories());
    }

    detailDialog->setCreateMode(true);

    connect(detailDialog, &ChangeReasonDetailDialog::statusMessage,
            this, &ChangeReasonController::statusMessage);
    connect(detailDialog, &ChangeReasonDetailDialog::errorMessage,
            this, &ChangeReasonController::errorMessage);
    connect(detailDialog, &ChangeReasonDetailDialog::changeReasonSaved,
            this, [this](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Change reason saved: " << code.toStdString();
        if (listWindow_) {
            listWindow_->markAsStale();
        }
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Change Reason");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_note_edit_20_regular.svg", iconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void ChangeReasonController::showDetailWindow(
    const dq::domain::change_reason& reason) {

    const QString identifier = QString::fromStdString(reason.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << reason.code;

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new ChangeReasonDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());

    // Load categories for the combo box
    if (changeReasonCache_ && changeReasonCache_->isLoaded()) {
        detailDialog->setCategories(changeReasonCache_->allCategories());
    }

    detailDialog->setCreateMode(false);
    detailDialog->setChangeReason(reason);

    connect(detailDialog, &ChangeReasonDetailDialog::statusMessage,
            this, &ChangeReasonController::statusMessage);
    connect(detailDialog, &ChangeReasonDetailDialog::errorMessage,
            this, &ChangeReasonController::errorMessage);
    connect(detailDialog, &ChangeReasonDetailDialog::changeReasonSaved,
            this, [this](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Change reason saved: " << code.toStdString();
        if (listWindow_) {
            listWindow_->markAsStale();
        }
    });
    connect(detailDialog, &ChangeReasonDetailDialog::changeReasonDeleted,
            this, [this, key](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Change reason deleted: " << code.toStdString();
        if (listWindow_) {
            listWindow_->markAsStale();
        }
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Change Reason: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_note_edit_20_regular.svg", iconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<ChangeReasonController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void ChangeReasonController::onNotificationReceived(
    const QString& eventType, const QDateTime& timestamp,
    const QStringList& entityIds) {

    if (eventType != QString::fromStdString(std::string{reason_event_name})) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Received reason change notification at "
                              << timestamp.toString(Qt::ISODate).toStdString()
                              << " with " << entityIds.size() << " reason codes";

    // Mark the list window as stale
    if (listWindow_) {
        listWindow_->markAsStale();
        BOOST_LOG_SEV(lg(), debug) << "Marked reason list as stale";
    }
}

void ChangeReasonController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for change reason: "
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

    auto* historyDialog = new ChangeReasonHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog, &ChangeReasonHistoryDialog::statusChanged,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(historyDialog, &ChangeReasonHistoryDialog::errorOccurred,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });
    connect(historyDialog, &ChangeReasonHistoryDialog::revertVersionRequested,
            this, &ChangeReasonController::onRevertVersion);
    connect(historyDialog, &ChangeReasonHistoryDialog::openVersionRequested,
            this, &ChangeReasonController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Change Reason History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<ChangeReasonController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void ChangeReasonController::onOpenVersion(
    const dq::domain::change_reason& reason, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for change reason: " << reason.code;

    const QString code = QString::fromStdString(reason.code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new ChangeReasonDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setChangeReason(reason);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &ChangeReasonDetailDialog::statusMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(detailDialog, &ChangeReasonDetailDialog::errorMessage,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Change Reason: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<ChangeReasonController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void ChangeReasonController::onRevertVersion(
    const dq::domain::change_reason& reason) {
    BOOST_LOG_SEV(lg(), info) << "Reverting change reason to version: "
                              << reason.version;

    // The history dialog already shows the confirmation and prepares the data.
    // We just need to save it as a new version.
    auto* detailDialog = new ChangeReasonDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());

    // Load categories for the combo box
    if (changeReasonCache_ && changeReasonCache_->isLoaded()) {
        detailDialog->setCategories(changeReasonCache_->allCategories());
    }

    detailDialog->setChangeReason(reason);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &ChangeReasonDetailDialog::statusMessage,
            this, &ChangeReasonController::statusMessage);
    connect(detailDialog, &ChangeReasonDetailDialog::errorMessage,
            this, &ChangeReasonController::errorMessage);
    connect(detailDialog, &ChangeReasonDetailDialog::changeReasonSaved,
            this, [this](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Change reason reverted: " << code.toStdString();
        emit statusMessage(QString("Change reason '%1' reverted successfully").arg(code));
        if (listWindow_) {
            listWindow_->markAsStale();
        }
    });

    const QColor iconColor(220, 220, 220);
    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Change Reason: %1")
        .arg(QString::fromStdString(reason.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_rotate_counterclockwise_20_regular.svg", iconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

}
