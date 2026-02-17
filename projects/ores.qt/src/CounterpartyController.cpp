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
#include "ores.qt/CounterpartyController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/CounterpartyMdiWindow.hpp"
#include "ores.qt/CounterpartyDetailDialog.hpp"
#include "ores.qt/CounterpartyHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.refdata/eventing/counterparty_changed_event.hpp"
#include "ores.refdata/eventing/counterparty_identifier_changed_event.hpp"
#include "ores.refdata/eventing/counterparty_contact_information_changed_event.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    constexpr std::string_view counterparty_event_name =
        eventing::domain::event_traits<refdata::eventing::counterparty_changed_event>::name;
    constexpr std::string_view counterparty_identifier_event_name =
        eventing::domain::event_traits<refdata::eventing::counterparty_identifier_changed_event>::name;
    constexpr std::string_view counterparty_contact_event_name =
        eventing::domain::event_traits<refdata::eventing::counterparty_contact_information_changed_event>::name;
}

CounterpartyController::CounterpartyController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    ImageCache* imageCache,
    ChangeReasonCache* changeReasonCache,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      imageCache_(imageCache),
      changeReasonCache_(changeReasonCache),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "CounterpartyController created";

    if (clientManager_) {
        connect(clientManager_, &ClientManager::notificationReceived,
                this, &CounterpartyController::onNotificationReceived);

        auto subscribeAll = [self = QPointer<CounterpartyController>(this)]() {
            if (!self) return;
            BOOST_LOG_SEV(lg(), info) << "Subscribing to counterparty change events";
            self->clientManager_->subscribeToEvent(std::string{counterparty_event_name});
            self->clientManager_->subscribeToEvent(std::string{counterparty_identifier_event_name});
            self->clientManager_->subscribeToEvent(std::string{counterparty_contact_event_name});
        };

        connect(clientManager_, &ClientManager::loggedIn, this, subscribeAll);
        connect(clientManager_, &ClientManager::reconnected, this, subscribeAll);

        if (clientManager_->isConnected()) {
            subscribeAll();
        }
    }
}

CounterpartyController::~CounterpartyController() {
    BOOST_LOG_SEV(lg(), debug) << "CounterpartyController destroyed";
    if (clientManager_) {
        clientManager_->unsubscribeFromEvent(std::string{counterparty_event_name});
        clientManager_->unsubscribeFromEvent(std::string{counterparty_identifier_event_name});
        clientManager_->unsubscribeFromEvent(std::string{counterparty_contact_event_name});
    }
}

void CounterpartyController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "counterparties");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new CounterpartyMdiWindow(clientManager_, imageCache_, username_);

    // Connect signals
    connect(listWindow_, &CounterpartyMdiWindow::statusChanged,
            this, &CounterpartyController::statusMessage);
    connect(listWindow_, &CounterpartyMdiWindow::errorOccurred,
            this, &CounterpartyController::errorMessage);
    connect(listWindow_, &CounterpartyMdiWindow::showCounterpartyDetails,
            this, &CounterpartyController::onShowDetails);
    connect(listWindow_, &CounterpartyMdiWindow::addNewRequested,
            this, &CounterpartyController::onAddNewRequested);
    connect(listWindow_, &CounterpartyMdiWindow::showCounterpartyHistory,
            this, &CounterpartyController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Counterparties");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Handshake, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<CounterpartyController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Counterparty list window created";
}

void CounterpartyController::closeAllWindows() {
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

void CounterpartyController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void CounterpartyController::onShowDetails(
    const refdata::domain::counterparty& counterparty) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << counterparty.short_code;
    showDetailWindow(counterparty);
}

void CounterpartyController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new counterparty requested";
    showAddWindow();
}

void CounterpartyController::onShowHistory(
    const refdata::domain::counterparty& counterparty) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << counterparty.short_code;
    showHistoryWindow(counterparty);
}

void CounterpartyController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new counterparty";

    auto* detailDialog = new CounterpartyDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setImageCache(imageCache_);
    if (changeReasonCache_) {
        detailDialog->setChangeReasonCache(changeReasonCache_);
    }
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &CounterpartyDetailDialog::statusMessage,
            this, &CounterpartyController::statusMessage);
    connect(detailDialog, &CounterpartyDetailDialog::errorMessage,
            this, &CounterpartyController::errorMessage);
    connect(detailDialog, &CounterpartyDetailDialog::counterpartySaved,
            this, [self = QPointer<CounterpartyController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Counterparty saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Counterparty");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Handshake, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CounterpartyController::showDetailWindow(
    const refdata::domain::counterparty& counterparty) {

    const QString identifier = QString::fromStdString(counterparty.short_code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << counterparty.short_code;

    auto* detailDialog = new CounterpartyDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setImageCache(imageCache_);
    if (changeReasonCache_) {
        detailDialog->setChangeReasonCache(changeReasonCache_);
    }
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setCounterparty(counterparty);

    connect(detailDialog, &CounterpartyDetailDialog::statusMessage,
            this, &CounterpartyController::statusMessage);
    connect(detailDialog, &CounterpartyDetailDialog::errorMessage,
            this, &CounterpartyController::errorMessage);
    connect(detailDialog, &CounterpartyDetailDialog::counterpartySaved,
            this, [self = QPointer<CounterpartyController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Counterparty saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &CounterpartyDetailDialog::counterpartyDeleted,
            this, [self = QPointer<CounterpartyController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Counterparty deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Counterparty: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Handshake, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CounterpartyController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CounterpartyController::showHistoryWindow(
    const refdata::domain::counterparty& counterparty) {
    const QString code = QString::fromStdString(counterparty.short_code);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for counterparty: "
                              << counterparty.short_code;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << counterparty.short_code;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << counterparty.short_code;

    auto* historyDialog = new CounterpartyHistoryDialog(
        counterparty.id, code, clientManager_, mainWindow_);

    connect(historyDialog, &CounterpartyHistoryDialog::statusChanged,
            this, [self = QPointer<CounterpartyController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &CounterpartyHistoryDialog::errorOccurred,
            this, [self = QPointer<CounterpartyController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &CounterpartyHistoryDialog::revertVersionRequested,
            this, &CounterpartyController::onRevertVersion);
    connect(historyDialog, &CounterpartyHistoryDialog::openVersionRequested,
            this, &CounterpartyController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Counterparty History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<CounterpartyController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void CounterpartyController::onOpenVersion(
    const refdata::domain::counterparty& counterparty, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for counterparty: " << counterparty.short_code;

    const QString code = QString::fromStdString(counterparty.short_code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new CounterpartyDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setImageCache(imageCache_);
    if (changeReasonCache_) {
        detailDialog->setChangeReasonCache(changeReasonCache_);
    }
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCounterparty(counterparty);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &CounterpartyDetailDialog::statusMessage,
            this, [self = QPointer<CounterpartyController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &CounterpartyDetailDialog::errorMessage,
            this, [self = QPointer<CounterpartyController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Counterparty: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CounterpartyController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void CounterpartyController::onRevertVersion(
    const refdata::domain::counterparty& counterparty) {
    BOOST_LOG_SEV(lg(), info) << "Reverting counterparty to version: "
                              << counterparty.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new CounterpartyDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setImageCache(imageCache_);
    if (changeReasonCache_) {
        detailDialog->setChangeReasonCache(changeReasonCache_);
    }
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCounterparty(counterparty);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &CounterpartyDetailDialog::statusMessage,
            this, &CounterpartyController::statusMessage);
    connect(detailDialog, &CounterpartyDetailDialog::errorMessage,
            this, &CounterpartyController::errorMessage);
    connect(detailDialog, &CounterpartyDetailDialog::counterpartySaved,
            this, [self = QPointer<CounterpartyController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Counterparty reverted: " << code.toStdString();
        emit self->statusMessage(QString("Counterparty '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Counterparty: %1")
        .arg(QString::fromStdString(counterparty.short_code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* CounterpartyController::listWindow() const {
    return listWindow_;
}

void CounterpartyController::onNotificationReceived(
    const QString& eventType, const QDateTime& timestamp,
    const QStringList& entityIds, const QString& /*tenantId*/) {

    const auto eventStd = eventType.toStdString();
    if (eventStd != counterparty_event_name &&
        eventStd != counterparty_identifier_event_name &&
        eventStd != counterparty_contact_event_name) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Received " << eventStd << " notification at "
                              << timestamp.toString(Qt::ISODate).toStdString()
                              << " with " << entityIds.size() << " ids";

    if (listWindow_) {
        listWindow_->markAsStale();
        BOOST_LOG_SEV(lg(), debug) << "Marked counterparty list window as stale";
    }
}

}
