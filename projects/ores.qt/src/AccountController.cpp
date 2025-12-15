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
#include "ores.qt/AccountController.hpp"

#include <QPointer>
#include <QLabel>
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.accounts/domain/events/account_changed_event.hpp"

namespace ores::qt {

using namespace ores::utility::log;

namespace {
    // Event type name for account changes
    constexpr std::string_view account_event_name =
        eventing::domain::event_traits<accounts::domain::events::account_changed_event>::name;
}

AccountController::AccountController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QList<DetachableMdiSubWindow*>& allDetachableWindows,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, parent),
      allDetachableWindows_(allDetachableWindows),
      accountListWindow_(nullptr) {
    BOOST_LOG_SEV(lg(), debug) << "Account controller created";

    // Connect to notification signal from ClientManager
    if (clientManager_) {
        connect(clientManager_, &ClientManager::notificationReceived,
                this, &AccountController::onNotificationReceived);

        // Subscribe to events when connected (event adapter only available after login)
        connect(clientManager_, &ClientManager::connected,
                this, [this]() {
            BOOST_LOG_SEV(lg(), info) << "Subscribing to account change events";
            clientManager_->subscribeToEvent(std::string{account_event_name});
        });

        // Re-subscribe after reconnection
        connect(clientManager_, &ClientManager::reconnected,
                this, [this]() {
            BOOST_LOG_SEV(lg(), info) << "Re-subscribing to account change events after reconnect";
            clientManager_->subscribeToEvent(std::string{account_event_name});
        });

        // If already connected, subscribe now
        if (clientManager_->isConnected()) {
            BOOST_LOG_SEV(lg(), info) << "Already connected, subscribing to account change events";
            clientManager_->subscribeToEvent(std::string{account_event_name});
        }
    }
}

AccountController::~AccountController() {
    BOOST_LOG_SEV(lg(), debug) << "Account controller destroyed";

    // Unsubscribe from account change events
    if (clientManager_) {
        BOOST_LOG_SEV(lg(), debug) << "Unsubscribing from account change events";
        clientManager_->unsubscribeFromEvent(std::string{account_event_name});
    }
}

void AccountController::showListWindow() {
    // Reuse existing window if it exists
    if (accountListWindow_) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing accounts window";

        // Bring window to front
        if (accountListWindow_->isDetached()) {
            accountListWindow_->setVisible(true);
            accountListWindow_->show();
            accountListWindow_->raise();
            accountListWindow_->activateWindow();
        } else {
            accountListWindow_->setVisible(true);
            mdiArea_->setActiveSubWindow(accountListWindow_);
            accountListWindow_->show();
            accountListWindow_->raise();
        }
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new accounts MDI window";
    const QColor iconColor(220, 220, 220);

    // For now, create a placeholder widget until AccountMdiWindow is implemented
    auto* placeholderWidget = new QLabel("Account Management\n\nComing soon...");
    placeholderWidget->setAlignment(Qt::AlignCenter);
    placeholderWidget->setMinimumSize(400, 300);

    accountListWindow_ = new DetachableMdiSubWindow();
    accountListWindow_->setAttribute(Qt::WA_DeleteOnClose);
    accountListWindow_->setWidget(placeholderWidget);
    accountListWindow_->setWindowTitle("Accounts");
    accountListWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_person_20_filled.svg", iconColor));

    // Track window for detach/reattach operations
    allDetachableWindows_.append(accountListWindow_);
    QPointer<AccountController> self = this;
    QPointer<DetachableMdiSubWindow> windowBeingDestroyed = accountListWindow_;
    connect(accountListWindow_, &QObject::destroyed, this,
        [self, windowBeingDestroyed]() {
        if (!self)
            return;

        if (!windowBeingDestroyed.isNull()) {
            self->allDetachableWindows_.removeAll(windowBeingDestroyed.data());
        }

        if (self->accountListWindow_ == windowBeingDestroyed)
            self->accountListWindow_ = nullptr;
    });

    mdiArea_->addSubWindow(accountListWindow_);
    accountListWindow_->adjustSize();
    accountListWindow_->show();
}

void AccountController::closeAllWindows() {
    if (accountListWindow_) {
        accountListWindow_->close();
    }
}

void AccountController::onNotificationReceived(
    const QString& eventType, const QDateTime& timestamp) {
    // Check if this is an account change event
    if (eventType != QString::fromStdString(std::string{account_event_name})) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Received account change notification at "
                              << timestamp.toString(Qt::ISODate).toStdString();

    // TODO: When AccountMdiWindow is implemented, mark it as stale here
    // if (accountListWindow_) {
    //     auto* accountWidget = qobject_cast<AccountMdiWindow*>(
    //         accountListWindow_->widget());
    //     if (accountWidget) {
    //         accountWidget->markAsStale();
    //         BOOST_LOG_SEV(lg(), debug) << "Marked account window as stale";
    //     }
    // }
}

}
