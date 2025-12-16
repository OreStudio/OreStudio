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
#include <boost/uuid/uuid.hpp>
#include "ores.qt/AccountMdiWindow.hpp"
#include "ores.qt/AccountDetailDialog.hpp"
#include "ores.qt/AccountHistoryDialog.hpp"
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

    auto* accountWidget = new AccountMdiWindow(clientManager_, username_, mainWindow_);

    // Connect status signals
    connect(accountWidget, &AccountMdiWindow::statusChanged,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(accountWidget, &AccountMdiWindow::errorOccurred,
            this, [this](const QString& err_msg) {
        emit errorMessage("Error: " + err_msg);
    });

    // Connect account operations
    connect(accountWidget, &AccountMdiWindow::addNewRequested,
            this, &AccountController::onAddNewRequested);
    connect(accountWidget, &AccountMdiWindow::showAccountDetails,
            this, &AccountController::onShowAccountDetails);
    connect(accountWidget, &AccountMdiWindow::showAccountHistory,
            this, &AccountController::onShowAccountHistory);

    accountListWindow_ = new DetachableMdiSubWindow();
    accountListWindow_->setAttribute(Qt::WA_DeleteOnClose);
    accountListWindow_->setWidget(accountWidget);
    accountListWindow_->setWindowTitle("Accounts");
    accountListWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_person_accounts_20_regular.svg", iconColor));

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

    // If the account list window is open, mark it as stale
    if (accountListWindow_) {
        auto* accountWidget = qobject_cast<AccountMdiWindow*>(
            accountListWindow_->widget());
        if (accountWidget) {
            accountWidget->markAsStale();
            BOOST_LOG_SEV(lg(), debug) << "Marked account window as stale";
        }
    }
}

void AccountController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new account requested";
    showDetailWindow(std::nullopt);
}

void AccountController::onShowAccountDetails(
    const AccountWithLoginInfo& accountWithLoginInfo) {
    BOOST_LOG_SEV(lg(), info) << "Showing account details for: "
                             << accountWithLoginInfo.account.username;
    showDetailWindow(accountWithLoginInfo);
}

void AccountController::onShowAccountHistory(const QString& username) {
    BOOST_LOG_SEV(lg(), info) << "Showing account history for: "
                             << username.toStdString();

    const QColor iconColor(220, 220, 220);

    auto* historyDialog = new AccountHistoryDialog(username, clientManager_, mainWindow_);

    // Connect status signals
    connect(historyDialog, &AccountHistoryDialog::statusChanged,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(historyDialog, &AccountHistoryDialog::errorOccurred,
            this, [this](const QString& err_msg) {
        emit errorMessage(err_msg);
    });

    // Create and configure window
    auto* historyWindow = new DetachableMdiSubWindow();
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Account History: %1").arg(username));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    // Track window for cleanup
    allDetachableWindows_.append(historyWindow);
    QPointer<AccountController> self = this;
    QPointer<DetachableMdiSubWindow> windowBeingDestroyed = historyWindow;
    connect(historyWindow, &QObject::destroyed, this,
        [self, windowBeingDestroyed]() {
        if (!self) return;
        if (!windowBeingDestroyed.isNull()) {
            self->allDetachableWindows_.removeAll(windowBeingDestroyed.data());
        }
    });

    mdiArea_->addSubWindow(historyWindow);
    historyWindow->adjustSize();
    historyWindow->show();

    // Load the history data
    historyDialog->loadHistory();
}

void AccountController::markAccountListAsStale() {
    if (accountListWindow_) {
        auto* accountWidget = qobject_cast<AccountMdiWindow*>(
            accountListWindow_->widget());
        if (accountWidget) {
            accountWidget->markAsStale();
        }
    }
}

void AccountController::showDetailWindow(
    const std::optional<AccountWithLoginInfo>& accountWithLoginInfo) {
    const bool isCreateMode = !accountWithLoginInfo.has_value();
    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new AccountDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());

    if (isCreateMode) {
        accounts::domain::account empty_account;
        detailDialog->setAccount(empty_account);
        detailDialog->setLoginInfo(std::nullopt);
    } else {
        detailDialog->setAccount(accountWithLoginInfo->account);
        detailDialog->setLoginInfo(accountWithLoginInfo->loginInfo);
    }

    // Connect common signals
    connect(detailDialog, &AccountDetailDialog::statusMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(detailDialog, &AccountDetailDialog::errorMessage,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });

    // Connect account change signals - all mark the list as stale
    connect(detailDialog, &AccountDetailDialog::accountCreated,
            this, [this](const boost::uuids::uuid&) { markAccountListAsStale(); });
    connect(detailDialog, &AccountDetailDialog::accountUpdated,
            this, [this](const boost::uuids::uuid&) { markAccountListAsStale(); });
    connect(detailDialog, &AccountDetailDialog::accountDeleted,
            this, [this](const boost::uuids::uuid&) { markAccountListAsStale(); });

    // Create and configure window
    auto* detailWindow = new DetachableMdiSubWindow();
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);

    if (isCreateMode) {
        detailWindow->setWindowTitle("New Account");
    } else {
        detailWindow->setWindowTitle(QString("Account: %1")
            .arg(QString::fromStdString(accountWithLoginInfo->account.username)));
    }

    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_person_accounts_20_regular.svg", iconColor));

    // Track window for cleanup
    allDetachableWindows_.append(detailWindow);
    QPointer<AccountController> self = this;
    QPointer<DetachableMdiSubWindow> windowBeingDestroyed = detailWindow;
    connect(detailWindow, &QObject::destroyed, this,
        [self, windowBeingDestroyed]() {
        if (!self) return;
        if (!windowBeingDestroyed.isNull()) {
            self->allDetachableWindows_.removeAll(windowBeingDestroyed.data());
        }
    });

    mdiArea_->addSubWindow(detailWindow);
    detailWindow->adjustSize();
    detailWindow->show();
}

}
