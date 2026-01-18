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
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/AccountMdiWindow.hpp"
#include "ores.qt/AccountDetailDialog.hpp"
#include "ores.qt/AccountHistoryDialog.hpp"
#include "ores.qt/SessionHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.iam/eventing/account_changed_event.hpp"
#include "ores.iam/messaging/account_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using comms::messaging::frame;
using comms::messaging::message_type;
using namespace ores::logging;

namespace {
    // Event type name for account changes
    constexpr std::string_view account_event_name =
        eventing::domain::event_traits<iam::eventing::account_changed_event>::name;
}

AccountController::AccountController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, parent),
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
    connect(accountWidget, &AccountMdiWindow::showSessionHistory,
            this, &AccountController::onShowSessionHistory);

    accountListWindow_ = new DetachableMdiSubWindow();
    accountListWindow_->setAttribute(Qt::WA_DeleteOnClose);
    accountListWindow_->setWidget(accountWidget);
    accountListWindow_->setWindowTitle("Accounts");
    accountListWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_person_accounts_20_regular.svg", iconColor));

    // Track window for detach/reattach operations
    register_detachable_window(accountListWindow_);
    QPointer<AccountController> self = this;
    QPointer<DetachableMdiSubWindow> windowBeingDestroyed = accountListWindow_;
    connect(accountListWindow_, &QObject::destroyed, this,
        [self, windowBeingDestroyed]() {
        if (!self)
            return;

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

void AccountController::reloadListWindow() {
    if (accountListWindow_) {
        if (auto* widget = qobject_cast<AccountMdiWindow*>(accountListWindow_->widget())) {
            widget->reload();
        }
    }
}

void AccountController::onNotificationReceived(
    const QString& eventType, const QDateTime& timestamp,
    const QStringList& entityIds) {
    // Check if this is an account change event
    if (eventType != QString::fromStdString(std::string{account_event_name})) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Received account change notification at "
                              << timestamp.toString(Qt::ISODate).toStdString()
                              << " with " << entityIds.size() << " account IDs";

    // If the account list window is open, mark it as stale
    if (accountListWindow_) {
        auto* accountWidget = qobject_cast<AccountMdiWindow*>(
            accountListWindow_->widget());
        if (accountWidget) {
            accountWidget->markAsStale();
            BOOST_LOG_SEV(lg(), debug) << "Marked account window as stale";
        }
    }

    // Notify open detail/history dialogs for affected accounts
    // Iterate over managed windows to find matching dialogs
    for (auto* window : managed_windows_) {
        if (!window || window == accountListWindow_)
            continue;

        // Check if it's an account detail dialog
        if (auto* detailDialog = qobject_cast<AccountDetailDialog*>(window->widget())) {
            const QString dialogAccountId = detailDialog->accountId();

            // Mark as stale if no specific IDs (broadcast) or if this account matches
            if (entityIds.isEmpty() || entityIds.contains(dialogAccountId)) {
                detailDialog->markAsStale();
                BOOST_LOG_SEV(lg(), debug) << "Marked account detail dialog as stale for: "
                                           << dialogAccountId.toStdString();
            }
            continue;
        }

        // Check if it's an account history dialog
        if (auto* historyDialog = qobject_cast<AccountHistoryDialog*>(window->widget())) {
            // History dialogs are always marked stale on account changes
            // since we don't have easy access to account_id
            historyDialog->markAsStale();
            BOOST_LOG_SEV(lg(), debug) << "Marked account history dialog as stale for: "
                                       << historyDialog->username().toStdString();
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

    // Connect open and revert signals
    connect(historyDialog, &AccountHistoryDialog::openVersionRequested,
            this, &AccountController::onOpenAccountVersion);
    connect(historyDialog, &AccountHistoryDialog::revertVersionRequested,
            this, &AccountController::onRevertAccount);

    // Create and configure window
    auto* historyWindow = new DetachableMdiSubWindow();
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Account History: %1").arg(username));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    // Track window for cleanup
    register_detachable_window(historyWindow);

    show_managed_window(historyWindow, accountListWindow_);

    // Load the history data
    historyDialog->loadHistory();
}

void AccountController::onShowSessionHistory(const boost::uuids::uuid& accountId,
                                              const QString& username) {
    BOOST_LOG_SEV(lg(), info) << "Showing session history for: "
                             << username.toStdString();

    const QColor iconColor(220, 220, 220);

    auto* sessionDialog = new SessionHistoryDialog(clientManager_, mainWindow_);

    // Connect status signals
    connect(sessionDialog, &SessionHistoryDialog::statusMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(sessionDialog, &SessionHistoryDialog::errorMessage,
            this, [this](const QString& err_msg) {
        emit errorMessage(err_msg);
    });

    // Create and configure window
    auto* sessionWindow = new DetachableMdiSubWindow();
    sessionWindow->setAttribute(Qt::WA_DeleteOnClose);
    sessionWindow->setWidget(sessionDialog);
    sessionWindow->setWindowTitle(QString("Session History: %1").arg(username));
    sessionWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_clock_16_regular.svg", iconColor));

    // Track window for cleanup
    register_detachable_window(sessionWindow);

    show_managed_window(sessionWindow, accountListWindow_);

    // Load the session data for this account
    sessionDialog->setAccount(accountId, username);
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
        iam::domain::account empty_account;
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
    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, accountListWindow_);
}

void AccountController::onOpenAccountVersion(
    const iam::domain::account& account, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for account: " << account.username;

    const QString username = QString::fromStdString(account.username);
    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new AccountDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());

    connect(detailDialog, &AccountDetailDialog::statusMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(detailDialog, &AccountDetailDialog::errorMessage,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });

    // Connect revert signal
    connect(detailDialog, &AccountDetailDialog::revertRequested,
            this, &AccountController::onRevertAccount);

    detailDialog->setAccount(account);
    detailDialog->setLoginInfo(std::nullopt);  // No login info for historical versions
    detailDialog->setReadOnly(true, versionNumber);

    auto* detailWindow = new DetachableMdiSubWindow();
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Account: %1 (Version %2 - Read Only)")
        .arg(username).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_person_accounts_20_regular.svg", iconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, accountListWindow_);
}

void AccountController::onRevertAccount(const iam::domain::account& account) {
    BOOST_LOG_SEV(lg(), info) << "Reverting account: " << account.username;

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Revert requested but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    // Create update request with the historical data
    QPointer<AccountController> self = this;
    const boost::uuids::uuid account_id = account.id;
    const std::string username = account.username;
    const std::string email = account.email;
    const std::string recorded_by = username_.toStdString();

    QFuture<std::pair<bool, std::string>> future =
        QtConcurrent::run([self, account_id, email, recorded_by]()
            -> std::pair<bool, std::string> {
            if (!self) return {false, ""};

            BOOST_LOG_SEV(lg(), debug) << "Sending update account request for revert: "
                                       << boost::uuids::to_string(account_id);

            iam::messaging::save_account_request request;
            request.account_id = account_id;
            request.email = email;
            request.recorded_by = recorded_by;

            auto payload = request.serialize();
            frame request_frame(message_type::save_account_request,
                0, std::move(payload));

            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result)
                return {false, "Failed to communicate with server"};

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result)
                return {false, "Failed to decompress server response"};

            auto response = iam::messaging::save_account_response::
                deserialize(*payload_result);

            if (!response)
                return {false, "Invalid server response"};

            return {response->success, response->message};
        });

    auto* watcher = new QFutureWatcher<std::pair<bool, std::string>>(self);
    connect(watcher, &QFutureWatcher<std::pair<bool, std::string>>::finished, self,
        [self, watcher, username]() {

        if (!self) return;

        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), debug) << "Account reverted successfully.";
            emit self->statusMessage(QString("Successfully reverted account: %1")
                .arg(QString::fromStdString(username)));
            self->markAccountListAsStale();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Account revert failed: " << message;
            emit self->errorMessage(QString("Failed to revert account: %1")
                .arg(QString::fromStdString(message)));
            MessageBoxHelper::critical(self->mainWindow_, "Revert Failed",
                QString::fromStdString(message));
        }
    });

    watcher->setFuture(future);
}

}
