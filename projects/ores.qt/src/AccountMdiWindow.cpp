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
#include "ores.qt/AccountMdiWindow.hpp"

#include <vector>
#include <QtCore/QVariant>
#include <QtCore/QTimer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QtWidgets/QWidget>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QTableView>
#include <QtWidgets/QScrollBar>
#include <QtWidgets/QApplication>
#include <QtWidgets/QPushButton>
#include <QMessageBox>
#include <QToolBar>
#include <QAction>
#include <QPixmap>
#include <QSortFilterProxyModel>
#include <QImage>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.accounts/messaging/account_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using comms::messaging::message_type;
using namespace ores::utility::log;

AccountMdiWindow::
AccountMdiWindow(ClientManager* clientManager,
                 const QString& username,
                 QWidget* parent)
    : QWidget(parent),
      verticalLayout_(new QVBoxLayout(this)),
      accountTableView_(new QTableView(this)),
      toolBar_(new QToolBar(this)),
      pagination_widget_(new PaginationWidget(this)),
      reloadAction_(new QAction("Reload", this)),
      pulseTimer_(new QTimer(this)),
      addAction_(new QAction("Add", this)),
      editAction_(new QAction("Edit", this)),
      deleteAction_(new QAction("Delete", this)),
      lockAction_(new QAction("Lock", this)),
      unlockAction_(new QAction("Unlock", this)),
      accountModel_(std::make_unique<ClientAccountModel>(clientManager)),
      proxyModel_(new QSortFilterProxyModel(this)),
      clientManager_(clientManager),
      username_(username) {

    BOOST_LOG_SEV(lg(), debug) << "Creating account MDI window";

    toolBar_->setMovable(false);
    toolBar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    const QColor iconColor(220, 220, 220);

    // Setup reload action with normal and stale icons
    setupReloadAction();
    toolBar_->addAction(reloadAction_);

    toolBar_->addSeparator();

    addAction_->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_add_20_regular.svg", iconColor));
    addAction_->setToolTip("Add new account");
    connect(addAction_, &QAction::triggered, this, &AccountMdiWindow::addNew);
    toolBar_->addAction(addAction_);

    editAction_->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_edit_20_regular.svg", iconColor));
    editAction_->setToolTip("Edit selected account");
    connect(editAction_, &QAction::triggered, this,
        &AccountMdiWindow::editSelected);
    toolBar_->addAction(editAction_);

    deleteAction_->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_delete_20_regular.svg", iconColor));
    deleteAction_->setToolTip("Delete selected account(s)");
    connect(deleteAction_, &QAction::triggered, this,
        &AccountMdiWindow::deleteSelected);
    toolBar_->addAction(deleteAction_);

    toolBar_->addSeparator();

    lockAction_->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_lock_closed_20_filled.svg", iconColor));
    lockAction_->setToolTip("Lock selected account");
    connect(lockAction_, &QAction::triggered, this,
        &AccountMdiWindow::lockSelected);
    toolBar_->addAction(lockAction_);

    unlockAction_->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_lock_open_20_filled.svg", iconColor));
    unlockAction_->setToolTip("Unlock selected account");
    connect(unlockAction_, &QAction::triggered, this,
        &AccountMdiWindow::unlockSelected);
    toolBar_->addAction(unlockAction_);

    verticalLayout_->addWidget(toolBar_);
    verticalLayout_->addWidget(accountTableView_);
    verticalLayout_->addWidget(pagination_widget_);

    accountTableView_->setObjectName("accountTableView");
    accountTableView_->setAlternatingRowColors(true);
    accountTableView_->setSelectionMode(QAbstractItemView::ExtendedSelection);
    accountTableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    accountTableView_->setWordWrap(false);

    proxyModel_->setSourceModel(accountModel_.get());
    accountTableView_->setModel(proxyModel_);
    accountTableView_->setSortingEnabled(true);
    accountTableView_->sortByColumn(0, Qt::AscendingOrder);

    QHeaderView* verticalHeader(accountTableView_->verticalHeader());
    QHeaderView* horizontalHeader(accountTableView_->horizontalHeader());

    verticalHeader->setDefaultSectionSize(24);
    verticalHeader->setSectionResizeMode(QHeaderView::Fixed);
    horizontalHeader->setSectionResizeMode(QHeaderView::ResizeToContents);

    // Connect signals
    connect(accountModel_.get(), &ClientAccountModel::dataLoaded,
            this, &AccountMdiWindow::onDataLoaded);
    connect(accountModel_.get(), &ClientAccountModel::loadError,
            this, &AccountMdiWindow::onLoadError);
    connect(accountTableView_, &QTableView::doubleClicked,
            this, &AccountMdiWindow::onRowDoubleClicked);
    connect(accountTableView_->selectionModel(),
        &QItemSelectionModel::selectionChanged,
            this, &AccountMdiWindow::onSelectionChanged);

    // Connect pagination widget signals
    connect(pagination_widget_, &PaginationWidget::page_size_changed,
            this, [this](std::uint32_t size) {
        BOOST_LOG_SEV(lg(), debug) << "Page size changed to: " << size;
        accountModel_->set_page_size(size);
        accountModel_->refresh(true);
    });

    connect(pagination_widget_, &PaginationWidget::load_all_requested,
            this, [this]() {
        BOOST_LOG_SEV(lg(), debug) << "Load all requested from pagination widget";
        const auto total = accountModel_->total_available_count();
        if (total > 0 && total <= 1000) {
            emit statusChanged("Loading all accounts...");
            accountModel_->set_page_size(total);
            accountModel_->refresh(true);
        } else if (total > 1000) {
            BOOST_LOG_SEV(lg(), warn) << "Total count " << total
                                      << " exceeds maximum page size of 1000";
            emit statusChanged("Cannot load all - too many records (max 1000)");
        }
    });

    // Connect connection state signals
    if (clientManager_) {
        connect(clientManager_, &ClientManager::connected, this, &AccountMdiWindow::onConnectionStateChanged);
        connect(clientManager_, &ClientManager::disconnected, this, &AccountMdiWindow::onConnectionStateChanged);
    }

    updateActionStates();

    emit statusChanged("Loading accounts...");

    // Initial load
    if (clientManager_->isConnected()) {
        accountModel_->refresh();
    } else {
        emit statusChanged("Disconnected - Offline");
        toolBar_->setEnabled(false);
    }
}

AccountMdiWindow::~AccountMdiWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Destroying account MDI window";

    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
        watcher->waitForFinished();
    }
}

void AccountMdiWindow::onConnectionStateChanged() {
    const bool connected = clientManager_ && clientManager_->isConnected();
    toolBar_->setEnabled(connected);

    if (connected) {
        emit statusChanged("Connected");
    } else {
        emit statusChanged("Disconnected - Offline");
    }
}

void AccountMdiWindow::reload() {
    BOOST_LOG_SEV(lg(), debug) << "Reload requested";
    if (!clientManager_->isConnected()) {
        emit statusChanged("Cannot reload - Disconnected");
        return;
    }
    emit statusChanged("Reloading accounts...");
    clearStaleIndicator();
    accountModel_->refresh();
}

void AccountMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new account requested";
    emit addNewRequested();
}

void AccountMdiWindow::onDataLoaded() {
    const auto loaded = accountModel_->rowCount();
    const auto total = accountModel_->total_available_count();

    pagination_widget_->update_state(loaded, total);

    const bool has_more = loaded < total && total > 0 && total <= 1000;
    BOOST_LOG_SEV(lg(), debug) << "onDataLoaded: loaded=" << loaded
                               << ", total=" << total
                               << ", has_more=" << has_more;
    pagination_widget_->set_load_all_enabled(has_more);

    const QString message = QString("Loaded %1 of %2 accounts")
                              .arg(loaded)
                              .arg(total);
    emit statusChanged(message);
    BOOST_LOG_SEV(lg(), debug) << "Account data loaded successfully: "
                             << loaded << " of " << total << " accounts";

    if (accountModel_->rowCount() > 0 &&
        accountTableView_->selectionModel()->selectedRows().isEmpty()) {
        accountTableView_->selectRow(0);
        BOOST_LOG_SEV(lg(), debug) << "Auto-selected first row";
    }
}

void AccountMdiWindow::onLoadError(const QString& error_message) {
    emit errorOccurred(error_message);
    BOOST_LOG_SEV(lg(), error) << "Error loading accounts: "
                              << error_message.toStdString();

    MessageBoxHelper::critical(this, "Load Error", error_message);
}

void AccountMdiWindow::onRowDoubleClicked(const QModelIndex& index) {
    if (!index.isValid()) {
        return;
    }

    // Map proxy index to source index
    const QModelIndex sourceIndex = proxyModel_->mapToSource(index);
    const auto* account = accountModel_->getAccount(sourceIndex.row());
    if (!account) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get account for row: "
                                 << sourceIndex.row();
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Emitting showAccountDetails for account: "
                             << account->username;
    emit showAccountDetails(*account);
}

void AccountMdiWindow::onSelectionChanged() {
    const int selection_count = accountTableView_->selectionModel()->selectedRows().count();
    updateActionStates();
    emit selectionChanged(selection_count);
}

void AccountMdiWindow::editSelected() {
    const auto selected = accountTableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    onRowDoubleClicked(selected.first());
}

void AccountMdiWindow::deleteSelected() {
    const auto selected = accountTableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
         MessageBoxHelper::warning(this, "Disconnected", "Cannot delete account while disconnected.");
         return;
    }

    std::vector<boost::uuids::uuid> account_ids;
    for (const auto& index : selected) {
        // Map proxy index to source index
        const QModelIndex sourceIndex = proxyModel_->mapToSource(index);
        const auto* account = accountModel_->getAccount(sourceIndex.row());
        if (account)
            account_ids.push_back(account->id);
    }

    if (account_ids.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid accounts to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for " << account_ids.size()
                               << " accounts";

    QString confirmMessage;
    if (account_ids.size() == 1) {
        // Map proxy index to source index
        const QModelIndex sourceIndex = proxyModel_->mapToSource(selected.first());
        const auto* account = accountModel_->getAccount(sourceIndex.row());
        confirmMessage = QString("Are you sure you want to delete account '%1'?")
            .arg(QString::fromStdString(account->username));
    } else {
        confirmMessage = QString("Are you sure you want to delete %1 accounts?")
            .arg(account_ids.size());
    }

    auto reply = MessageBoxHelper::question(this, "Delete Account",
        confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user.";
        return;
    }

    QPointer<AccountMdiWindow> self = this;
    using DeleteResult = std::vector<std::pair<boost::uuids::uuid, std::pair<bool, std::string>>>;

    auto task = [self, account_ids]() -> DeleteResult {
        DeleteResult results;
        if (!self) return {};

        for (const auto& account_id : account_ids) {
            BOOST_LOG_SEV(lg(), debug) << "Deleting account: "
                                       << boost::uuids::to_string(account_id);

            accounts::messaging::delete_account_request request{account_id};
            auto payload = request.serialize();

            comms::messaging::frame request_frame(
                message_type::delete_account_request,
                0, std::move(payload)
            );

            auto response_result = self->clientManager_->sendRequest(
                std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send delete request";
                results.push_back({account_id,
                    {false, "Failed to communicate with server"}});
                continue;
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response";
                results.push_back({account_id,
                    {false, "Failed to decompress server response"}});
                continue;
            }

            auto response = accounts::messaging::delete_account_response::
                deserialize(*payload_result);

            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
                results.push_back({account_id,
                    {false, "Invalid server response"}});
                continue;
            }

            results.push_back({account_id,
                {response->success, response->message}});
        }

        return results;
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, watcher]() {
        auto results = watcher->result();
        watcher->deleteLater();

        int success_count = 0;
        int failure_count = 0;
        QString first_error;

        for (const auto& [account_id, result] : results) {
            auto [success, message] = result;

            if (success) {
                BOOST_LOG_SEV(lg(), debug) << "Account deleted successfully: "
                                           << boost::uuids::to_string(account_id);
                success_count++;
                emit self->accountDeleted(account_id);
            } else {
                BOOST_LOG_SEV(lg(), error) << "Account deletion failed: "
                                           << boost::uuids::to_string(account_id)
                                           << " - " << message;
                failure_count++;
                if (first_error.isEmpty()) {
                    first_error = QString::fromStdString(message);
                }
            }
        }

        self->accountModel_->refresh();
        if (failure_count == 0) {
            QString msg = success_count == 1
                ? "Successfully deleted 1 account"
                : QString("Successfully deleted %1 accounts").arg(success_count);
            emit self->statusChanged(msg);
        } else if (success_count == 0) {
            QString msg = QString("Failed to delete %1 %2: %3")
                .arg(failure_count)
                .arg(failure_count == 1 ? "account" : "accounts")
                .arg(first_error);
            emit self->errorOccurred(msg);
            MessageBoxHelper::critical(self, "Delete Failed", msg);
        } else {
            QString msg = QString("Deleted %1 %2, failed to delete %3 %4")
                .arg(success_count)
                .arg(success_count == 1 ? "account" : "accounts")
                .arg(failure_count)
                .arg(failure_count == 1 ? "account" : "accounts");
            emit self->statusChanged(msg);
            MessageBoxHelper::warning(self, "Partial Success", msg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void AccountMdiWindow::lockSelected() {
    const auto selected = accountTableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Lock requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
         MessageBoxHelper::warning(this, "Disconnected", "Cannot lock account while disconnected.");
         return;
    }

    // Map proxy index to source index
    const QModelIndex sourceIndex = proxyModel_->mapToSource(selected.first());
    const auto* account = accountModel_->getAccount(sourceIndex.row());
    if (!account) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get account for lock";
        return;
    }

    QString confirmMessage = QString("Are you sure you want to lock account '%1'?")
        .arg(QString::fromStdString(account->username));

    auto reply = MessageBoxHelper::question(this, "Lock Account",
        confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    QPointer<AccountMdiWindow> self = this;
    boost::uuids::uuid account_id = account->id;

    auto task = [self, account_id]() -> std::pair<bool, std::string> {
        if (!self) return {false, "Window closed"};

        accounts::messaging::lock_account_request request{account_id};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            message_type::lock_account_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {false, "Failed to decompress server response"};
        }

        auto response = accounts::messaging::lock_account_response::
            deserialize(*payload_result);

        if (!response) {
            return {false, "Invalid server response"};
        }

        return {response->success, response->error_message};
    };

    auto* watcher = new QFutureWatcher<std::pair<bool, std::string>>(self);
    connect(watcher, &QFutureWatcher<std::pair<bool, std::string>>::finished,
            self, [self, watcher]() {
        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            emit self->statusChanged("Account locked successfully");
            self->accountModel_->refresh();
        } else {
            QString msg = QString("Failed to lock account: %1")
                .arg(QString::fromStdString(message));
            emit self->errorOccurred(msg);
            MessageBoxHelper::critical(self, "Lock Failed", msg);
        }
    });

    QFuture<std::pair<bool, std::string>> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void AccountMdiWindow::unlockSelected() {
    const auto selected = accountTableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Unlock requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
         MessageBoxHelper::warning(this, "Disconnected", "Cannot unlock account while disconnected.");
         return;
    }

    // Map proxy index to source index
    const QModelIndex sourceIndex = proxyModel_->mapToSource(selected.first());
    const auto* account = accountModel_->getAccount(sourceIndex.row());
    if (!account) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get account for unlock";
        return;
    }

    QString confirmMessage = QString("Are you sure you want to unlock account '%1'?")
        .arg(QString::fromStdString(account->username));

    auto reply = MessageBoxHelper::question(this, "Unlock Account",
        confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    QPointer<AccountMdiWindow> self = this;
    boost::uuids::uuid account_id = account->id;

    auto task = [self, account_id]() -> std::pair<bool, std::string> {
        if (!self) return {false, "Window closed"};

        accounts::messaging::unlock_account_request request{account_id};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            message_type::unlock_account_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {false, "Failed to decompress server response"};
        }

        auto response = accounts::messaging::unlock_account_response::
            deserialize(*payload_result);

        if (!response) {
            return {false, "Invalid server response"};
        }

        return {response->success, response->error_message};
    };

    auto* watcher = new QFutureWatcher<std::pair<bool, std::string>>(self);
    connect(watcher, &QFutureWatcher<std::pair<bool, std::string>>::finished,
            self, [self, watcher]() {
        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            emit self->statusChanged("Account unlocked successfully");
            self->accountModel_->refresh();
        } else {
            QString msg = QString("Failed to unlock account: %1")
                .arg(QString::fromStdString(message));
            emit self->errorOccurred(msg);
            MessageBoxHelper::critical(self, "Unlock Failed", msg);
        }
    });

    QFuture<std::pair<bool, std::string>> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

QSize AccountMdiWindow::sizeHint() const {
    const int minimumWidth = 800;
    const int minimumHeight = 500;

    QSize baseSize = QWidget::sizeHint();

    return { qMax(baseSize.width(), minimumWidth),
             qMax(baseSize.height(), minimumHeight) };
}

void AccountMdiWindow::updateActionStates() {
    const int selection_count = accountTableView_
        ->selectionModel()->selectedRows().count();
    const bool hasSelection = selection_count > 0;

    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    lockAction_->setEnabled(hasSelection);
    unlockAction_->setEnabled(hasSelection);
}

void AccountMdiWindow::setupReloadAction() {
    const QColor normalColor(220, 220, 220);
    const QColor staleColor(255, 165, 0);

    normalReloadIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_clockwise_16_regular.svg", normalColor);
    staleReloadIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_clockwise_16_regular.svg", staleColor);

    reloadAction_->setIcon(normalReloadIcon_);
    reloadAction_->setToolTip("Reload accounts from server");
    connect(reloadAction_, &QAction::triggered, this, &AccountMdiWindow::reload);

    connect(pulseTimer_, &QTimer::timeout, this, [this]() {
        pulseState_ = !pulseState_;
        reloadAction_->setIcon(pulseState_ ? staleReloadIcon_ : normalReloadIcon_);

        pulseCount_++;
        if (pulseCount_ >= 6) {
            pulseTimer_->stop();
            reloadAction_->setIcon(staleReloadIcon_);
        }
    });
}

void AccountMdiWindow::startPulseAnimation() {
    pulseCount_ = 0;
    pulseState_ = false;
    pulseTimer_->start(500);
}

void AccountMdiWindow::stopPulseAnimation() {
    pulseTimer_->stop();
    reloadAction_->setIcon(normalReloadIcon_);
}

void AccountMdiWindow::markAsStale() {
    if (!isStale_) {
        isStale_ = true;
        reloadAction_->setToolTip("Data changed on server - click to reload");
        startPulseAnimation();
        BOOST_LOG_SEV(lg(), info) << "Account data marked as stale";
    }
}

void AccountMdiWindow::clearStaleIndicator() {
    if (isStale_) {
        isStale_ = false;
        stopPulseAnimation();
        reloadAction_->setToolTip("Reload accounts from server");
        BOOST_LOG_SEV(lg(), debug) << "Stale indicator cleared";
    }
}

}
