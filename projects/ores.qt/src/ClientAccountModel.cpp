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
#include "ores.qt/ClientAccountModel.hpp"

#include <algorithm>
#include <unordered_map>
#include <QtConcurrent>
#include <QColor>
#include <QFont>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.comms/net/client_session.hpp"
#include "ores.iam/messaging/account_protocol.hpp"
#include "ores.iam/messaging/login_protocol.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    std::string account_key_extractor(const iam::domain::account& a) {
        return a.username;
    }
}

ClientAccountModel::
ClientAccountModel(ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent), clientManager_(clientManager),
      watcher_(new QFutureWatcher<FutureWatcherResult>(this)),
      recencyTracker_(account_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_,
        &QFutureWatcher<FutureWatcherResult>::finished,
        this, &ClientAccountModel::onAccountsLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
        this, &ClientAccountModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
        this, &ClientAccountModel::onPulsingComplete);
}

int ClientAccountModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(accounts_.size());
}

int ClientAccountModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return Column::ColumnCount;
}

QVariant ClientAccountModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= accounts_.size())
        return {};

    const auto& item = accounts_[row];
    const auto& account = item.account;

    // Recency highlighting for foreground color
    if (role == Qt::ForegroundRole) {
        // Skip recency coloring for badge columns (handled by delegate)
        if (index.column() == Column::Status ||
            index.column() == Column::Locked) {
            return {};
        }
        auto recency_color = recency_foreground_color(account.username);
        if (recency_color.isValid())
            return recency_color;
        return {};
    }

    if (role != Qt::DisplayRole)
        return {};

    switch (index.column()) {
    case Column::Username: return QString::fromStdString(account.username);
    case Column::AccountType: return QString::fromStdString(account.account_type);
    case Column::Email: return QString::fromStdString(account.email);
    case Column::Status: {
        auto status = calculateLoginStatus(item.loginInfo);
        switch (status) {
        case LoginStatus::Never: return tr("Never");
        case LoginStatus::LongAgo: return tr("Old");
        case LoginStatus::Recent: return tr("Recent");
        case LoginStatus::Online: return tr("Online");
        }
        return tr("-");
    }
    case Column::Locked:
        if (item.loginInfo.has_value())
            return item.loginInfo->locked ? tr("Locked") : tr("-");
        return tr("-");
    case Column::Version: return account.version;
    case Column::ModifiedBy: return QString::fromStdString(account.modified_by);
    case Column::RecordedAt: return relative_time_helper::format(account.recorded_at);
    default: return {};
    }
}

QVariant ClientAccountModel::
headerData(int section, Qt::Orientation orientation, int role) const {
    if (role != Qt::DisplayRole)
        return {};

    if (orientation == Qt::Horizontal) {
        switch (section) {
        case Column::Username: return tr("Username");
        case Column::AccountType: return tr("Type");
        case Column::Email: return tr("Email");
        case Column::Status: return tr("Status");
        case Column::Locked: return tr("Locked");
        case Column::Version: return tr("Version");
        case Column::ModifiedBy: return tr("Modified By");
        case Column::RecordedAt: return tr("Recorded At");
        default: return {};
        }
    }

    return {};
}

void ClientAccountModel::refresh(bool /*replace*/) {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh account model: disconnected.";
        return;
    }

    if (!accounts_.empty()) {
        beginResetModel();
        accounts_.clear();
        total_available_count_ = 0;
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_accounts(0, page_size_);
}

void ClientAccountModel::load_page(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    // Clear existing data and load the requested page
    if (!accounts_.empty()) {
        beginResetModel();
        accounts_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_accounts(offset, limit);
}

void ClientAccountModel::fetch_accounts(std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientAccountModel> self = this;

    QFuture<FutureWatcherResult> future =
        QtConcurrent::run([self, offset, limit]() -> FutureWatcherResult {
            return exception_helper::wrap_async_fetch<FutureWatcherResult>([&]() -> FutureWatcherResult {
                BOOST_LOG_SEV(lg(), debug) << "Making an accounts request with offset="
                                           << offset << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false, .accounts = {}, .loginInfos = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                // Fetch accounts using typed request
                iam::messaging::get_accounts_request accounts_request;
                accounts_request.offset = offset;
                accounts_request.limit = limit;

                auto accounts_result = self->clientManager_->
                    process_authenticated_request(std::move(accounts_request));

                if (!accounts_result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to fetch accounts: "
                                               << comms::net::to_string(accounts_result.error());
                    return {.success = false, .accounts = {}, .loginInfos = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(
                                "Failed to fetch accounts: " + comms::net::to_string(accounts_result.error())),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug) << "Received " << accounts_result->accounts.size()
                                           << " accounts, total available: "
                                           << accounts_result->total_available_count;

                // Fetch login info using typed request
                iam::messaging::list_login_info_request login_info_request;

                std::vector<iam::domain::login_info> login_infos;
                auto login_info_result = self->clientManager_->
                    process_authenticated_request(std::move(login_info_request));

                if (login_info_result) {
                    login_infos = std::move(login_info_result->login_infos);
                    BOOST_LOG_SEV(lg(), debug) << "Received " << login_infos.size()
                                               << " login info records";
                } else {
                    BOOST_LOG_SEV(lg(), warn) << "Failed to fetch login info: "
                                              << comms::net::to_string(login_info_result.error());
                }

                return {.success = true, .accounts = std::move(accounts_result->accounts),
                        .loginInfos = std::move(login_infos),
                        .total_available_count = accounts_result->total_available_count,
                        .error_message = {}, .error_details = {}};
            }, "accounts");
        });

     watcher_->setFuture(future);
}

void ClientAccountModel::onAccountsLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "On accounts loaded event.";
    is_fetching_ = false;

    auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch accounts: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    // Build map of login_info by account_id for joining
    std::unordered_map<std::string, iam::domain::login_info> login_info_map;
    for (auto& li : result.loginInfos) {
        login_info_map[boost::uuids::to_string(li.account_id)] = std::move(li);
    }

    const int new_count = static_cast<int>(result.accounts.size());

    if (new_count > 0) {
        // Join accounts with login_info and sort by username
        std::vector<AccountWithLoginInfo> new_items;
        new_items.reserve(new_count);
        for (auto& acct : result.accounts) {
            AccountWithLoginInfo item;
            std::string id_str = boost::uuids::to_string(acct.id);
            auto li_it = login_info_map.find(id_str);
            if (li_it != login_info_map.end()) {
                item.loginInfo = std::move(li_it->second);
            }
            item.account = std::move(acct);
            new_items.push_back(std::move(item));
        }

        std::ranges::sort(new_items, [](auto const& a, auto const& b) {
            return a.account.username < b.account.username;
        });

        beginResetModel();
        accounts_ = std::move(new_items);
        endResetModel();

        // Update the set of recent accounts for recency coloring
        std::vector<iam::domain::account> accounts_for_tracker;
        accounts_for_tracker.reserve(accounts_.size());
        for (const auto& item : accounts_) {
            accounts_for_tracker.push_back(item.account);
        }
        const bool has_recent = recencyTracker_.update(accounts_for_tracker);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " accounts newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " accounts."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

const AccountWithLoginInfo* ClientAccountModel::getAccountWithLoginInfo(int row) const {
    if (row < 0 || row >= static_cast<int>(accounts_.size()))
        return nullptr;

    return &accounts_[row];
}

const iam::domain::account* ClientAccountModel::getAccount(int row) const {
    if (row < 0 || row >= static_cast<int>(accounts_.size()))
        return nullptr;

    return &accounts_[row].account;
}

std::vector<AccountWithLoginInfo> ClientAccountModel::getAccountsWithLoginInfo() const {
    return accounts_;
}

void ClientAccountModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

QVariant ClientAccountModel::
recency_foreground_color(const std::string& username) const {
    if (recencyTracker_.is_recent(username) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientAccountModel::onPulseStateChanged(bool /*isOn*/) {
    if (!accounts_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientAccountModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

LoginStatus ClientAccountModel::calculateLoginStatus(
    const std::optional<iam::domain::login_info>& loginInfo) {

    // No login info means never logged in
    if (!loginInfo.has_value()) {
        return LoginStatus::Never;
    }

    // Currently online
    if (loginInfo->online) {
        return LoginStatus::Online;
    }

    // Check if last_login is at epoch (never logged in)
    const auto epoch = std::chrono::system_clock::time_point{};
    if (loginInfo->last_login == epoch) {
        return LoginStatus::Never;
    }

    // Calculate time since last login
    const auto now = std::chrono::system_clock::now();
    const auto days_since_login = std::chrono::duration_cast<std::chrono::hours>(
        now - loginInfo->last_login).count() / 24;

    // Threshold: 30 days for "recent" vs "long ago"
    constexpr int recent_threshold_days = 30;

    if (days_since_login <= recent_threshold_days) {
        return LoginStatus::Recent;
    }

    return LoginStatus::LongAgo;
}

}

