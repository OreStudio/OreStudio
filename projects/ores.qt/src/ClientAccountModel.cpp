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
#include <unordered_set>
#include <unordered_map>
#include <QtConcurrent>
#include <QColor>
#include "ores.qt/ExceptionHelper.hpp"
#include <QDateTime>
#include <QFont>
#include <boost/uuid/uuid_io.hpp>
#include "ores.comms/net/client_session.hpp"
#include "ores.iam/messaging/account_protocol.hpp"
#include "ores.iam/messaging/login_protocol.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

ClientAccountModel::
ClientAccountModel(ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent), clientManager_(clientManager),
      watcher_(new QFutureWatcher<FutureWatcherResult>(this)),
      pulse_timer_(new QTimer(this)) {

    connect(watcher_,
        &QFutureWatcher<FutureWatcherResult>::finished,
        this, &ClientAccountModel::onAccountsLoaded);

    // Setup pulse timer for recency color updates (same interval as reload button)
    connect(pulse_timer_, &QTimer::timeout,
        this, &ClientAccountModel::onPulseTimerTimeout);
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
    case Column::RecordedBy: return QString::fromStdString(account.recorded_by);
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
        case Column::Email: return tr("Email");
        case Column::Status: return tr("Status");
        case Column::Locked: return tr("Locked");
        case Column::Version: return tr("Version");
        case Column::RecordedBy: return tr("Recorded By");
        case Column::RecordedAt: return tr("Recorded At");
        default: return {};
        }
    }

    return {};
}

void ClientAccountModel::refresh(bool replace) {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh (replace=" << replace << ").";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    // If not connected, we can't fetch.
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh account model: disconnected.";
        return;
    }

    // If replacing, start from offset 0; otherwise append to existing data
    const std::uint32_t offset = replace ? 0 : static_cast<std::uint32_t>(accounts_.size());

    if (replace) {
        // Clear existing data when replacing
        if (!accounts_.empty()) {
            beginResetModel();
            accounts_.clear();
            total_available_count_ = 0;
            recent_usernames_.clear();
            pulse_timer_->stop();
            pulse_count_ = 0;
            pulse_state_ = false;
            endResetModel();
        }
    }

    fetch_accounts(offset, page_size_);
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
        recent_usernames_.clear();
        pulse_timer_->stop();
        pulse_count_ = 0;
        pulse_state_ = false;
        endResetModel();
    }

    fetch_accounts(offset, limit);
}

void ClientAccountModel::fetch_accounts(std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientAccountModel> self = this;

    QFuture<FutureWatcherResult> future =
        QtConcurrent::run([self, offset, limit]() -> FutureWatcherResult {
            BOOST_LOG_SEV(lg(), debug) << "Making an accounts request with offset="
                                       << offset << ", limit=" << limit;
            if (!self) return {false, {}, {}, 0};

            // Fetch accounts using typed request
            iam::messaging::get_accounts_request accounts_request;
            accounts_request.offset = offset;
            accounts_request.limit = limit;

            auto accounts_result = self->clientManager_->
                process_authenticated_request(std::move(accounts_request));

            if (!accounts_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to fetch accounts: "
                                           << comms::net::to_string(accounts_result.error());
                return {false, {}, {}, 0};
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

            return {true, std::move(accounts_result->accounts),
                    std::move(login_infos),
                    accounts_result->total_available_count};
        });

     watcher_->setFuture(future);
}

void ClientAccountModel::onAccountsLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "On accounts loaded event.";
    is_fetching_ = false;

    FetchResult result;
    try {
        result = watcher_->result();
    } catch (const std::exception& e) {
        exception_helper::handle_fetch_exception(e, tr("accounts"), lg(),
            [this](const QString& msg, const QString& details) {
                emit loadError(msg, details);
            });
        return;
    }

    if (result.success) {
        total_available_count_ = result.total_available_count;

        // Build map of login_info by account_id for joining
        std::unordered_map<std::string, iam::domain::login_info> login_info_map;
        for (auto& li : result.loginInfos) {
            login_info_map[boost::uuids::to_string(li.account_id)] = std::move(li);
        }

        // Build set of existing IDs for duplicate detection
        std::unordered_set<std::string> existing_ids;
        for (const auto& item : accounts_) {
            existing_ids.insert(boost::uuids::to_string(item.account.id));
        }

        // Filter out duplicates and join with login_info
        std::vector<AccountWithLoginInfo> new_items;
        for (auto& acct : result.accounts) {
            std::string id_str = boost::uuids::to_string(acct.id);
            if (existing_ids.find(id_str) == existing_ids.end()) {
                AccountWithLoginInfo item;
                item.account = std::move(acct);

                // Join with login_info if available
                auto li_it = login_info_map.find(id_str);
                if (li_it != login_info_map.end()) {
                    item.loginInfo = std::move(li_it->second);
                }

                new_items.push_back(std::move(item));
                existing_ids.insert(id_str);
            } else {
                BOOST_LOG_SEV(lg(), trace) << "Skipping duplicate account: "
                                           << id_str;
            }
        }

        const int new_count = static_cast<int>(new_items.size());

        if (new_count > 0) {
            // Use model reset since we're inserting and then sorting
            // (sorting is a structural change that requires full reset)
            beginResetModel();
            accounts_.insert(accounts_.end(),
                std::make_move_iterator(new_items.begin()),
                std::make_move_iterator(new_items.end()));

            // Sort all accounts by username
            std::ranges::sort(accounts_, [](auto const& a, auto const& b) {
                return a.account.username < b.account.username;
            });
            endResetModel();
        }

        BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " new accounts "
                                  << "(received " << result.accounts.size()
                                  << ", filtered " << (result.accounts.size() - new_count)
                                  << " duplicates). Total in model: " << accounts_.size()
                                  << ", Total available: " << total_available_count_;

        // Update the set of recent accounts for recency coloring
        update_recent_accounts();

        // Start the pulse timer if there are recent accounts to highlight
        if (!recent_usernames_.empty() && !pulse_timer_->isActive()) {
            pulse_count_ = 0;
            pulse_state_ = true;  // Start with highlight on
            pulse_timer_->start(pulse_interval_ms_);
        }

        emit dataLoaded();
    } else {
        BOOST_LOG_SEV(lg(), error) << "Accounts request failed: no response.";
        emit loadError(tr("Failed to load accounts from server"));
    }
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

bool ClientAccountModel::canFetchMore(const QModelIndex& parent) const {
    if (parent.isValid())
        return false;

    const bool has_more = accounts_.size() < total_available_count_;

    BOOST_LOG_SEV(lg(), trace) << "canFetchMore: " << has_more
                               << " (loaded: " << accounts_.size()
                               << ", available: " << total_available_count_ << ")";
    return has_more && !is_fetching_;
}

void ClientAccountModel::fetchMore(const QModelIndex& parent) {
    if (parent.isValid() || is_fetching_)
        return;

    BOOST_LOG_SEV(lg(), debug) << "fetchMore called, loading next page.";
    refresh(false); // false = append, don't replace
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

void ClientAccountModel::update_recent_accounts() {
    recent_usernames_.clear();

    if (accounts_.empty()) {
        BOOST_LOG_SEV(lg(), debug) << "No accounts to check for recency.";
        return;
    }

    QDateTime now = QDateTime::currentDateTime();

    // If this is the first load, set last_reload_time_ and return (no recent accounts)
    if (!last_reload_time_.isValid()) {
        last_reload_time_ = now;
        BOOST_LOG_SEV(lg(), debug) << "First load, setting last_reload_time_";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Checking accounts newer than last reload: "
                               << last_reload_time_.toString(Qt::ISODate).toStdString();

    // Find accounts with recorded_at newer than last reload
    for (const auto& item : accounts_) {
        const auto& account = item.account;
        if (account.recorded_at == std::chrono::system_clock::time_point{}) {
            continue;
        }

        // Convert time_point to QDateTime
        const auto msecs = std::chrono::duration_cast<std::chrono::milliseconds>(
            account.recorded_at.time_since_epoch()).count();
        QDateTime recordedAt = QDateTime::fromMSecsSinceEpoch(msecs);

        if (recordedAt.isValid() && recordedAt > last_reload_time_) {
            recent_usernames_.insert(account.username);
            BOOST_LOG_SEV(lg(), trace) << "Account " << account.username
                                       << " is recent";
        }
    }

    // Update the reload timestamp for next comparison
    last_reload_time_ = now;

    BOOST_LOG_SEV(lg(), debug) << "Found " << recent_usernames_.size()
                               << " accounts newer than last reload";
}

QVariant ClientAccountModel::
recency_foreground_color(const std::string& username) const {
    if (recent_usernames_.empty() || recent_usernames_.find(username) == recent_usernames_.end()) {
        return {};  // No special color
    }

    // Only show color when pulse_state_ is true (pulsing on)
    if (!pulse_state_) {
        return {};
    }

    // Return yellow/gold color for recent accounts
    return QColor(255, 200, 0);  // Gold color
}

void ClientAccountModel::onPulseTimerTimeout() {
    pulse_state_ = !pulse_state_;
    pulse_count_++;

    // After max_pulse_cycles_, keep the highlight on permanently
    if (pulse_count_ >= max_pulse_cycles_) {
        pulse_timer_->stop();
        pulse_state_ = true;  // Keep highlight on after pulsing stops
    }

    // Emit dataChanged for ForegroundRole on all rows with recent usernames
    if (!recent_usernames_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
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

