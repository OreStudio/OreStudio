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
#include <QDateTime>
#include <QFont>
#include <boost/uuid/uuid_io.hpp>
#include "ores.comms/messaging/frame.hpp"
#include "ores.comms/messaging/message_types.hpp"
#include "ores.accounts/messaging/account_protocol.hpp"
#include "ores.accounts/messaging/login_protocol.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using comms::messaging::frame;
using comms::messaging::message_type;
using namespace ores::utility::log;

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
        if (index.column() == Column::IsAdmin ||
            index.column() == Column::Status ||
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
    case Column::IsAdmin: return account.is_admin ? tr("Admin") : tr("-");
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
        case Column::IsAdmin: return tr("Admin");
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

    is_fetching_ = true;
    QPointer<ClientAccountModel> self = this;
    const std::uint32_t page_size = page_size_;

    QFuture<FutureWatcherResult> future =
        QtConcurrent::run([self, offset, page_size]() -> FutureWatcherResult {
            BOOST_LOG_SEV(lg(), debug) << "Making an accounts request with offset="
                                       << offset << ", limit=" << page_size;
            if (!self) return {false, {}, {}, 0};

            // Fetch accounts
            accounts::messaging::list_accounts_request accounts_request;
            accounts_request.offset = offset;
            accounts_request.limit = page_size;
            auto accounts_payload = accounts_request.serialize();

            frame accounts_frame(message_type::list_accounts_request,
                0, std::move(accounts_payload));

            auto accounts_response_result =
                self->clientManager_->sendRequest(std::move(accounts_frame));

            if (!accounts_response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send accounts request: "
                                           << accounts_response_result.error();
                return {false, {}, {}, 0};
            }

            auto accounts_payload_result = accounts_response_result->decompressed_payload();
            if (!accounts_payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress accounts response";
                return {false, {}, {}, 0};
            }

            auto accounts_response =
                accounts::messaging::list_accounts_response::deserialize(*accounts_payload_result);

            if (!accounts_response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize accounts response";
                return {false, {}, {}, 0};
            }

            BOOST_LOG_SEV(lg(), debug) << "Received " << accounts_response->accounts.size()
                                       << " accounts, total available: "
                                       << accounts_response->total_available_count;

            // Fetch login info
            accounts::messaging::list_login_info_request login_info_request;
            auto login_info_payload = login_info_request.serialize();

            frame login_info_frame(message_type::list_login_info_request,
                0, std::move(login_info_payload));

            auto login_info_response_result =
                self->clientManager_->sendRequest(std::move(login_info_frame));

            std::vector<accounts::domain::login_info> login_infos;
            if (login_info_response_result) {
                auto login_payload_result = login_info_response_result->decompressed_payload();
                if (login_payload_result) {
                    auto login_response =
                        accounts::messaging::list_login_info_response::deserialize(*login_payload_result);
                    if (login_response) {
                        login_infos = std::move(login_response->login_infos);
                        BOOST_LOG_SEV(lg(), debug) << "Received " << login_infos.size()
                                                   << " login info records";
                    } else {
                        BOOST_LOG_SEV(lg(), warn) << "Failed to deserialize login info response";
                    }
                } else {
                    BOOST_LOG_SEV(lg(), warn) << "Failed to decompress login info response";
                }
            } else {
                BOOST_LOG_SEV(lg(), warn) << "Failed to fetch login info: "
                                          << login_info_response_result.error();
            }

            return {true, std::move(accounts_response->accounts),
                    std::move(login_infos),
                    accounts_response->total_available_count};
        });

     watcher_->setFuture(future);
}

void ClientAccountModel::onAccountsLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "On accounts loaded event.";
    is_fetching_ = false;

    auto result = watcher_->result();
    if (result.success) {
        total_available_count_ = result.total_available_count;

        // Build map of login_info by account_id for joining
        std::unordered_map<std::string, accounts::domain::login_info> login_info_map;
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

const accounts::domain::account* ClientAccountModel::getAccount(int row) const {
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
        if (account.recorded_at.empty()) {
            continue;
        }

        // Parse recorded_at as datetime (try multiple formats)
        QDateTime recordedAt = QDateTime::fromString(
            QString::fromStdString(account.recorded_at), Qt::ISODate);

        if (!recordedAt.isValid()) {
            // Try alternative format (YYYY-MM-DD HH:MM:SS)
            recordedAt = QDateTime::fromString(
                QString::fromStdString(account.recorded_at), "yyyy-MM-dd HH:mm:ss");
        }

        if (!recordedAt.isValid()) {
            // Try date-only format (YYYY-MM-DD) - assume start of day
            recordedAt = QDateTime::fromString(
                QString::fromStdString(account.recorded_at), "yyyy-MM-dd");
        }

        if (recordedAt.isValid() && recordedAt > last_reload_time_) {
            recent_usernames_.insert(account.username);
            BOOST_LOG_SEV(lg(), trace) << "Account " << account.username
                                       << " is recent (recorded_at: "
                                       << account.recorded_at << ")";
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
    const std::optional<accounts::domain::login_info>& loginInfo) {

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

