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
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include "ores.comms/messaging/frame.hpp"
#include "ores.comms/messaging/message_types.hpp"
#include "ores.accounts/messaging/account_protocol.hpp"

namespace ores::qt {

using comms::messaging::frame;
using comms::messaging::message_type;
using namespace ores::utility::log;

ClientAccountModel::
ClientAccountModel(ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent), clientManager_(clientManager),
      watcher_(new QFutureWatcher<FutureWatcherResult>(this)) {

    connect(watcher_,
        &QFutureWatcher<FutureWatcherResult>::finished,
        this, &ClientAccountModel::onAccountsLoaded);
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
    if (!index.isValid() || role != Qt::DisplayRole)
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= accounts_.size())
        return {};

    const auto& account = accounts_[row];

    switch (index.column()) {
    case Column::Username: return QString::fromStdString(account.username);
    case Column::Email: return QString::fromStdString(account.email);
    case Column::IsAdmin: return account.is_admin ? tr("Yes") : tr("No");
    case Column::Version: return account.version;
    case Column::RecordedBy: return QString::fromStdString(account.recorded_by);
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
        case Column::Version: return tr("Version");
        case Column::RecordedBy: return tr("Recorded By");
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
            if (!self) return {false, {}, 0};

            accounts::messaging::list_accounts_request request;
            request.offset = offset;
            request.limit = page_size;
            auto payload = request.serialize();

            frame request_frame(message_type::list_accounts_request,
                0, std::move(payload));

            // Send request synchronously (on background thread)
            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send request: "
                                           << response_result.error();
                return {false, {}, 0};
            }

            // Log frame attributes for debugging
            const auto& header = response_result->header();
            BOOST_LOG_SEV(lg(), debug) << "Accounts response frame: type="
                                       << static_cast<int>(header.type)
                                       << ", compression=" << header.compression
                                       << ", payload_size=" << response_result->payload().size();

            // Decompress payload
            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress accounts response"
                                           << ", compression=" << header.compression
                                           << ", error=" << payload_result.error();
                return {false, {}, 0};
            }

            BOOST_LOG_SEV(lg(), debug) << "Received an accounts response.";
            auto response =
                accounts::messaging::list_accounts_response::deserialize(*payload_result);

            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize accounts response"
                                           << ", decompressed_payload_size="
                                           << payload_result->size();
                return {false, {}, 0};
            }

            BOOST_LOG_SEV(lg(), debug) << "Received " << response->accounts.size()
                                       << " accounts, total available: "
                                       << response->total_available_count;

            return {true, std::move(response->accounts),
                    response->total_available_count};
        });

     watcher_->setFuture(future);
}

void ClientAccountModel::onAccountsLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "On accounts loaded event.";
    is_fetching_ = false;

    auto result = watcher_->result();
    if (result.success) {
        total_available_count_ = result.total_available_count;

        // Build set of existing IDs for duplicate detection
        std::unordered_set<std::string> existing_ids;
        for (const auto& acct : accounts_) {
            existing_ids.insert(boost::uuids::to_string(acct.id));
        }

        // Filter out duplicates from new results
        std::vector<accounts::domain::account> new_accounts;
        for (auto& acct : result.accounts) {
            std::string id_str = boost::uuids::to_string(acct.id);
            if (existing_ids.find(id_str) == existing_ids.end()) {
                new_accounts.push_back(std::move(acct));
                existing_ids.insert(id_str);
            } else {
                BOOST_LOG_SEV(lg(), trace) << "Skipping duplicate account: "
                                           << id_str;
            }
        }

        const int old_size = static_cast<int>(accounts_.size());
        const int new_count = static_cast<int>(new_accounts.size());

        if (new_count > 0) {
            // Append new data to existing accounts
            beginInsertRows(QModelIndex(), old_size, old_size + new_count - 1);
            accounts_.insert(accounts_.end(),
                std::make_move_iterator(new_accounts.begin()),
                std::make_move_iterator(new_accounts.end()));
            endInsertRows();

            // Sort all accounts by username
            std::ranges::sort(accounts_, [](auto const& a, auto const& b) {
                return a.username < b.username;
            });

            // Notify that all data may have changed due to sorting
            if (old_size > 0) {
                emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1));
            }
        }

        BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " new accounts "
                                  << "(received " << result.accounts.size()
                                  << ", filtered " << (result.accounts.size() - new_count)
                                  << " duplicates). Total in model: " << accounts_.size()
                                  << ", Total available: " << total_available_count_;

        emit dataLoaded();
    } else {
        BOOST_LOG_SEV(lg(), error) << "Accounts request failed: no response.";
        emit loadError(tr("Failed to load accounts from server"));
    }
}

const accounts::domain::account* ClientAccountModel::getAccount(int row) const {
    if (row < 0 || row >= static_cast<int>(accounts_.size()))
        return nullptr;

    return &accounts_[row];
}

std::vector<accounts::domain::account> ClientAccountModel::getAccounts() const {
    return accounts_;
}

bool ClientAccountModel::canFetchMore(const QModelIndex& parent) const {
    if (parent.isValid())
        return false;

    const bool has_more = accounts_.size() < page_size_ &&
                          accounts_.size() < total_available_count_;

    BOOST_LOG_SEV(lg(), trace) << "canFetchMore: " << has_more
                               << " (loaded: " << accounts_.size()
                               << ", page_size: " << page_size_
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

}
