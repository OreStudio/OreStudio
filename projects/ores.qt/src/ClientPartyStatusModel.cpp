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
#include "ores.qt/ClientPartyStatusModel.hpp"

#include <QtConcurrent>
#include "ores.refdata/messaging/party_status_protocol.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.comms/net/client_session.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    std::string party_status_key_extractor(const refdata::domain::party_status& e) {
        return e.code;
    }
}

ClientPartyStatusModel::ClientPartyStatusModel(
    ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(party_status_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientPartyStatusModel::onStatussLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientPartyStatusModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientPartyStatusModel::onPulsingComplete);
}

int ClientPartyStatusModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(statuses_.size());
}

int ClientPartyStatusModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientPartyStatusModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= statuses_.size())
        return {};

    const auto& status = statuses_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Code:
            return QString::fromStdString(status.code);
        case Name:
            return QString::fromStdString(status.name);
        case Description:
            return QString::fromStdString(status.description);
        case DisplayOrder:
            return status.display_order;
        case Version:
            return status.version;
        case ModifiedBy:
            return QString::fromStdString(status.modified_by);
        case RecordedAt:
            return relative_time_helper::format(status.recorded_at);
        default:
            return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(status.code);
    }

    return {};
}

QVariant ClientPartyStatusModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case Code:
        return tr("Code");
    case Name:
        return tr("Name");
    case Description:
        return tr("Description");
    case DisplayOrder:
        return tr("Order");
    case Version:
        return tr("Version");
    case ModifiedBy:
        return tr("Modified By");
    case RecordedAt:
        return tr("Recorded At");
    default:
        return {};
    }
}

void ClientPartyStatusModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh party status model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!statuses_.empty()) {
        beginResetModel();
        statuses_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_statuses(0, page_size_);
}

void ClientPartyStatusModel::load_page(std::uint32_t offset,
                                          std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!statuses_.empty()) {
        beginResetModel();
        statuses_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_statuses(offset, limit);
}

void ClientPartyStatusModel::fetch_statuses(
    std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientPartyStatusModel> self = this;

    QFuture<FetchResult> future =
        QtConcurrent::run([self, offset, limit]() -> FetchResult {
            return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug) << "Making party statuses request with offset="
                                           << offset << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false, .statuses = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                refdata::messaging::get_party_statuses_request request;

                auto result = self->clientManager_->
                    process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to fetch party statuses: "
                                               << comms::net::to_string(result.error());
                    return {.success = false, .statuses = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(
                                "Failed to fetch party statuses: " + comms::net::to_string(result.error())),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug) << "Fetched " << result->statuses.size()
                                           << " party statuses";
                const std::uint32_t count =
                    static_cast<std::uint32_t>(result->statuses.size());
                return {.success = true,
                        .statuses = std::move(result->statuses),
                        .total_available_count = count,
                        .error_message = {}, .error_details = {}};
            }, "party statuses");
        });

    watcher_->setFuture(future);
}

void ClientPartyStatusModel::onStatussLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch party statuses: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.statuses.size());

    if (new_count > 0) {
        beginResetModel();
        statuses_ = std::move(result.statuses);
        endResetModel();

        const bool has_recent = recencyTracker_.update(statuses_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " party statuses newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " party statuses."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientPartyStatusModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const refdata::domain::party_status*
ClientPartyStatusModel::getStatus(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= statuses_.size())
        return nullptr;
    return &statuses_[idx];
}

QVariant ClientPartyStatusModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientPartyStatusModel::onPulseStateChanged(bool /*isOn*/) {
    if (!statuses_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientPartyStatusModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
