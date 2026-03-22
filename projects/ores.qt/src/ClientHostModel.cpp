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
#include "ores.qt/ClientHostModel.hpp"

#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include "ores.compute/messaging/host_protocol.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    std::string host_key_extractor(const compute::domain::host& e) {
        return e.external_id;
    }
}

ClientHostModel::ClientHostModel(
    ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(host_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientHostModel::onHostsLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientHostModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientHostModel::onPulsingComplete);
}

int ClientHostModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(hosts_.size());
}

int ClientHostModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientHostModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= hosts_.size())
        return {};

    const auto& host = hosts_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case ExternalId:
            return QString::fromStdString(host.external_id);
        case Location:
            return QString::fromStdString(host.location);
        case CpuCount:
            return static_cast<qlonglong>(host.cpu_count);
        case RamMb:
            return static_cast<qlonglong>(host.ram_mb);
        case GpuType:
            return QString::fromStdString(host.gpu_type);
        case LastRpcTime:
            return relative_time_helper::format(host.last_rpc_time);
        case CreditTotal:
            return static_cast<qlonglong>(host.credit_total);
        case Version:
            return static_cast<qlonglong>(host.version);
        case ModifiedBy:
            return QString::fromStdString(host.modified_by);
        default:
            return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(host.external_id);
    }

    return {};
}

QVariant ClientHostModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case ExternalId:
        return tr("Host ID");
    case Location:
        return tr("Location");
    case CpuCount:
        return tr("CPUs");
    case RamMb:
        return tr("RAM (MB)");
    case GpuType:
        return tr("GPU");
    case LastRpcTime:
        return tr("Last Heartbeat");
    case CreditTotal:
        return tr("Credits");
    case Version:
        return tr("Version");
    case ModifiedBy:
        return tr("Modified By");
    default:
        return {};
    }
}

void ClientHostModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh compute host model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!hosts_.empty()) {
        beginResetModel();
        hosts_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_hosts(0, page_size_);
}

void ClientHostModel::load_page(std::uint32_t offset,
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

    if (!hosts_.empty()) {
        beginResetModel();
        hosts_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_hosts(offset, limit);
}

void ClientHostModel::fetch_hosts(
    std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientHostModel> self = this;

    QFuture<FetchResult> future =
        QtConcurrent::run([self, offset, limit]() -> FetchResult {
            return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug) << "Making compute hosts request with offset="
                                           << offset << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false, .hosts = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                compute::messaging::list_hosts_request request;
                request.offset = offset;
                request.limit = limit;

                auto result = self->clientManager_->
                    process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to fetch compute hosts: "
                                               << result.error();
                    return {.success = false, .hosts = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(
                                "Failed to fetch compute hosts: " + result.error()),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug) << "Fetched " << result->hosts.size()
                                           << " compute hosts, total available: "
                                           << result->total_available_count;
                return {.success = true,
                        .hosts = std::move(result->hosts),
                        .total_available_count = static_cast<std::uint32_t>(result->total_available_count),
                        .error_message = {}, .error_details = {}};
            }, "compute hosts");
        });

    watcher_->setFuture(future);
}

void ClientHostModel::onHostsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch compute hosts: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.hosts.size());

    if (new_count > 0) {
        beginResetModel();
        hosts_ = std::move(result.hosts);
        endResetModel();

        const bool has_recent = recencyTracker_.update(hosts_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " compute hosts newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " compute hosts."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientHostModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const compute::domain::host*
ClientHostModel::getHost(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= hosts_.size())
        return nullptr;
    return &hosts_[idx];
}

QVariant ClientHostModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientHostModel::onPulseStateChanged(bool /*isOn*/) {
    if (!hosts_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientHostModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
