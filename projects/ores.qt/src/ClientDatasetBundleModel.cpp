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
#include "ores.qt/ClientDatasetBundleModel.hpp"

#include <QtConcurrent>
#include "ores.dq/messaging/dataset_bundle_protocol.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.comms/net/client_session.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    std::string dataset_bundle_key_extractor(const dq::domain::dataset_bundle& e) {
        return e.code;
    }
}

ClientDatasetBundleModel::ClientDatasetBundleModel(
    ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(dataset_bundle_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientDatasetBundleModel::onBundlesLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientDatasetBundleModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientDatasetBundleModel::onPulsingComplete);
}

int ClientDatasetBundleModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(bundles_.size());
}

int ClientDatasetBundleModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientDatasetBundleModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= bundles_.size())
        return {};

    const auto& bundle = bundles_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Code:
            return QString::fromStdString(bundle.code);
        case Name:
            return QString::fromStdString(bundle.name);
        case Description:
            return QString::fromStdString(bundle.description);
        case Version:
            return bundle.version;
        case ModifiedBy:
            return QString::fromStdString(bundle.modified_by);
        case RecordedAt:
            return relative_time_helper::format(bundle.recorded_at);
        default:
            return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(bundle.code);
    }

    return {};
}

QVariant ClientDatasetBundleModel::headerData(
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

void ClientDatasetBundleModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh dataset bundle model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!bundles_.empty()) {
        beginResetModel();
        bundles_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_bundles(0, page_size_);
}

void ClientDatasetBundleModel::load_page(std::uint32_t offset,
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

    if (!bundles_.empty()) {
        beginResetModel();
        bundles_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_bundles(offset, limit);
}

void ClientDatasetBundleModel::fetch_bundles(
    std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientDatasetBundleModel> self = this;

    QFuture<FetchResult> future =
        QtConcurrent::run([self, offset, limit]() -> FetchResult {
            return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug) << "Making dataset bundles request with offset="
                                           << offset << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false, .bundles = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                dq::messaging::get_dataset_bundles_request request;

                auto result = self->clientManager_->
                    process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to fetch dataset bundles: "
                                               << comms::net::to_string(result.error());
                    return {.success = false, .bundles = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(
                                "Failed to fetch dataset bundles: " + comms::net::to_string(result.error())),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug) << "Fetched " << result->bundles.size()
                                           << " dataset bundles";
                const std::uint32_t count =
                    static_cast<std::uint32_t>(result->bundles.size());
                return {.success = true,
                        .bundles = std::move(result->bundles),
                        .total_available_count = count,
                        .error_message = {}, .error_details = {}};
            }, "dataset bundles");
        });

    watcher_->setFuture(future);
}

void ClientDatasetBundleModel::onBundlesLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch dataset bundles: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.bundles.size());

    if (new_count > 0) {
        beginResetModel();
        bundles_ = std::move(result.bundles);
        endResetModel();

        const bool has_recent = recencyTracker_.update(bundles_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " dataset bundles newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " dataset bundles."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientDatasetBundleModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const dq::domain::dataset_bundle*
ClientDatasetBundleModel::getBundle(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= bundles_.size())
        return nullptr;
    return &bundles_[idx];
}

QVariant ClientDatasetBundleModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientDatasetBundleModel::onPulseStateChanged(bool /*isOn*/) {
    if (!bundles_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientDatasetBundleModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
