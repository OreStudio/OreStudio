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
#include "ores.qt/ClientResultModel.hpp"

#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include "ores.compute.api/messaging/result_protocol.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {

std::string result_key_extractor(const compute::domain::result& e) {
    return e.modified_by;
}

QString format_state(int s) {
    switch (s) {
    case 1: return QObject::tr("Inactive");
    case 2: return QObject::tr("Unsent");
    case 4: return QObject::tr("Running");
    case 5: return QObject::tr("Done");
    default: return QString::number(s);
    }
}

QString format_outcome(int o) {
    switch (o) {
    case 0: return QObject::tr("Pending");
    case 1: return QObject::tr("Success");
    case 3: return QObject::tr("Failed");
    case 4: return QObject::tr("No Reply");
    default: return QString::number(o);
    }
}


} // namespace

ClientResultModel::ClientResultModel(
    ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(result_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientResultModel::onResultsLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientResultModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientResultModel::onPulsingComplete);
}

int ClientResultModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(results_.size());
}

int ClientResultModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientResultModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= results_.size())
        return {};

    const auto& result = results_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case WorkunitId:
            return QString::fromStdString(boost::uuids::to_string(result.workunit_id));
        case HostId: {
            const auto uid = boost::uuids::to_string(result.host_id);
            return uid == "00000000-0000-0000-0000-000000000000"
                ? QString{}
                : QString::fromStdString(uid);
        }
        case ServerState:
            return format_state(result.server_state);
        case Outcome:
            return format_outcome(result.outcome);
        case OutputUri:
            return QString::fromStdString(result.output_uri);
        case ReceivedAt:
            return relative_time_helper::format(result.received_at);
        case Version:
            return static_cast<qlonglong>(result.version);
        case ModifiedBy:
            return QString::fromStdString(result.modified_by);
        default:
            return {};
        }
    }

    if (role == Qt::ForegroundRole)
        return recency_foreground_color(result.modified_by);

    return {};
}

QVariant ClientResultModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case WorkunitId:
        return tr("Workunit ID");
    case HostId:
        return tr("Host ID");
    case ServerState:
        return tr("State");
    case Outcome:
        return tr("Outcome");
    case OutputUri:
        return tr("Output URI");
    case ReceivedAt:
        return tr("Received At");
    case Version:
        return tr("Version");
    case ModifiedBy:
        return tr("Modified By");
    default:
        return {};
    }
}

void ClientResultModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh compute result model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!results_.empty()) {
        beginResetModel();
        results_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_results(0, page_size_);
}

void ClientResultModel::load_page(std::uint32_t offset,
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

    if (!results_.empty()) {
        beginResetModel();
        results_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_results(offset, limit);
}

void ClientResultModel::fetch_results(
    std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientResultModel> self = this;

    QFuture<FetchResult> future =
        QtConcurrent::run([self, offset, limit]() -> FetchResult {
            return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug) << "Making compute results request with offset="
                                           << offset << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false, .results = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                compute::messaging::list_results_request request;
                request.offset = offset;
                request.limit = limit;

                auto result = self->clientManager_->
                    process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to fetch compute results: "
                                               << result.error();
                    return {.success = false, .results = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(
                                "Failed to fetch compute results: " + result.error()),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug) << "Fetched " << result->results.size()
                                           << " compute results, total available: "
                                           << result->total_available_count;
                return {.success = true,
                        .results = std::move(result->results),
                        .total_available_count = static_cast<std::uint32_t>(result->total_available_count),
                        .error_message = {}, .error_details = {}};
            }, "compute results");
        });

    watcher_->setFuture(future);
}

void ClientResultModel::onResultsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch compute results: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.results.size());

    if (new_count > 0) {
        beginResetModel();
        results_ = std::move(result.results);
        endResetModel();

        const bool has_recent = recencyTracker_.update(results_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " compute results newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " compute results."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientResultModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const compute::domain::result*
ClientResultModel::getResult(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= results_.size())
        return nullptr;
    return &results_[idx];
}

QVariant ClientResultModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientResultModel::onPulseStateChanged(bool /*isOn*/) {
    if (!results_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientResultModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
