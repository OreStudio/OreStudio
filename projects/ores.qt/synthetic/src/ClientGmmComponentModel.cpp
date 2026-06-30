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
#include "ores.qt/ClientGmmComponentModel.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

namespace {
std::string gmm_component_key_extractor(const synthetic::domain::gmm_component& e) {
    return boost::uuids::to_string(e.id);
}
}

ClientGmmComponentModel::ClientGmmComponentModel(ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent)
    , clientManager_(clientManager)
    , watcher_(new QFutureWatcher<FetchResult>(this))
    , recencyTracker_(gmm_component_key_extractor)
    , pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_,
            &QFutureWatcher<FetchResult>::finished,
            this,
            &ClientGmmComponentModel::onComponentsLoaded);

    connect(pulseManager_,
            &RecencyPulseManager::pulse_state_changed,
            this,
            &ClientGmmComponentModel::onPulseStateChanged);
    connect(pulseManager_,
            &RecencyPulseManager::pulsing_complete,
            this,
            &ClientGmmComponentModel::onPulsingComplete);
}

int ClientGmmComponentModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(gmm_components_.size());
}

int ClientGmmComponentModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientGmmComponentModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= gmm_components_.size())
        return {};

    const auto& gmm_component = gmm_components_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
            case ComponentIndex:
                return static_cast<qlonglong>(gmm_component.component_index);
            case Description:
                return QString::fromStdString(gmm_component.description);
            case Mean:
            case Stdev:
            case Weight:
            case Version:
                return static_cast<qlonglong>(gmm_component.version);
            case ModifiedBy:
                return QString::fromStdString(gmm_component.modified_by);
            case RecordedAt:
                return relative_time_helper::format(gmm_component.recorded_at);
            default:
                return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(boost::uuids::to_string(gmm_component.id));
    }

    return {};
}

QVariant
ClientGmmComponentModel::headerData(int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
        case ComponentIndex:
            return tr("Component Index");
        case Description:
            return tr("Description");
        case Mean:
            return tr("Mean");
        case Stdev:
            return tr("Std Dev");
        case Weight:
            return tr("Weight");
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

void ClientGmmComponentModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh GMM component model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!gmm_components_.empty()) {
        beginResetModel();
        gmm_components_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_gmm_components(0, page_size_);
}

void ClientGmmComponentModel::load_page(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!gmm_components_.empty()) {
        beginResetModel();
        gmm_components_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_gmm_components(offset, limit);
}

void ClientGmmComponentModel::fetch_gmm_components(std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientGmmComponentModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self, offset, limit]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>(
            [&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug) << "Making GMM components request with offset=" << offset
                                           << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false,
                            .gmm_components = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                synthetic::messaging::get_gmm_components_request request;
                request.offset = offset;
                request.limit = limit;

                auto result =
                    self->clientManager_->process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to send request: " << result.error();
                    return {.success = false,
                            .gmm_components = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(result.error()),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug)
                    << "Fetched " << result->gmm_components.size()
                    << " GMM components, total available: " << result->total_available_count;
                return {.success = true,
                        .gmm_components = std::move(result->gmm_components),
                        .total_available_count =
                            static_cast<std::uint32_t>(result->total_available_count),
                        .error_message = {},
                        .error_details = {}};
            },
            "GMM components");
    });

    watcher_->setFuture(future);
}

void ClientGmmComponentModel::onComponentsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to fetch GMM components: " << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.gmm_components.size());

    if (new_count > 0) {
        beginResetModel();
        gmm_components_ = std::move(result.gmm_components);
        endResetModel();

        const bool has_recent = recencyTracker_.update(gmm_components_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " GMM components newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " GMM components."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientGmmComponentModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const synthetic::domain::gmm_component* ClientGmmComponentModel::getComponent(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= gmm_components_.size())
        return nullptr;
    return &gmm_components_[idx];
}

QVariant ClientGmmComponentModel::recency_foreground_color(const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientGmmComponentModel::onPulseStateChanged(bool /*isOn*/) {
    if (!gmm_components_.empty()) {
        emit dataChanged(
            index(0, 0), index(rowCount() - 1, columnCount() - 1), {Qt::ForegroundRole});
    }
}

void ClientGmmComponentModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
