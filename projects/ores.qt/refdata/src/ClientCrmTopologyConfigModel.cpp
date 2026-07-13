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
#include "ores.qt/ClientCrmTopologyConfigModel.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/crm_topology_config_protocol.hpp"
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

namespace {
std::string crm_topology_config_key_extractor(const refdata::domain::crm_topology_config& e) {
    return e.name;
}
}

ClientCrmTopologyConfigModel::ClientCrmTopologyConfigModel(ClientManager* clientManager,
                                                           QObject* parent)
    : AbstractClientModel(parent)
    , clientManager_(clientManager)
    , watcher_(new QFutureWatcher<FetchResult>(this))
    , recencyTracker_(crm_topology_config_key_extractor)
    , pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_,
            &QFutureWatcher<FetchResult>::finished,
            this,
            &ClientCrmTopologyConfigModel::onConfigsLoaded);

    connect(pulseManager_,
            &RecencyPulseManager::pulse_state_changed,
            this,
            &ClientCrmTopologyConfigModel::onPulseStateChanged);
    connect(pulseManager_,
            &RecencyPulseManager::pulsing_complete,
            this,
            &ClientCrmTopologyConfigModel::onPulsingComplete);
}

int ClientCrmTopologyConfigModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(crm_topology_configs_.size());
}

int ClientCrmTopologyConfigModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientCrmTopologyConfigModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= crm_topology_configs_.size())
        return {};

    const auto& config = crm_topology_configs_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
            case Name:
                return QString::fromStdString(config.name);
            case PivotCurrencyCode:
                return QString::fromStdString(config.pivot_currency_code);
            case Enabled:
                return config.enabled ? tr("true") : tr("false");
            case Version:
                return static_cast<qlonglong>(config.version);
            case ModifiedBy:
                return QString::fromStdString(config.modified_by);
            case RecordedAt:
                return relative_time_helper::format(config.recorded_at);
            default:
                return {};
        }
    }

    if (role == Qt::DecorationRole && imageCache_) {
        if (index.column() == Column::PivotCurrencyCode)
            return currency_flag_icon(*imageCache_, config.pivot_currency_code);
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(config.name);
    }

    return {};
}

QVariant
ClientCrmTopologyConfigModel::headerData(int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
        case Name:
            return tr("Name");
        case PivotCurrencyCode:
            return tr("Pivot Currency");
        case Enabled:
            return tr("Enabled");
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

void ClientCrmTopologyConfigModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh CRM topology config model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!crm_topology_configs_.empty()) {
        beginResetModel();
        crm_topology_configs_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_crm_topology_configs(0, page_size_);
}

void ClientCrmTopologyConfigModel::load_page(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!crm_topology_configs_.empty()) {
        beginResetModel();
        crm_topology_configs_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_crm_topology_configs(offset, limit);
}

void ClientCrmTopologyConfigModel::fetch_crm_topology_configs(std::uint32_t offset,
                                                              std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientCrmTopologyConfigModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self, offset, limit]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>(
            [&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug)
                    << "Making CRM topology configs request with offset=" << offset
                    << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false,
                            .crm_topology_configs = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                refdata::messaging::get_crm_topology_configs_request request;
                request.offset = offset;
                request.limit = limit;

                auto result =
                    self->clientManager_->process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to send request: " << result.error();
                    return {.success = false,
                            .crm_topology_configs = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(result.error()),
                            .error_details = {}};
                }

                // A transport-level success (result is set) does not mean the
                // request itself succeeded -- the server encodes business/
                // repository failures (e.g. a query error) as a normally-
                // deserializable response with success=false and a message,
                // not a transport error. Missing this check silently turns a
                // real backend failure into "0 rows loaded", indistinguishable
                // from a genuinely empty result set.
                if (!result->success) {
                    BOOST_LOG_SEV(lg(), error) << "Server reported failure: " << result->message;
                    return {.success = false,
                            .crm_topology_configs = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(result->message),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug)
                    << "Fetched " << result->crm_topology_configs.size()
                    << " CRM topology configs, total available: " << result->total_available_count;
                return {.success = true,
                        .crm_topology_configs = std::move(result->crm_topology_configs),
                        .total_available_count =
                            static_cast<std::uint32_t>(result->total_available_count),
                        .error_message = {},
                        .error_details = {}};
            },
            "CRM topology configs");
    });

    watcher_->setFuture(future);
}

void ClientCrmTopologyConfigModel::onConfigsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to fetch CRM topology configs: " << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.crm_topology_configs.size());

    if (new_count > 0) {
        beginResetModel();
        crm_topology_configs_ = std::move(result.crm_topology_configs);
        endResetModel();

        const bool has_recent = recencyTracker_.update(crm_topology_configs_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " CRM topology configs newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " CRM topology configs."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientCrmTopologyConfigModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const refdata::domain::crm_topology_config* ClientCrmTopologyConfigModel::getConfig(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= crm_topology_configs_.size())
        return nullptr;
    return &crm_topology_configs_[idx];
}


QVariant ClientCrmTopologyConfigModel::recency_foreground_color(const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientCrmTopologyConfigModel::onPulseStateChanged(bool /*isOn*/) {
    if (!crm_topology_configs_.empty()) {
        emit dataChanged(
            index(0, 0), index(rowCount() - 1, columnCount() - 1), {Qt::ForegroundRole});
    }
}

void ClientCrmTopologyConfigModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
