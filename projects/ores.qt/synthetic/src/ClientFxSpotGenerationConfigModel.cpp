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
#include "ores.qt/ClientFxSpotGenerationConfigModel.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

ClientFxSpotGenerationConfigModel::ClientFxSpotGenerationConfigModel(
    ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent)
    , clientManager_(clientManager)
    , watcher_(new QFutureWatcher<FetchResult>(this)) {

    connect(watcher_,
            &QFutureWatcher<FetchResult>::finished,
            this,
            &ClientFxSpotGenerationConfigModel::onConfigsLoaded);
}

int ClientFxSpotGenerationConfigModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(configs_.size());
}

int ClientFxSpotGenerationConfigModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientFxSpotGenerationConfigModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= configs_.size())
        return {};

    const auto& config = configs_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
            case SourceName:
                return QString::fromStdString(config.source_name);
            case OreKey:
                return QString::fromStdString(config.ore_key);
            case InitialPrice:
                return config.gmm_initial_price;
            case TicksPerHour:
                return config.ticks_per_hour;
            case Enabled:
                return config.enabled ? tr("Yes") : tr("No");
            case Version:
                return static_cast<qlonglong>(config.version);
            case RecordedAt:
                return relative_time_helper::format(config.recorded_at);
            default:
                return {};
        }
    }

    return {};
}

QVariant ClientFxSpotGenerationConfigModel::headerData(int section,
                                                       Qt::Orientation orientation,
                                                       int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
        case SourceName:
            return tr("Source Name");
        case OreKey:
            return tr("ORE Key");
        case InitialPrice:
            return tr("Initial Price");
        case TicksPerHour:
            return tr("Ticks Per Hour");
        case Enabled:
            return tr("Enabled");
        case Version:
            return tr("Version");
        case RecordedAt:
            return tr("Recorded At");
        default:
            return {};
    }
}

void ClientFxSpotGenerationConfigModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn)
            << "Cannot refresh FX spot generation config model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!configs_.empty()) {
        beginResetModel();
        configs_.clear();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_configs(0, page_size_);
}

void ClientFxSpotGenerationConfigModel::load_page(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!configs_.empty()) {
        beginResetModel();
        configs_.clear();
        endResetModel();
    }

    fetch_configs(offset, limit);
}

void ClientFxSpotGenerationConfigModel::fetch_configs(std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientFxSpotGenerationConfigModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self, offset, limit]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>(
            [&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug)
                    << "Making FX spot generation configs request with offset=" << offset
                    << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false,
                            .configs = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                synthetic::messaging::get_fx_spot_generation_configs_request request{
                    .offset = static_cast<int>(offset), .limit = static_cast<int>(limit)};

                auto result =
                    self->clientManager_->process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to send request: " << result.error();
                    return {.success = false,
                            .configs = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(result.error()),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug)
                    << "Fetched " << result->configs.size() << " FX spot generation configs";
                return {.success = true,
                        .configs = std::move(result->configs),
                        .total_available_count =
                            static_cast<std::uint32_t>(result->total_available_count),
                        .error_message = {},
                        .error_details = {}};
            },
            "FX spot generation configs");
    });

    watcher_->setFuture(future);
}

void ClientFxSpotGenerationConfigModel::onConfigsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch FX spot generation configs: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.configs.size());

    if (new_count > 0) {
        beginResetModel();
        configs_ = std::move(result.configs);
        endResetModel();
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " FX spot generation configs."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientFxSpotGenerationConfigModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const synthetic::domain::fx_spot_generation_config*
ClientFxSpotGenerationConfigModel::getConfig(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= configs_.size())
        return nullptr;
    return &configs_[idx];
}

}
