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
#include "ores.qt/ClientAppVersionModel.hpp"

#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include "ores.compute/messaging/app_version_protocol.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    std::string app_version_key_extractor(const compute::domain::app_version& e) {
        return e.wrapper_version;
    }
}

ClientAppVersionModel::ClientAppVersionModel(
    ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(app_version_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientAppVersionModel::onVersionsLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientAppVersionModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientAppVersionModel::onPulsingComplete);
}

int ClientAppVersionModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(app_versions_.size());
}

int ClientAppVersionModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientAppVersionModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= app_versions_.size())
        return {};

    const auto& app_version = app_versions_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case AppId:
            return QString::fromStdString(boost::uuids::to_string(app_version.app_id));
        case WrapperVersion:
            return QString::fromStdString(app_version.wrapper_version);
        case EngineVersion:
            return QString::fromStdString(app_version.engine_version);
        case Platform: {
            QString platforms;
            for (const auto& p : app_version.platforms) {
                if (!platforms.isEmpty()) platforms += ", ";
                platforms += QString::fromStdString(p);
            }
            return platforms;
        }
        case MinRamMb:
            return static_cast<qlonglong>(app_version.min_ram_mb);
        case PackageUri:
            return QString::fromStdString(app_version.package_uri);
        case Version:
            return static_cast<qlonglong>(app_version.version);
        case ModifiedBy:
            return QString::fromStdString(app_version.modified_by);
        default:
            return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(app_version.wrapper_version);
    }

    return {};
}

QVariant ClientAppVersionModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case AppId:
        return tr("App ID");
    case WrapperVersion:
        return tr("Wrapper Version");
    case EngineVersion:
        return tr("Engine Version");
    case Platform:
        return tr("Platform");
    case MinRamMb:
        return tr("Min RAM (MB)");
    case PackageUri:
        return tr("Package URI");
    case Version:
        return tr("Version");
    case ModifiedBy:
        return tr("Modified By");
    default:
        return {};
    }
}

void ClientAppVersionModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh app version model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!app_versions_.empty()) {
        beginResetModel();
        app_versions_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_app_versions(0, page_size_);
}

void ClientAppVersionModel::load_page(std::uint32_t offset,
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

    if (!app_versions_.empty()) {
        beginResetModel();
        app_versions_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_app_versions(offset, limit);
}

void ClientAppVersionModel::fetch_app_versions(
    std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientAppVersionModel> self = this;

    QFuture<FetchResult> future =
        QtConcurrent::run([self, offset, limit]() -> FetchResult {
            return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug) << "Making app versions request with offset="
                                           << offset << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false, .app_versions = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                compute::messaging::list_app_versions_request request;
                request.offset = offset;
                request.limit = limit;

                auto result = self->clientManager_->
                    process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to fetch app versions: "
                                               << result.error();
                    return {.success = false, .app_versions = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(
                                "Failed to fetch app versions: " + result.error()),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug) << "Fetched " << result->app_versions.size()
                                           << " app versions, total available: "
                                           << result->total_available_count;
                return {.success = true,
                        .app_versions = std::move(result->app_versions),
                        .total_available_count = static_cast<std::uint32_t>(result->total_available_count),
                        .error_message = {}, .error_details = {}};
            }, "app versions");
        });

    watcher_->setFuture(future);
}

void ClientAppVersionModel::onVersionsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch app versions: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.app_versions.size());

    if (new_count > 0) {
        beginResetModel();
        app_versions_ = std::move(result.app_versions);
        endResetModel();

        const bool has_recent = recencyTracker_.update(app_versions_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " app versions newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " app versions."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientAppVersionModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const compute::domain::app_version*
ClientAppVersionModel::getVersion(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= app_versions_.size())
        return nullptr;
    return &app_versions_[idx];
}

QVariant ClientAppVersionModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientAppVersionModel::onPulseStateChanged(bool /*isOn*/) {
    if (!app_versions_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientAppVersionModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
