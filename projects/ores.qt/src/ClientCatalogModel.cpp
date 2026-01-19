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
#include "ores.qt/ClientCatalogModel.hpp"

#include <QFutureWatcher>
#include <QtConcurrent>
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.dq/messaging/data_organization_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

ClientCatalogModel::ClientCatalogModel(ClientManager* clientManager,
                                       QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientCatalogModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientCatalogModel::onPulsingComplete);
}

int ClientCatalogModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(catalogs_.size());
}

int ClientCatalogModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientCatalogModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid() || index.row() >= static_cast<int>(catalogs_.size()))
        return {};

    const auto& catalog = catalogs_[index.row()];

    if (role == Qt::ForegroundRole) {
        return foregroundColor(catalog.name);
    }

    if (role != Qt::DisplayRole)
        return {};

    switch (index.column()) {
    case Name:
        return QString::fromStdString(catalog.name);
    case Description:
        return QString::fromStdString(catalog.description);
    case Owner:
        return catalog.owner
            ? QString::fromStdString(*catalog.owner)
            : QString();
    case Version:
        return catalog.version;
    case RecordedBy:
        return QString::fromStdString(catalog.recorded_by);
    case RecordedAt:
        return relative_time_helper::format(catalog.recorded_at);
    default:
        return {};
    }
}

QVariant ClientCatalogModel::headerData(int section,
                                        Qt::Orientation orientation,
                                        int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case Name: return tr("Name");
    case Description: return tr("Description");
    case Owner: return tr("Owner");
    case Version: return tr("Version");
    case RecordedBy: return tr("Recorded By");
    case RecordedAt: return tr("Recorded At");
    default: return {};
    }
}

void ClientCatalogModel::loadData() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorOccurred("Not connected to server");
        return;
    }

    emit loadStarted();

    QPointer<ClientCatalogModel> self = this;

    struct LoadResult {
        bool success;
        std::vector<dq::domain::catalog> catalogs;
        QString error_message;
        QString error_details;
    };

    auto task = [self]() -> LoadResult {
        return exception_helper::wrap_async_fetch<LoadResult>([&]() -> LoadResult {
            if (!self || !self->clientManager_) {
                return {.success = false, .catalogs = {},
                        .error_message = "Model was destroyed",
                        .error_details = {}};
            }

            dq::messaging::get_catalogs_request request;
            auto payload = request.serialize();

            comms::messaging::frame request_frame(
                comms::messaging::message_type::get_catalogs_request,
                0, std::move(payload));

            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));
            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send request";
                return {.success = false, .catalogs = {},
                        .error_message = "Failed to communicate with server",
                        .error_details = {}};
            }

            // Check for server error response
            if (auto err = exception_helper::check_error_response(*response_result)) {
                BOOST_LOG_SEV(lg(), error) << "Server error: "
                                           << err->message.toStdString();
                return {.success = false, .catalogs = {},
                        .error_message = err->message,
                        .error_details = err->details};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response";
                return {.success = false, .catalogs = {},
                        .error_message = "Failed to decompress response",
                        .error_details = {}};
            }

            auto response =
                dq::messaging::get_catalogs_response::deserialize(*payload_result);
            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
                return {.success = false, .catalogs = {},
                        .error_message = "Invalid server response",
                        .error_details = {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Fetched " << response->catalogs.size()
                                       << " catalogs";
            return {.success = true, .catalogs = std::move(response->catalogs),
                    .error_message = {}, .error_details = {}};
        }, "catalogs");
    };

    auto* watcher = new QFutureWatcher<LoadResult>(this);
    connect(watcher, &QFutureWatcher<LoadResult>::finished, this,
            [self, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error) << "Failed to fetch catalogs: "
                                       << result.error_message.toStdString();
            emit self->errorOccurred(result.error_message, result.error_details);
            return;
        }

        self->beginResetModel();
        self->recentNames_.clear();
        self->pulseManager_->stop_pulsing();
        self->catalogs_ = std::move(result.catalogs);
        self->endResetModel();

        self->updateRecentCatalogs();

        if (!self->recentNames_.empty() && !self->pulseManager_->is_pulsing()) {
            self->pulseManager_->start_pulsing();
        }

        emit self->loadFinished();

        BOOST_LOG_SEV(lg(), debug)
            << "Loaded " << self->catalogs_.size() << " catalogs";
    });

    watcher->setFuture(QtConcurrent::run(task));
}

const dq::domain::catalog& ClientCatalogModel::catalogAt(int row) const {
    return catalogs_.at(row);
}

void ClientCatalogModel::updateRecentCatalogs() {
    recentNames_.clear();

    const QDateTime now = QDateTime::currentDateTime();

    // First load: set baseline timestamp, no highlighting
    if (!lastReloadTime_.isValid()) {
        lastReloadTime_ = now;
        BOOST_LOG_SEV(lg(), debug) << "First load - setting baseline timestamp";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Checking catalogs newer than last reload: "
                               << lastReloadTime_.toString(Qt::ISODate).toStdString();

    // Find catalogs with recorded_at newer than last reload
    for (const auto& catalog : catalogs_) {
        if (catalog.recorded_at == std::chrono::system_clock::time_point{}) {
            continue;
        }

        const auto msecs = std::chrono::duration_cast<std::chrono::milliseconds>(
            catalog.recorded_at.time_since_epoch()).count();
        QDateTime recordedAt = QDateTime::fromMSecsSinceEpoch(msecs);

        if (recordedAt.isValid() && recordedAt > lastReloadTime_) {
            recentNames_.insert(catalog.name);
            BOOST_LOG_SEV(lg(), trace) << "Catalog " << catalog.name << " is recent";
        }
    }

    lastReloadTime_ = now;

    BOOST_LOG_SEV(lg(), debug) << "Found " << recentNames_.size()
                               << " catalogs newer than last reload";
}

QVariant ClientCatalogModel::foregroundColor(const std::string& name) const {
    if (recentNames_.find(name) != recentNames_.end() && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientCatalogModel::onPulseStateChanged(bool /*isOn*/) {
    if (!catalogs_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientCatalogModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recentNames_.clear();
}

}
