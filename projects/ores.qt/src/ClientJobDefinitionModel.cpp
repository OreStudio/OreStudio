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
#include "ores.qt/ClientJobDefinitionModel.hpp"

#include <QtConcurrent>
#include "ores.scheduler/messaging/scheduler_protocol.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.comms/net/client_session.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    std::string job_definition_key_extractor(const scheduler::domain::job_definition& e) {
        return e.job_name;
    }
}

ClientJobDefinitionModel::ClientJobDefinitionModel(
    ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(job_definition_key_extractor, JobDefinitionTimestampExtractor{}),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientJobDefinitionModel::onDefinitionsLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientJobDefinitionModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientJobDefinitionModel::onPulsingComplete);
}

int ClientJobDefinitionModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(definitions_.size());
}

int ClientJobDefinitionModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientJobDefinitionModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= definitions_.size())
        return {};

    const auto& definition = definitions_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case JobName:
            return QString::fromStdString(definition.job_name);
        case Description:
            return QString::fromStdString(definition.description);
        case Schedule:
            return QString::fromStdString(definition.schedule_expression.to_string());
        case DatabaseName:
            return QString::fromStdString(definition.database_name);
        case Active:
            return definition.is_active ? tr("Active") : tr("Inactive");
        case Version:
            return definition.version;
        case ModifiedBy:
            return QString::fromStdString(definition.modified_by);
        default:
            return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(definition.job_name);
    }

    return {};
}

QVariant ClientJobDefinitionModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case JobName:
        return tr("Job Name");
    case Description:
        return tr("Description");
    case Schedule:
        return tr("Schedule");
    case DatabaseName:
        return tr("Database");
    case Active:
        return tr("Active");
    case Version:
        return tr("Version");
    case ModifiedBy:
        return tr("Modified By");
    default:
        return {};
    }
}

void ClientJobDefinitionModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh job definition model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!definitions_.empty()) {
        beginResetModel();
        definitions_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_definitions(0, page_size_);
}

void ClientJobDefinitionModel::load_page(std::uint32_t offset,
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

    if (!definitions_.empty()) {
        beginResetModel();
        definitions_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_definitions(offset, limit);
}

void ClientJobDefinitionModel::fetch_definitions(
    std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientJobDefinitionModel> self = this;

    QFuture<FetchResult> future =
        QtConcurrent::run([self, offset, limit]() -> FetchResult {
            return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug) << "Making job definitions request with offset="
                                           << offset << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false, .definitions = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                scheduler::messaging::get_job_definitions_request request;

                auto result = self->clientManager_->
                    process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to fetch job definitions: "
                                               << comms::net::to_string(result.error());
                    return {.success = false, .definitions = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(
                                "Failed to fetch job definitions: " + comms::net::to_string(result.error())),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug) << "Fetched " << result->definitions.size()
                                           << " job definitions";
                const std::uint32_t count =
                    static_cast<std::uint32_t>(result->definitions.size());
                return {.success = true,
                        .definitions = std::move(result->definitions),
                        .total_available_count = count,
                        .error_message = {}, .error_details = {}};
            }, "job definitions");
        });

    watcher_->setFuture(future);
}

void ClientJobDefinitionModel::onDefinitionsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch job definitions: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.definitions.size());

    if (new_count > 0) {
        beginResetModel();
        definitions_ = std::move(result.definitions);
        endResetModel();

        const bool has_recent = recencyTracker_.update(definitions_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " job definitions newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " job definitions."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientJobDefinitionModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const scheduler::domain::job_definition*
ClientJobDefinitionModel::getDefinition(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= definitions_.size())
        return nullptr;
    return &definitions_[idx];
}

QVariant ClientJobDefinitionModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientJobDefinitionModel::onPulseStateChanged(bool /*isOn*/) {
    if (!definitions_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientJobDefinitionModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
