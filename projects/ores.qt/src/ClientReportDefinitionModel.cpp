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
#include "ores.qt/ClientReportDefinitionModel.hpp"

#include <QtConcurrent>
#include <QColor>
#include <boost/uuid/uuid_io.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.dq.api/messaging/fsm_protocol.hpp"
#include "ores.reporting.api/messaging/report_definition_protocol.hpp"
#include "ores.scheduler.api/domain/cron_expression.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    std::string report_definition_key_extractor(const reporting::domain::report_definition& e) {
        return e.name;
    }
}

ClientReportDefinitionModel::ClientReportDefinitionModel(
    ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      fsm_watcher_(new QFutureWatcher<FsmStateFetchResult>(this)),
      recencyTracker_(report_definition_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientReportDefinitionModel::onDefinitionsLoaded);
    connect(fsm_watcher_, &QFutureWatcher<FsmStateFetchResult>::finished,
            this, &ClientReportDefinitionModel::onFsmStatesLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientReportDefinitionModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientReportDefinitionModel::onPulsingComplete);
}

int ClientReportDefinitionModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(definitions_.size());
}

int ClientReportDefinitionModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientReportDefinitionModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= definitions_.size())
        return {};

    const auto& definition = definitions_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Name:
            return QString::fromStdString(definition.name);
        case ReportType:
            return QString::fromStdString(definition.report_type);
        case ScheduleExpression:
            return QString::fromStdString(definition.schedule_expression);
        case ConcurrencyPolicy:
            return QString::fromStdString(definition.concurrency_policy);
        case Status:
            return resolve_fsm_state_name(definition.fsm_state_id);
        case NextFire: {
            if (!definition.scheduler_job_id.has_value() || definition.schedule_expression.empty())
                return tr("—");
            auto expr = scheduler::domain::cron_expression::from_string(definition.schedule_expression);
            if (!expr)
                return tr("—");
            const auto tp = expr->next_occurrence();
            return QString::fromStdString(
                platform::time::datetime::format_time_point_utc(tp));
        }
        case Version:
            return static_cast<qlonglong>(definition.version);
        case ModifiedBy:
            return QString::fromStdString(definition.modified_by);
        case RecordedAt:
            return relative_time_helper::format(definition.recorded_at);
        default:
            return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(definition.name);
    }

    return {};
}

QVariant ClientReportDefinitionModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case Name:
        return tr("Name");
    case ReportType:
        return tr("Type");
    case ScheduleExpression:
        return tr("Schedule");
    case ConcurrencyPolicy:
        return tr("Concurrency");
    case Status:
        return tr("Status");
    case NextFire:
        return tr("Next Fire (UTC)");
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

void ClientReportDefinitionModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh report definition model: disconnected.";
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

void ClientReportDefinitionModel::load_page(std::uint32_t offset,
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

void ClientReportDefinitionModel::fetch_definitions(
    std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientReportDefinitionModel> self = this;

    QFuture<FetchResult> future =
        QtConcurrent::run([self, offset, limit]() -> FetchResult {
            return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug) << "Making report definitions request with offset="
                                           << offset << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false, .definitions = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                reporting::messaging::get_report_definitions_request request;

                auto result = self->clientManager_->
                    process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to fetch report definitions: "
                                               << result.error();
                    return {.success = false, .definitions = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(
                                "Failed to fetch report definitions: " + result.error()),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug) << "Fetched " << result->definitions.size()
                                           << " report definitions";
                const std::uint32_t count =
                    static_cast<std::uint32_t>(result->definitions.size());
                return {.success = true,
                        .definitions = std::move(result->definitions),
                        .total_available_count = count,
                        .error_message = {}, .error_details = {}};
            }, "report definitions");
        });

    watcher_->setFuture(future);
}

void ClientReportDefinitionModel::onDefinitionsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch report definitions: "
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
                                       << " report definitions newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " report definitions."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientReportDefinitionModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const reporting::domain::report_definition*
ClientReportDefinitionModel::getDefinition(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= definitions_.size())
        return nullptr;
    return &definitions_[idx];
}

QVariant ClientReportDefinitionModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientReportDefinitionModel::onPulseStateChanged(bool /*isOn*/) {
    if (!definitions_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientReportDefinitionModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

void ClientReportDefinitionModel::load_fsm_states() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<ClientReportDefinitionModel> self = this;
    QFuture<FsmStateFetchResult> future =
        QtConcurrent::run([self]() -> FsmStateFetchResult {
            if (!self || !self->clientManager_)
                return {false, {}};

            dq::messaging::get_fsm_states_request request;
            request.machine_name = "report_definition_lifecycle";
            auto result = self->clientManager_->process_authenticated_request(
                std::move(request));
            if (!result || !result->success)
                return {false, {}};

            QHash<QString, QString> map;
            for (const auto& s : result->states) {
                const auto id = QString::fromStdString(
                    boost::uuids::to_string(s.id));
                map[id] = QString::fromStdString(s.name);
            }
            return {true, std::move(map)};
        });
    fsm_watcher_->setFuture(future);
}

void ClientReportDefinitionModel::onFsmStatesLoaded() {
    const auto result = fsm_watcher_->result();
    if (!result.success || result.state_map.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to load FSM states for report definitions";
        return;
    }
    fsm_state_map_ = result.state_map;
    BOOST_LOG_SEV(lg(), debug) << "Loaded " << fsm_state_map_.size()
                               << " FSM states for report_definition_lifecycle";
    // Refresh Status column display if definitions are already loaded
    if (!definitions_.empty()) {
        const auto status_col = static_cast<int>(Status);
        emit dataChanged(index(0, status_col),
            index(rowCount() - 1, status_col), {Qt::DisplayRole});
    }
}

QString ClientReportDefinitionModel::resolve_fsm_state_name(
    const std::optional<boost::uuids::uuid>& state_id) const {
    if (!state_id)
        return tr("Draft");
    const auto key = QString::fromStdString(
        boost::uuids::to_string(*state_id));
    if (const auto it = fsm_state_map_.find(key); it != fsm_state_map_.end()) {
        const auto& name = it.value();
        return name.left(1).toUpper() + name.mid(1);
    }
    return tr("Unknown");
}

}
