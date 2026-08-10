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
#include "ores.qt/ClientTenorScheduleModel.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/tenor_schedule_protocol.hpp"
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

namespace {
std::string tenor_schedule_key_extractor(const refdata::domain::tenor_schedule& e) {
    return e.code;
}
}

ClientTenorScheduleModel::ClientTenorScheduleModel(ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent)
    , clientManager_(clientManager)
    , watcher_(new QFutureWatcher<FetchResult>(this))
    , recencyTracker_(tenor_schedule_key_extractor)
    , pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_,
            &QFutureWatcher<FetchResult>::finished,
            this,
            &ClientTenorScheduleModel::onSchedulesLoaded);

    connect(pulseManager_,
            &RecencyPulseManager::pulse_state_changed,
            this,
            &ClientTenorScheduleModel::onPulseStateChanged);
    connect(pulseManager_,
            &RecencyPulseManager::pulsing_complete,
            this,
            &ClientTenorScheduleModel::onPulsingComplete);
}

int ClientTenorScheduleModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(schedules_.size());
}

int ClientTenorScheduleModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientTenorScheduleModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= schedules_.size())
        return {};

    const auto& schedule = schedules_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
            case Code:
                return QString::fromStdString(schedule.code);
            case Name:
                return QString::fromStdString(schedule.name);
            case Description:
                return QString::fromStdString(schedule.description);
            case ScheduleSource:
                return QString::fromStdString(schedule.schedule_source);
            case CalendarCode:
                return schedule.calendar_code ? QString::fromStdString(*schedule.calendar_code) :
                                                QString{};
            case DiaryEntryType:
                return schedule.diary_entry_type ?
                           QString::fromStdString(*schedule.diary_entry_type) :
                           QString{};
            case DisplayOrder:
                return static_cast<qlonglong>(schedule.display_order);
            case Version:
                return static_cast<qlonglong>(schedule.version);
            case ModifiedBy:
                return QString::fromStdString(schedule.modified_by);
            case RecordedAt:
                return relative_time_helper::format(schedule.recorded_at);
            default:
                return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(schedule.code);
    }

    return {};
}

QVariant
ClientTenorScheduleModel::headerData(int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || (role != Qt::DisplayRole && role != Qt::ToolTipRole))
        return {};

    if (role == Qt::ToolTipRole) {
        switch (section) {
            default:
                return {};
        }
    }

    switch (section) {
        case Code:
            return tr("Code");
        case Name:
            return tr("Name");
        case Description:
            return tr("Description");
        case ScheduleSource:
            return tr("Schedule Source");
        case CalendarCode:
            return tr("Calendar");
        case DiaryEntryType:
            return tr("Diary Entry Type");
        case DisplayOrder:
            return tr("Display Order");
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

void ClientTenorScheduleModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh tenor schedule model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!schedules_.empty()) {
        beginResetModel();
        schedules_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_schedules(0, page_size_);
}

void ClientTenorScheduleModel::load_page(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!schedules_.empty()) {
        beginResetModel();
        schedules_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_schedules(offset, limit);
}

void ClientTenorScheduleModel::fetch_schedules(std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientTenorScheduleModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self, offset, limit]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>(
            [&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug)
                    << "Making tenor schedules request with offset=" << offset
                    << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false,
                            .schedules = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                refdata::messaging::get_tenor_schedules_request request;
                request.offset = offset;
                request.limit = limit;

                auto result =
                    self->clientManager_->process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to send request: " << result.error();
                    return {.success = false,
                            .schedules = {},
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
                            .schedules = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(result->message),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug)
                    << "Fetched " << result->schedules.size()
                    << " tenor schedules, total available: " << result->total_available_count;
                return {.success = true,
                        .schedules = std::move(result->schedules),
                        .total_available_count =
                            static_cast<std::uint32_t>(result->total_available_count),
                        .error_message = {},
                        .error_details = {}};
            },
            "tenor schedules");
    });

    watcher_->setFuture(future);
}

void ClientTenorScheduleModel::onSchedulesLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to fetch tenor schedules: " << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.schedules.size());

    if (new_count > 0) {
        beginResetModel();
        schedules_ = std::move(result.schedules);
        endResetModel();

        const bool has_recent = recencyTracker_.update(schedules_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " tenor schedules newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " tenor schedules."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientTenorScheduleModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const refdata::domain::tenor_schedule* ClientTenorScheduleModel::getSchedule(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= schedules_.size())
        return nullptr;
    return &schedules_[idx];
}


QVariant ClientTenorScheduleModel::recency_foreground_color(const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientTenorScheduleModel::onPulseStateChanged(bool /*isOn*/) {
    if (!schedules_.empty()) {
        emit dataChanged(
            index(0, 0), index(rowCount() - 1, columnCount() - 1), {Qt::ForegroundRole});
    }
}

void ClientTenorScheduleModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
