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
#include "ores.qt/ClientCalendarExceptionModel.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/calendar_exception_protocol.hpp"
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

namespace {
std::string calendar_exception_key_extractor(const refdata::domain::calendar_exception& e) {
    return boost::uuids::to_string(e.id);
}
}

ClientCalendarExceptionModel::ClientCalendarExceptionModel(ClientManager* clientManager,
                                                           QObject* parent)
    : AbstractClientModel(parent)
    , clientManager_(clientManager)
    , watcher_(new QFutureWatcher<FetchResult>(this))
    , recencyTracker_(calendar_exception_key_extractor)
    , pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_,
            &QFutureWatcher<FetchResult>::finished,
            this,
            &ClientCalendarExceptionModel::onExceptionsLoaded);

    connect(pulseManager_,
            &RecencyPulseManager::pulse_state_changed,
            this,
            &ClientCalendarExceptionModel::onPulseStateChanged);
    connect(pulseManager_,
            &RecencyPulseManager::pulsing_complete,
            this,
            &ClientCalendarExceptionModel::onPulsingComplete);
}

int ClientCalendarExceptionModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(calendar_exceptions_.size());
}

int ClientCalendarExceptionModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientCalendarExceptionModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= calendar_exceptions_.size())
        return {};

    const auto& exc = calendar_exceptions_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
            case CalendarCode:
                return QString::fromStdString(exc.calendar_code);
            case ExceptionDate:
                return QString::fromStdString(
                    ores::platform::time::datetime::to_iso8601_date(exc.exception_date));
            case IsBusinessDay:
                return exc.is_business_day ? tr("true") : tr("false");
            case Description:
                return exc.description ? QString::fromStdString(*exc.description) : QString{};
            case Version:
                return static_cast<qlonglong>(exc.version);
            case ModifiedBy:
                return QString::fromStdString(exc.modified_by);
            default:
                return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(boost::uuids::to_string(exc.id));
    }

    return {};
}

QVariant
ClientCalendarExceptionModel::headerData(int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || (role != Qt::DisplayRole && role != Qt::ToolTipRole))
        return {};

    if (role == Qt::ToolTipRole) {
        switch (section) {
            default:
                return {};
        }
    }

    switch (section) {
        case CalendarCode:
            return tr("Calendar");
        case ExceptionDate:
            return tr("Date");
        case IsBusinessDay:
            return tr("Business Day");
        case Description:
            return tr("Description");
        case Version:
            return tr("Version");
        case ModifiedBy:
            return tr("Modified By");
        default:
            return {};
    }
}

void ClientCalendarExceptionModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh calendar exception model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!calendar_exceptions_.empty()) {
        beginResetModel();
        calendar_exceptions_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_calendar_exceptions(0, page_size_);
}

void ClientCalendarExceptionModel::load_page(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!calendar_exceptions_.empty()) {
        beginResetModel();
        calendar_exceptions_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_calendar_exceptions(offset, limit);
}

void ClientCalendarExceptionModel::fetch_calendar_exceptions(std::uint32_t offset,
                                                             std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientCalendarExceptionModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self, offset, limit]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>(
            [&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug)
                    << "Making calendar exceptions request with offset=" << offset
                    << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false,
                            .calendar_exceptions = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                refdata::messaging::get_calendar_exceptions_request request;
                request.offset = offset;
                request.limit = limit;

                auto result =
                    self->clientManager_->process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to send request: " << result.error();
                    return {.success = false,
                            .calendar_exceptions = {},
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
                            .calendar_exceptions = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(result->message),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug)
                    << "Fetched " << result->calendar_exceptions.size()
                    << " calendar exceptions, total available: " << result->total_available_count;
                return {.success = true,
                        .calendar_exceptions = std::move(result->calendar_exceptions),
                        .total_available_count =
                            static_cast<std::uint32_t>(result->total_available_count),
                        .error_message = {},
                        .error_details = {}};
            },
            "calendar exceptions");
    });

    watcher_->setFuture(future);
}

void ClientCalendarExceptionModel::onExceptionsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to fetch calendar exceptions: " << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.calendar_exceptions.size());

    if (new_count > 0) {
        beginResetModel();
        calendar_exceptions_ = std::move(result.calendar_exceptions);
        endResetModel();

        const bool has_recent = recencyTracker_.update(calendar_exceptions_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " calendar exceptions newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " calendar exceptions."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientCalendarExceptionModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const refdata::domain::calendar_exception*
ClientCalendarExceptionModel::getException(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= calendar_exceptions_.size())
        return nullptr;
    return &calendar_exceptions_[idx];
}


QVariant ClientCalendarExceptionModel::recency_foreground_color(const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientCalendarExceptionModel::onPulseStateChanged(bool /*isOn*/) {
    if (!calendar_exceptions_.empty()) {
        emit dataChanged(
            index(0, 0), index(rowCount() - 1, columnCount() - 1), {Qt::ForegroundRole});
    }
}

void ClientCalendarExceptionModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
