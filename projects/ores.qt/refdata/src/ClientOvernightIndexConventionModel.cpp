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
#include "ores.qt/ClientOvernightIndexConventionModel.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/overnight_index_convention_protocol.hpp"
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

namespace {
std::string
overnight_index_convention_key_extractor(const refdata::domain::overnight_index_convention& e) {
    return e.id;
}
}

ClientOvernightIndexConventionModel::ClientOvernightIndexConventionModel(
    ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent)
    , clientManager_(clientManager)
    , watcher_(new QFutureWatcher<FetchResult>(this))
    , recencyTracker_(overnight_index_convention_key_extractor)
    , pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_,
            &QFutureWatcher<FetchResult>::finished,
            this,
            &ClientOvernightIndexConventionModel::onConventionsLoaded);

    connect(pulseManager_,
            &RecencyPulseManager::pulse_state_changed,
            this,
            &ClientOvernightIndexConventionModel::onPulseStateChanged);
    connect(pulseManager_,
            &RecencyPulseManager::pulsing_complete,
            this,
            &ClientOvernightIndexConventionModel::onPulsingComplete);
}

int ClientOvernightIndexConventionModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(overnight_index_conventions_.size());
}

int ClientOvernightIndexConventionModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientOvernightIndexConventionModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= overnight_index_conventions_.size())
        return {};

    const auto& ni = overnight_index_conventions_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
            case Id:
                return QString::fromStdString(ni.id);
            case FixingCalendar:
                return QString::fromStdString(ni.fixing_calendar);
            case DayCountFraction:
                return QString::fromStdString(ni.day_count_fraction);
            case SettlementDays:
                return static_cast<qlonglong>(ni.settlement_days);
            case Version:
                return static_cast<qlonglong>(ni.version);
            case ModifiedBy:
                return QString::fromStdString(ni.modified_by);
            case RecordedAt:
                return relative_time_helper::format(ni.recorded_at);
            default:
                return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(ni.id);
    }

    return {};
}

QVariant ClientOvernightIndexConventionModel::headerData(int section,
                                                         Qt::Orientation orientation,
                                                         int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
        case Id:
            return tr("Id");
        case FixingCalendar:
            return tr("Fixing Calendar");
        case DayCountFraction:
            return tr("DCF");
        case SettlementDays:
            return tr("Settlement Days");
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

void ClientOvernightIndexConventionModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn)
            << "Cannot refresh overnight index convention model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!overnight_index_conventions_.empty()) {
        beginResetModel();
        overnight_index_conventions_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_overnight_index_conventions(0, page_size_);
}

void ClientOvernightIndexConventionModel::load_page(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!overnight_index_conventions_.empty()) {
        beginResetModel();
        overnight_index_conventions_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_overnight_index_conventions(offset, limit);
}

void ClientOvernightIndexConventionModel::fetch_overnight_index_conventions(std::uint32_t offset,
                                                                            std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientOvernightIndexConventionModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self, offset, limit]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>(
            [&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug)
                    << "Making overnight index conventions request with offset=" << offset
                    << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false,
                            .overnight_index_conventions = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                refdata::messaging::get_overnight_index_conventions_request request;

                auto result =
                    self->clientManager_->process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to send request: " << result.error();
                    return {.success = false,
                            .overnight_index_conventions = {},
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
                            .overnight_index_conventions = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(result->message),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug)
                    << "Fetched " << result->overnight_index_conventions.size()
                    << " overnight index conventions";
                const std::uint32_t count =
                    static_cast<std::uint32_t>(result->overnight_index_conventions.size());
                return {.success = true,
                        .overnight_index_conventions =
                            std::move(result->overnight_index_conventions),
                        .total_available_count = count,
                        .error_message = {},
                        .error_details = {}};
            },
            "overnight index conventions");
    });

    watcher_->setFuture(future);
}

void ClientOvernightIndexConventionModel::onConventionsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch overnight index conventions: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.overnight_index_conventions.size());

    if (new_count > 0) {
        beginResetModel();
        overnight_index_conventions_ = std::move(result.overnight_index_conventions);
        endResetModel();

        const bool has_recent = recencyTracker_.update(overnight_index_conventions_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " overnight index conventions newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " overnight index conventions."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientOvernightIndexConventionModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const refdata::domain::overnight_index_convention*
ClientOvernightIndexConventionModel::getConvention(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= overnight_index_conventions_.size())
        return nullptr;
    return &overnight_index_conventions_[idx];
}


QVariant
ClientOvernightIndexConventionModel::recency_foreground_color(const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientOvernightIndexConventionModel::onPulseStateChanged(bool /*isOn*/) {
    if (!overnight_index_conventions_.empty()) {
        emit dataChanged(
            index(0, 0), index(rowCount() - 1, columnCount() - 1), {Qt::ForegroundRole});
    }
}

void ClientOvernightIndexConventionModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
