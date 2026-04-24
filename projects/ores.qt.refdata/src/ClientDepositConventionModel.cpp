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
#include "ores.qt/ClientDepositConventionModel.hpp"

#include <QtConcurrent>
#include "ores.refdata.api/messaging/deposit_convention_protocol.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    std::string deposit_convention_key_extractor(const refdata::domain::deposit_convention& e) {
        return e.id;
    }
}

ClientDepositConventionModel::ClientDepositConventionModel(
    ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(deposit_convention_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientDepositConventionModel::onConventionsLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientDepositConventionModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientDepositConventionModel::onPulsingComplete);
}

int ClientDepositConventionModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(deposit_conventions_.size());
}

int ClientDepositConventionModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientDepositConventionModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= deposit_conventions_.size())
        return {};

    const auto& dc = deposit_conventions_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Id:
            return QString::fromStdString(dc.id);
        case IndexBased:
            return dc.index_based ? tr("true") : tr("false");
        case Index:
            return dc.index
                ? QString::fromStdString(*dc.index)
                : QString{};
        case Calendar:
            return dc.calendar
                ? QString::fromStdString(*dc.calendar)
                : QString{};
        case DayCountFraction:
            return dc.day_count_fraction
                ? QString::fromStdString(*dc.day_count_fraction)
                : QString{};
        case Version:
            return static_cast<qlonglong>(dc.version);
        case ModifiedBy:
            return QString::fromStdString(dc.modified_by);
        case RecordedAt:
            return relative_time_helper::format(dc.recorded_at);
        default:
            return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(dc.id);
    }

    return {};
}

QVariant ClientDepositConventionModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case Id:
        return tr("Id");
    case IndexBased:
        return tr("Index Based");
    case Index:
        return tr("Index");
    case Calendar:
        return tr("Calendar");
    case DayCountFraction:
        return tr("DCF");
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

void ClientDepositConventionModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh deposit convention model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!deposit_conventions_.empty()) {
        beginResetModel();
        deposit_conventions_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_deposit_conventions(0, page_size_);
}

void ClientDepositConventionModel::load_page(std::uint32_t offset,
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

    if (!deposit_conventions_.empty()) {
        beginResetModel();
        deposit_conventions_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_deposit_conventions(offset, limit);
}

void ClientDepositConventionModel::fetch_deposit_conventions(
    std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientDepositConventionModel> self = this;

    QFuture<FetchResult> future =
        QtConcurrent::run([self, offset, limit]() -> FetchResult {
            return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug) << "Making deposit conventions request with offset="
                                           << offset << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false, .deposit_conventions = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                refdata::messaging::get_deposit_conventions_request request;

                auto result = self->clientManager_->
                    process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to send request: " << result.error();
                    return {.success = false, .deposit_conventions = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(result.error()),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug) << "Fetched " << result->deposit_conventions.size()
                                           << " deposit conventions";
                const std::uint32_t count =
                    static_cast<std::uint32_t>(result->deposit_conventions.size());
                return {.success = true,
                        .deposit_conventions = std::move(result->deposit_conventions),
                        .total_available_count = count,
                        .error_message = {}, .error_details = {}};
            }, "deposit conventions");
        });

    watcher_->setFuture(future);
}

void ClientDepositConventionModel::onConventionsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch deposit conventions: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.deposit_conventions.size());

    if (new_count > 0) {
        beginResetModel();
        deposit_conventions_ = std::move(result.deposit_conventions);
        endResetModel();

        const bool has_recent = recencyTracker_.update(deposit_conventions_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " deposit conventions newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " deposit conventions."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientDepositConventionModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const refdata::domain::deposit_convention*
ClientDepositConventionModel::getConvention(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= deposit_conventions_.size())
        return nullptr;
    return &deposit_conventions_[idx];
}

QVariant ClientDepositConventionModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientDepositConventionModel::onPulseStateChanged(bool /*isOn*/) {
    if (!deposit_conventions_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientDepositConventionModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
