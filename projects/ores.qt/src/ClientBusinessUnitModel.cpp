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
#include "ores.qt/ClientBusinessUnitModel.hpp"

#include <QtConcurrent>
#include "ores.refdata/messaging/business_unit_protocol.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.comms/net/client_session.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    std::string business_unit_key_extractor(const refdata::domain::business_unit& e) {
        return e.unit_code;
    }
}

ClientBusinessUnitModel::ClientBusinessUnitModel(
    ClientManager* clientManager, ImageCache* imageCache, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      imageCache_(imageCache),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(business_unit_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientBusinessUnitModel::onUnitsLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientBusinessUnitModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientBusinessUnitModel::onPulsingComplete);

    if (imageCache_) {
        connect(imageCache_, &ImageCache::imagesLoaded, this, [this]() {
            if (!business_units_.empty()) {
                emit dataChanged(index(0, Column::BusinessCentreCode),
                    index(rowCount() - 1, Column::BusinessCentreCode),
                    {Qt::DecorationRole});
            }
        });
        connect(imageCache_, &ImageCache::imageLoaded, this, [this](const QString&) {
            if (!business_units_.empty()) {
                emit dataChanged(index(0, Column::BusinessCentreCode),
                    index(rowCount() - 1, Column::BusinessCentreCode),
                    {Qt::DecorationRole});
            }
        });
    }
}

int ClientBusinessUnitModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(business_units_.size());
}

int ClientBusinessUnitModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientBusinessUnitModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= business_units_.size())
        return {};

    const auto& business_unit = business_units_[row];

    if (role == Qt::DecorationRole && index.column() == Column::BusinessCentreCode) {
        if (imageCache_ && !business_unit.business_centre_code.empty()) {
            return imageCache_->getBusinessCentreFlagIcon(
                business_unit.business_centre_code);
        }
        return {};
    }

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case UnitCode:
            return QString::fromStdString(business_unit.unit_code);
        case UnitName:
            return QString::fromStdString(business_unit.unit_name);
        case BusinessCentreCode:
            return QString::fromStdString(business_unit.business_centre_code);
        case Status:
            return QString::fromStdString(business_unit.status);
        case Version:
            return business_unit.version;
        case ModifiedBy:
            return QString::fromStdString(business_unit.modified_by);
        case RecordedAt:
            return relative_time_helper::format(business_unit.recorded_at);
        default:
            return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(business_unit.unit_code);
    }

    return {};
}

QVariant ClientBusinessUnitModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case UnitCode:
        return tr("Code");
    case UnitName:
        return tr("Name");
    case BusinessCentreCode:
        return tr("Centre");
    case Status:
        return tr("Status");
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

void ClientBusinessUnitModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh business unit model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!business_units_.empty()) {
        beginResetModel();
        business_units_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_business_units(0, page_size_);
}

void ClientBusinessUnitModel::load_page(std::uint32_t offset,
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

    if (!business_units_.empty()) {
        beginResetModel();
        business_units_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_business_units(offset, limit);
}

void ClientBusinessUnitModel::fetch_business_units(
    std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientBusinessUnitModel> self = this;

    QFuture<FetchResult> future =
        QtConcurrent::run([self, offset, limit]() -> FetchResult {
            return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug) << "Making business units request with offset="
                                           << offset << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false, .business_units = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                refdata::messaging::get_business_units_request request;
                request.offset = offset;
                request.limit = limit;

                auto result = self->clientManager_->
                    process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to fetch business units: "
                                               << comms::net::to_string(result.error());
                    return {.success = false, .business_units = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(
                                "Failed to fetch business units: " + comms::net::to_string(result.error())),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug) << "Fetched " << result->business_units.size()
                                           << " business units, total available: "
                                           << result->total_available_count;
                return {.success = true,
                        .business_units = std::move(result->business_units),
                        .total_available_count = result->total_available_count,
                        .error_message = {}, .error_details = {}};
            }, "business units");
        });

    watcher_->setFuture(future);
}

void ClientBusinessUnitModel::onUnitsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch business units: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.business_units.size());

    if (new_count > 0) {
        beginResetModel();
        business_units_ = std::move(result.business_units);
        endResetModel();

        const bool has_recent = recencyTracker_.update(business_units_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " business units newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " business units."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientBusinessUnitModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const refdata::domain::business_unit*
ClientBusinessUnitModel::getUnit(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= business_units_.size())
        return nullptr;
    return &business_units_[idx];
}

QVariant ClientBusinessUnitModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientBusinessUnitModel::onPulseStateChanged(bool /*isOn*/) {
    if (!business_units_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientBusinessUnitModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
