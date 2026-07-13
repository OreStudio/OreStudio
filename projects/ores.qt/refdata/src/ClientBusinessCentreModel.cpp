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
#include "ores.qt/ClientBusinessCentreModel.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/business_centre_protocol.hpp"
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

namespace {
std::string business_centre_key_extractor(const refdata::domain::business_centre& e) {
    return e.code;
}
}

ClientBusinessCentreModel::ClientBusinessCentreModel(ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent)
    , clientManager_(clientManager)
    , watcher_(new QFutureWatcher<FetchResult>(this))
    , recencyTracker_(business_centre_key_extractor)
    , pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_,
            &QFutureWatcher<FetchResult>::finished,
            this,
            &ClientBusinessCentreModel::onCentresLoaded);

    connect(pulseManager_,
            &RecencyPulseManager::pulse_state_changed,
            this,
            &ClientBusinessCentreModel::onPulseStateChanged);
    connect(pulseManager_,
            &RecencyPulseManager::pulsing_complete,
            this,
            &ClientBusinessCentreModel::onPulsingComplete);
}

int ClientBusinessCentreModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(business_centres_.size());
}

int ClientBusinessCentreModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientBusinessCentreModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= business_centres_.size())
        return {};

    const auto& business_centre = business_centres_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
            case Code:
                return QString::fromStdString(business_centre.code);
            case Source:
                return QString::fromStdString(business_centre.source);
            case Description:
                return QString::fromStdString(business_centre.description);
            case CityName:
                return QString::fromStdString(business_centre.city_name);
            case CodingSchemeCode:
                return QString::fromStdString(business_centre.coding_scheme_code);
            case CountryAlpha2Code:
                return QString::fromStdString(business_centre.country_alpha2_code);
            case Version:
                return static_cast<qlonglong>(business_centre.version);
            case ModifiedBy:
                return QString::fromStdString(business_centre.modified_by);
            case RecordedAt:
                return relative_time_helper::format(business_centre.recorded_at);
            default:
                return {};
        }
    }

    if (role == Qt::DecorationRole && imageCache_) {
        if (index.column() == Column::CountryAlpha2Code)
            return country_flag_icon(*imageCache_, business_centre.country_alpha2_code);
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(business_centre.code);
    }

    return {};
}

QVariant
ClientBusinessCentreModel::headerData(int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
        case Code:
            return tr("Code");
        case Source:
            return tr("Source");
        case Description:
            return tr("Description");
        case CityName:
            return tr("City");
        case CodingSchemeCode:
            return tr("Coding Scheme");
        case CountryAlpha2Code:
            return tr("Country");
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

void ClientBusinessCentreModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh business centre model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!business_centres_.empty()) {
        beginResetModel();
        business_centres_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_business_centres(0, page_size_);
}

void ClientBusinessCentreModel::load_page(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!business_centres_.empty()) {
        beginResetModel();
        business_centres_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_business_centres(offset, limit);
}

void ClientBusinessCentreModel::fetch_business_centres(std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientBusinessCentreModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self, offset, limit]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>(
            [&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug)
                    << "Making business centres request with offset=" << offset
                    << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false,
                            .business_centres = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                refdata::messaging::get_business_centres_request request;
                request.offset = offset;
                request.limit = limit;

                auto result =
                    self->clientManager_->process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to send request: " << result.error();
                    return {.success = false,
                            .business_centres = {},
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
                            .business_centres = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(result->message),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug)
                    << "Fetched " << result->centres.size()
                    << " business centres, total available: " << result->total_available_count;
                return {.success = true,
                        .business_centres = std::move(result->centres),
                        .total_available_count =
                            static_cast<std::uint32_t>(result->total_available_count),
                        .error_message = {},
                        .error_details = {}};
            },
            "business centres");
    });

    watcher_->setFuture(future);
}

void ClientBusinessCentreModel::onCentresLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to fetch business centres: " << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.business_centres.size());

    if (new_count > 0) {
        beginResetModel();
        business_centres_ = std::move(result.business_centres);
        endResetModel();

        const bool has_recent = recencyTracker_.update(business_centres_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " business centres newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " business centres."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientBusinessCentreModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const refdata::domain::business_centre* ClientBusinessCentreModel::getCentre(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= business_centres_.size())
        return nullptr;
    return &business_centres_[idx];
}


QVariant ClientBusinessCentreModel::recency_foreground_color(const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientBusinessCentreModel::onPulseStateChanged(bool /*isOn*/) {
    if (!business_centres_.empty()) {
        emit dataChanged(
            index(0, 0), index(rowCount() - 1, columnCount() - 1), {Qt::ForegroundRole});
    }
}

void ClientBusinessCentreModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
