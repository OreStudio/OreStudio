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
#include "ores.qt/ClientCountryModel.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/country_protocol.hpp"
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

namespace {
std::string country_key_extractor(const refdata::domain::country& e) {
    return e.alpha2_code;
}
}

ClientCountryModel::ClientCountryModel(ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent)
    , clientManager_(clientManager)
    , watcher_(new QFutureWatcher<FetchResult>(this))
    , recencyTracker_(country_key_extractor)
    , pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_,
            &QFutureWatcher<FetchResult>::finished,
            this,
            &ClientCountryModel::onCountriesLoaded);

    connect(pulseManager_,
            &RecencyPulseManager::pulse_state_changed,
            this,
            &ClientCountryModel::onPulseStateChanged);
    connect(pulseManager_,
            &RecencyPulseManager::pulsing_complete,
            this,
            &ClientCountryModel::onPulsingComplete);
}

int ClientCountryModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(countries_.size());
}

int ClientCountryModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientCountryModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= countries_.size())
        return {};

    const auto& country = countries_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
            case Alpha2Code:
                return QString::fromStdString(country.alpha2_code);
            case Alpha3Code:
                return QString::fromStdString(country.alpha3_code);
            case NumericCode:
                return QString::fromStdString(country.numeric_code);
            case Name:
                return QString::fromStdString(country.name);
            case OfficialName:
                return QString::fromStdString(country.official_name);
            case Version:
                return static_cast<qlonglong>(country.version);
            case ModifiedBy:
                return QString::fromStdString(country.modified_by);
            case RecordedAt:
                return relative_time_helper::format(country.recorded_at);
            default:
                return {};
        }
    }

    if (role == Qt::DecorationRole && index.column() == iconColumn()) {
        return flagDecoration(country.image_id);
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(country.alpha2_code);
    }

    return {};
}

QVariant ClientCountryModel::headerData(int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
        case Alpha2Code:
            return tr("Alpha-2 Code");
        case Alpha3Code:
            return tr("Alpha-3 Code");
        case NumericCode:
            return tr("Numeric Code");
        case Name:
            return tr("Name");
        case OfficialName:
            return tr("Official Name");
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

void ClientCountryModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh country model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!countries_.empty()) {
        beginResetModel();
        countries_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_countries(0, page_size_);
}

void ClientCountryModel::load_page(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!countries_.empty()) {
        beginResetModel();
        countries_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_countries(offset, limit);
}

void ClientCountryModel::fetch_countries(std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientCountryModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self, offset, limit]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>(
            [&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug)
                    << "Making countries request with offset=" << offset << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false,
                            .countries = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                refdata::messaging::get_countries_request request;
                request.offset = offset;
                request.limit = limit;

                auto result =
                    self->clientManager_->process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to send request: " << result.error();
                    return {.success = false,
                            .countries = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(result.error()),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug)
                    << "Fetched " << result->countries.size()
                    << " countries, total available: " << result->total_available_count;
                return {.success = true,
                        .countries = std::move(result->countries),
                        .total_available_count =
                            static_cast<std::uint32_t>(result->total_available_count),
                        .error_message = {},
                        .error_details = {}};
            },
            "countries");
    });

    watcher_->setFuture(future);
}

void ClientCountryModel::onCountriesLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to fetch countries: " << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.countries.size());

    if (new_count > 0) {
        beginResetModel();
        countries_ = std::move(result.countries);
        endResetModel();

        const bool has_recent = recencyTracker_.update(countries_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " countries newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " countries."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientCountryModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const refdata::domain::country* ClientCountryModel::getCountry(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= countries_.size())
        return nullptr;
    return &countries_[idx];
}

QVariant ClientCountryModel::recency_foreground_color(const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientCountryModel::onPulseStateChanged(bool /*isOn*/) {
    if (!countries_.empty()) {
        emit dataChanged(
            index(0, 0), index(rowCount() - 1, columnCount() - 1), {Qt::ForegroundRole});
    }
}

void ClientCountryModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
