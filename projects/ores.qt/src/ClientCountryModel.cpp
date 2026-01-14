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
#include "ores.qt/ClientCountryModel.hpp"

#include <algorithm>
#include <unordered_set>
#include <QtConcurrent>
#include <QColor>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.comms/net/client_session.hpp"
#include "ores.refdata/messaging/protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

ClientCountryModel::
ClientCountryModel(ClientManager* clientManager, ImageCache* imageCache,
    QObject* parent)
    : QAbstractTableModel(parent), clientManager_(clientManager),
      imageCache_(imageCache),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      pulse_timer_(new QTimer(this)) {

    connect(watcher_,
        &QFutureWatcher<FetchResult>::finished,
        this, &ClientCountryModel::onCountriesLoaded);

    connect(pulse_timer_, &QTimer::timeout,
        this, &ClientCountryModel::onPulseTimerTimeout);

    // Connect to image cache to refresh decorations when images are loaded
    if (imageCache_) {
        connect(imageCache_, &ImageCache::imagesLoaded, this, [this]() {
            if (!countries_.empty()) {
                emit dataChanged(index(0, Column::Flag),
                    index(rowCount() - 1, Column::Flag),
                    {Qt::DecorationRole});
            }
        });

        // Also refresh when individual images load (on-demand loading)
        connect(imageCache_, &ImageCache::imageLoaded, this, [this](const QString&) {
            if (!countries_.empty()) {
                emit dataChanged(index(0, Column::Flag),
                    index(rowCount() - 1, Column::Flag),
                    {Qt::DecorationRole});
            }
        });
    }
}

int ClientCountryModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(countries_.size());
}

int ClientCountryModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return Column::ColumnCount;
}

QVariant ClientCountryModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= countries_.size())
        return {};

    const auto& country = countries_[row];

    // Handle DecorationRole for Flag column
    if (role == Qt::DecorationRole && index.column() == Column::Flag) {
        if (imageCache_) {
            if (country.image_id) {
                const auto image_id_str = boost::uuids::to_string(*country.image_id);
                return imageCache_->getIcon(image_id_str);
            }
            return imageCache_->getNoFlagIcon();
        }
        return {};
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(country.alpha2_code);
    }

    if (role != Qt::DisplayRole)
        return {};

    switch (index.column()) {
    case Column::Flag: return {};  // No text for flag column
    case Column::Name: return QString::fromStdString(country.name);
    case Column::Alpha2Code: return QString::fromStdString(country.alpha2_code);
    case Column::Alpha3Code: return QString::fromStdString(country.alpha3_code);
    case Column::NumericCode: return QString::fromStdString(country.numeric_code);
    case Column::OfficialName: return QString::fromStdString(country.official_name);
    case Column::Version: return country.version;
    case Column::RecordedBy: return QString::fromStdString(country.recorded_by);
    case Column::RecordedAt: return relative_time_helper::format(country.recorded_at);
    default: return {};
    }
}

QVariant ClientCountryModel::
headerData(int section, Qt::Orientation orientation, int role) const {
    if (role != Qt::DisplayRole)
        return {};

    if (orientation == Qt::Horizontal) {
        switch (section) {
        case Column::Flag: return tr("Flag");
        case Column::Name: return tr("Name");
        case Column::Alpha2Code: return tr("Alpha-2");
        case Column::Alpha3Code: return tr("Alpha-3");
        case Column::NumericCode: return tr("Numeric");
        case Column::OfficialName: return tr("Official Name");
        case Column::Version: return tr("Version");
        case Column::RecordedBy: return tr("Recorded By");
        case Column::RecordedAt: return tr("Recorded At");
        default: return {};
        }
    }

    return {};
}

void ClientCountryModel::refresh(bool replace) {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh (replace=" << replace << ").";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh country model: disconnected.";
        return;
    }

    const std::uint32_t offset = replace ? 0 : static_cast<std::uint32_t>(countries_.size());

    if (replace) {
        if (!countries_.empty()) {
            beginResetModel();
            countries_.clear();
            recent_alpha2_codes_.clear();
            pulse_timer_->stop();
            pulse_count_ = 0;
            pulse_state_ = false;
            total_available_count_ = 0;
            endResetModel();
        }
    }

    fetch_countries(offset, page_size_);
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
        recent_alpha2_codes_.clear();
        pulse_timer_->stop();
        pulse_count_ = 0;
        pulse_state_ = false;
        endResetModel();
    }

    fetch_countries(offset, limit);
}

void ClientCountryModel::fetch_countries(std::uint32_t offset,
                                          std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientCountryModel> self = this;

    QFuture<FetchResult> future =
        QtConcurrent::run([self, offset, limit]() -> FetchResult {
            BOOST_LOG_SEV(lg(), debug) << "Making a countries request with offset="
                                       << offset << ", limit=" << limit;
            if (!self) return {false, {}, 0};

            refdata::messaging::get_countries_request request;
            request.offset = offset;
            request.limit = limit;

            auto result = self->clientManager_->
                process_authenticated_request(std::move(request));

            if (!result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to fetch countries: "
                                           << comms::net::to_string(result.error());
                return {false, {}, 0};
            }

            BOOST_LOG_SEV(lg(), debug) << "Received " << result->countries.size()
                                       << " countries, total available: "
                                       << result->total_available_count;

            return {true, std::move(result->countries),
                    result->total_available_count};
        });

     watcher_->setFuture(future);
}

void ClientCountryModel::onCountriesLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "On countries loaded event.";
    is_fetching_ = false;

    auto result = watcher_->result();
    if (result.success) {
        total_available_count_ = result.total_available_count;

        // Build set of existing alpha-2 codes for duplicate detection
        std::unordered_set<std::string> existing_codes;
        for (const auto& country : countries_) {
            existing_codes.insert(country.alpha2_code);
        }

        // Filter out duplicates from new results
        std::vector<refdata::domain::country> new_countries;
        for (auto& country : result.countries) {
            if (existing_codes.find(country.alpha2_code) == existing_codes.end()) {
                new_countries.push_back(std::move(country));
                existing_codes.insert(country.alpha2_code);
            } else {
                BOOST_LOG_SEV(lg(), trace) << "Skipping duplicate country: "
                                           << country.alpha2_code;
            }
        }

        const int old_size = static_cast<int>(countries_.size());
        const int new_count = static_cast<int>(new_countries.size());

        if (new_count > 0) {
            beginInsertRows(QModelIndex(), old_size, old_size + new_count - 1);
            countries_.insert(countries_.end(),
                std::make_move_iterator(new_countries.begin()),
                std::make_move_iterator(new_countries.end()));
            endInsertRows();

            update_recent_countries();

            if (!recent_alpha2_codes_.empty() && !pulse_timer_->isActive()) {
                pulse_count_ = 0;
                pulse_state_ = true;
                pulse_timer_->start(pulse_interval_ms_);
            }
        }

        BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " new countries "
                                  << "(received " << result.countries.size()
                                  << ", filtered " << (result.countries.size() - new_count)
                                  << " duplicates). Total in model: " << countries_.size()
                                  << ", Total available: " << total_available_count_;

        emit dataLoaded();
    } else {
        BOOST_LOG_SEV(lg(), error) << "Countries request failed: no response.";
        emit loadError(tr("Failed to load countries from server"));
    }
}

const refdata::domain::country* ClientCountryModel::getCountry(int row) const {
    if (row < 0 || row >= static_cast<int>(countries_.size()))
        return nullptr;

    return &countries_[row];
}

std::vector<refdata::domain::country> ClientCountryModel::getCountries() const {
    return countries_;
}

bool ClientCountryModel::canFetchMore(const QModelIndex& parent) const {
    if (parent.isValid())
        return false;

    // For Qt's automatic fetch-more, we only allow it for appending data
    // (e.g., infinite scroll). Page-based navigation uses load_page() instead.
    const bool has_more = countries_.size() < page_size_ &&
                          countries_.size() < total_available_count_;

    BOOST_LOG_SEV(lg(), trace) << "canFetchMore: " << has_more
                               << " (loaded: " << countries_.size()
                               << ", page_size: " << page_size_
                               << ", available: " << total_available_count_ << ")";
    return has_more && !is_fetching_;
}

void ClientCountryModel::fetchMore(const QModelIndex& parent) {
    if (parent.isValid() || is_fetching_)
        return;

    BOOST_LOG_SEV(lg(), debug) << "fetchMore called, loading next page.";
    refresh(false);
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

void ClientCountryModel::update_recent_countries() {
    recent_alpha2_codes_.clear();

    const QDateTime now = QDateTime::currentDateTime();

    // First load: set baseline timestamp, no highlighting
    if (!last_reload_time_.isValid()) {
        last_reload_time_ = now;
        BOOST_LOG_SEV(lg(), debug) << "First load - setting baseline timestamp: "
                                   << last_reload_time_.toString(Qt::ISODate).toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Checking countries newer than last reload: "
                               << last_reload_time_.toString(Qt::ISODate).toStdString();

    // Find countries with recorded_at newer than last reload
    for (const auto& country : countries_) {
        if (country.recorded_at == std::chrono::system_clock::time_point{}) {
            continue;
        }

        const auto msecs = std::chrono::duration_cast<std::chrono::milliseconds>(
            country.recorded_at.time_since_epoch()).count();
        QDateTime recordedAt = QDateTime::fromMSecsSinceEpoch(msecs);

        if (recordedAt.isValid() && recordedAt > last_reload_time_) {
            recent_alpha2_codes_.insert(country.alpha2_code);
            BOOST_LOG_SEV(lg(), trace) << "Country " << country.alpha2_code
                                       << " is recent";
        }
    }

    last_reload_time_ = now;

    BOOST_LOG_SEV(lg(), debug) << "Found " << recent_alpha2_codes_.size()
                               << " countries newer than last reload";
}

QVariant ClientCountryModel::
recency_foreground_color(const std::string& alpha2_code) const {
    // Recent countries show yellow when pulsing
    if (recent_alpha2_codes_.find(alpha2_code) != recent_alpha2_codes_.end() && pulse_state_) {
        return color_constants::stale_indicator;
    }

    return {};
}

void ClientCountryModel::onPulseTimerTimeout() {
    pulse_state_ = !pulse_state_;
    pulse_count_++;

    // Stop pulsing after max cycles but keep yellow color on
    if (pulse_count_ >= max_pulse_cycles_) {
        pulse_timer_->stop();
        pulse_state_ = true;
        BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete, staying highlighted";
    }

    // Emit dataChanged to update the view with new colors
    if (!countries_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

}
