/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/ClientCurrencyModel.hpp"

#include <algorithm>
#include <unordered_set>
#include <QtConcurrent>
#include <QColor>
#include <QDateTime>
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.comms/messaging/message_types.hpp"
#include "ores.risk/messaging/protocol.hpp"

namespace ores::qt {

using comms::messaging::frame;
using comms::messaging::message_type;
using namespace ores::utility::log;

ClientCurrencyModel::
ClientCurrencyModel(ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent), clientManager_(clientManager),
      watcher_(new QFutureWatcher<FutureWatcherResult>(this)),
      pulse_timer_(new QTimer(this)) {

    connect(watcher_,
        &QFutureWatcher<FutureWatcherResult>::finished,
        this, &ClientCurrencyModel::onCurrenciesLoaded);

    // Setup pulse timer for recency color updates (same interval as reload button)
    connect(pulse_timer_, &QTimer::timeout,
        this, &ClientCurrencyModel::onPulseTimerTimeout);
}

int ClientCurrencyModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(currencies_.size());
}

int ClientCurrencyModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return Column::ColumnCount;
}

QVariant ClientCurrencyModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= currencies_.size())
        return {};

    const auto& currency = currencies_[row];

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(currency.iso_code);
    }

    if (role != Qt::DisplayRole)
        return {};

    switch (index.column()) {
    case Column::CurrencyName: return QString::fromStdString(currency.name);
    case Column::IsoCode: return QString::fromStdString(currency.iso_code);
    case Column::Version: return currency.version;
    case Column::NumericCode: return QString::fromStdString(currency.numeric_code);
    case Column::Symbol: return QString::fromStdString(currency.symbol);
    case Column::FractionSymbol: return QString::fromStdString(currency.fraction_symbol);
    case Column::FractionsPerUnit: return currency.fractions_per_unit;
    case Column::RoundingType: return QString::fromStdString(currency.rounding_type);
    case Column::RoundingPrecision: return currency.rounding_precision;
    case Column::Format: return QString::fromStdString(currency.format);
    case Column::CurrencyType: return QString::fromStdString(currency.currency_type);
    case Column::RecordedBy: return QString::fromStdString(currency.recorded_by);
    case Column::RecordedAt: return relative_time_helper::format(currency.recorded_at);
    default: return {};
    }
}

QVariant ClientCurrencyModel::
headerData(int section, Qt::Orientation orientation, int role) const {
    if (role != Qt::DisplayRole)
        return {};

    if (orientation == Qt::Horizontal) {
        switch (section) {
        case Column::CurrencyName: return tr("Currency Name");
        case Column::IsoCode: return tr("ISO Code");
        case Column::Version: return tr("Version");
        case Column::NumericCode: return tr("Numeric Code");
        case Column::Symbol: return tr("Symbol");
        case Column::FractionSymbol: return tr("Frac. Symbol");
        case Column::FractionsPerUnit: return tr("Frac. per unit");
        case Column::RoundingType: return tr("Rounding type");
        case Column::RoundingPrecision: return tr("Rounding precision");
        case Column::Format: return tr("Format");
        case Column::CurrencyType: return tr("Currency Type");
        case Column::RecordedBy: return tr("Recorded By");
        case Column::RecordedAt: return tr("Recorded At");
        default: return {};
        }
    }

    return {};
}

void ClientCurrencyModel::refresh(bool replace) {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh (replace=" << replace << ").";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    // If not connected, we can't fetch.
    if (!clientManager_ || !clientManager_->isConnected()) {
        // If replacing, we might want to clear the model to show "offline/empty"
        // or keep stale data. Let's keep stale data but maybe emit error?
        // For now, just log and return.
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh currency model: disconnected.";
        return;
    }

    // If replacing, start from offset 0; otherwise append to existing data
    const std::uint32_t offset = replace ? 0 : static_cast<std::uint32_t>(currencies_.size());

    if (replace) {
        // Clear existing data when replacing
        if (!currencies_.empty()) {
            beginResetModel();
            currencies_.clear();
            recent_iso_codes_.clear();
            pulse_timer_->stop();
            pulse_count_ = 0;
            pulse_state_ = false;
            total_available_count_ = 0;
            endResetModel();
        }
    }

    is_fetching_ = true;
    QPointer<ClientCurrencyModel> self = this;
    const std::uint32_t page_size = page_size_;

    QFuture<FutureWatcherResult> future =
        QtConcurrent::run([self, offset, page_size]() -> FutureWatcherResult {
            BOOST_LOG_SEV(lg(), debug) << "Making a currencies request with offset="
                                       << offset << ", limit=" << page_size;
            if (!self) return {false, {}, 0};

            risk::messaging::get_currencies_request request;
            request.offset = offset;
            request.limit = page_size;
            auto payload = request.serialize();

            frame request_frame(message_type::get_currencies_request,
                0, std::move(payload));

            // Send request synchronously (on background thread)
            // ClientManager handles connection check, but we checked before spawning too.
            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send request: "
                                           << response_result.error();
                return {false, {}, 0};
            }

            // Log frame attributes for debugging
            const auto& header = response_result->header();
            BOOST_LOG_SEV(lg(), debug) << "Currencies response frame: type="
                                       << static_cast<int>(header.type)
                                       << ", compression=" << header.compression
                                       << ", payload_size=" << response_result->payload().size();

            // Decompress payload
            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress currencies response"
                                           << ", compression=" << header.compression
                                           << ", error=" << payload_result.error();
                return {false, {}, 0};
            }

            BOOST_LOG_SEV(lg(), debug) << "Received a currencies response.";
            auto response =
                risk::messaging::get_currencies_response::deserialize(*payload_result);

            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize currencies response"
                                           << ", decompressed_payload_size="
                                           << payload_result->size();
                return {false, {}, 0};
            }

            BOOST_LOG_SEV(lg(), debug) << "Received " << response->currencies.size()
                                       << " currencies, total available: "
                                       << response->total_available_count;

            return {true, std::move(response->currencies),
                    response->total_available_count};
        });

     watcher_->setFuture(future);
}

void ClientCurrencyModel::onCurrenciesLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "On currencies loaded event.";
    is_fetching_ = false;

    auto result = watcher_->result();
    if (result.success) {
        total_available_count_ = result.total_available_count;

        // Build set of existing ISO codes for duplicate detection
        std::unordered_set<std::string> existing_codes;
        for (const auto& curr : currencies_) {
            existing_codes.insert(curr.iso_code);
        }

        // Filter out duplicates from new results
        std::vector<risk::domain::currency> new_currencies;
        for (auto& curr : result.currencies) {
            if (existing_codes.find(curr.iso_code) == existing_codes.end()) {
                new_currencies.push_back(std::move(curr));
                existing_codes.insert(curr.iso_code);
            } else {
                BOOST_LOG_SEV(lg(), trace) << "Skipping duplicate currency: "
                                           << curr.iso_code;
            }
        }

        const int old_size = static_cast<int>(currencies_.size());
        const int new_count = static_cast<int>(new_currencies.size());

        if (new_count > 0) {
            // Append new data to existing currencies
            beginInsertRows(QModelIndex(), old_size, old_size + new_count - 1);
            currencies_.insert(currencies_.end(),
                std::make_move_iterator(new_currencies.begin()),
                std::make_move_iterator(new_currencies.end()));
            endInsertRows();

            // Update the set of recent currencies for recency coloring
            update_recent_currencies();

            // Start the pulse timer if there are recent currencies to highlight
            if (!recent_iso_codes_.empty() && !pulse_timer_->isActive()) {
                pulse_count_ = 0;
                pulse_state_ = true;  // Start with highlight on
                pulse_timer_->start(pulse_interval_ms_);
            }
        }

        BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " new currencies "
                                  << "(received " << result.currencies.size()
                                  << ", filtered " << (result.currencies.size() - new_count)
                                  << " duplicates). Total in model: " << currencies_.size()
                                  << ", Total available: " << total_available_count_;

        emit dataLoaded();
    } else {
        BOOST_LOG_SEV(lg(), error) << "Currencies request failed: no response.";
        emit loadError(tr("Failed to load currencies from server"));
    }
}

const risk::domain::currency* ClientCurrencyModel::getCurrency(int row) const {
    if (row < 0 || row >= static_cast<int>(currencies_.size()))
        return nullptr;

    return &currencies_[row];
}

std::vector<risk::domain::currency> ClientCurrencyModel::getCurrencies() const {
    return currencies_;
}

bool ClientCurrencyModel::canFetchMore(const QModelIndex& parent) const {
    if (parent.isValid())
        return false;

    // NOTE: Automatic fetch-more is disabled because sqlgen doesn't support
    // OFFSET yet (see repository implementation). Without OFFSET, subsequent
    // fetches return the same first N records, causing duplicates.
    // Users can use "Load All" button to load all records in one request.

    // Only allow fetching if we have loaded fewer records than requested page size
    // This handles the initial load, but prevents auto-pagination
    const bool has_more = currencies_.size() < page_size_ &&
                          currencies_.size() < total_available_count_;

    BOOST_LOG_SEV(lg(), trace) << "canFetchMore: " << has_more
                               << " (loaded: " << currencies_.size()
                               << ", page_size: " << page_size_
                               << ", available: " << total_available_count_ << ")";
    return has_more && !is_fetching_;
}

void ClientCurrencyModel::fetchMore(const QModelIndex& parent) {
    if (parent.isValid() || is_fetching_)
        return;

    BOOST_LOG_SEV(lg(), debug) << "fetchMore called, loading next page.";
    refresh(false); // false = append, don't replace
}

void ClientCurrencyModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

void ClientCurrencyModel::update_recent_currencies() {
    recent_iso_codes_.clear();

    const QDateTime now = QDateTime::currentDateTime();

    // First load: set baseline timestamp, no highlighting
    if (!last_reload_time_.isValid()) {
        last_reload_time_ = now;
        BOOST_LOG_SEV(lg(), debug) << "First load - setting baseline timestamp: "
                                   << last_reload_time_.toString(Qt::ISODate).toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Checking currencies newer than last reload: "
                               << last_reload_time_.toString(Qt::ISODate).toStdString();

    // Find currencies with recorded_at newer than last reload
    for (const auto& currency : currencies_) {
        if (currency.recorded_at.empty()) {
            continue;
        }

        // Parse recorded_at as datetime (try multiple formats)
        QDateTime recordedAt = QDateTime::fromString(
            QString::fromStdString(currency.recorded_at), Qt::ISODate);

        if (!recordedAt.isValid()) {
            // Try alternative format (YYYY-MM-DD HH:MM:SS)
            recordedAt = QDateTime::fromString(
                QString::fromStdString(currency.recorded_at), "yyyy-MM-dd HH:mm:ss");
        }

        if (!recordedAt.isValid()) {
            // Try date-only format (YYYY-MM-DD) - assume start of day
            recordedAt = QDateTime::fromString(
                QString::fromStdString(currency.recorded_at), "yyyy-MM-dd");
        }

        if (recordedAt.isValid() && recordedAt > last_reload_time_) {
            recent_iso_codes_.insert(currency.iso_code);
            BOOST_LOG_SEV(lg(), trace) << "Currency " << currency.iso_code
                                       << " is recent (recorded_at: "
                                       << currency.recorded_at << ")";
        }
    }

    // Update the reload timestamp for next comparison
    last_reload_time_ = now;

    BOOST_LOG_SEV(lg(), debug) << "Found " << recent_iso_codes_.size()
                               << " currencies newer than last reload";
}

QVariant ClientCurrencyModel::
recency_foreground_color(const std::string& iso_code) const {
    if (recent_iso_codes_.empty() || recent_iso_codes_.find(iso_code) == recent_iso_codes_.end()) {
        return {};
    }

    // Only show color when pulse_state_ is true (pulsing on)
    if (!pulse_state_) {
        return {};
    }

    // Use the stale indicator color (same as reload button)
    return color_constants::stale_indicator;
}

void ClientCurrencyModel::onPulseTimerTimeout() {
    pulse_state_ = !pulse_state_;
    pulse_count_++;

    // Stop pulsing after max cycles but keep yellow color on
    if (pulse_count_ >= max_pulse_cycles_) {
        pulse_timer_->stop();
        pulse_state_ = true;  // Keep highlight on after pulsing stops
        BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete, staying highlighted";
    }

    // Emit dataChanged to update the view with new colors
    if (!currencies_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

}
