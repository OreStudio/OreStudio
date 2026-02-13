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
#include "ores.qt/ClientCounterpartyModel.hpp"

#include <unordered_set>
#include <QtConcurrent>
#include "ores.refdata/messaging/counterparty_protocol.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;
using ores::comms::messaging::frame;
using ores::comms::messaging::message_type;

namespace {
    std::string counterparty_key_extractor(const refdata::domain::counterparty& e) {
        return e.short_code;
    }
}

ClientCounterpartyModel::ClientCounterpartyModel(
    ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(counterparty_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientCounterpartyModel::onCounterpartysLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientCounterpartyModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientCounterpartyModel::onPulsingComplete);
}

int ClientCounterpartyModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(counterparties_.size());
}

int ClientCounterpartyModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientCounterpartyModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= counterparties_.size())
        return {};

    const auto& counterparty = counterparties_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case ShortCode:
            return QString::fromStdString(counterparty.short_code);
        case FullName:
            return QString::fromStdString(counterparty.full_name);
        case TransliteratedName:
            return QString::fromStdString(counterparty.transliterated_name.value_or(""));
        case PartyType:
            return QString::fromStdString(counterparty.party_type);
        case Status:
            return QString::fromStdString(counterparty.status);
        case BusinessCenterCode:
            return QString::fromStdString(counterparty.business_center_code);
        case Version:
            return counterparty.version;
        case RecordedBy:
            return QString::fromStdString(counterparty.recorded_by);
        case RecordedAt:
            return relative_time_helper::format(counterparty.recorded_at);
        default:
            return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(counterparty.short_code);
    }

    return {};
}

QVariant ClientCounterpartyModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case ShortCode:
        return tr("Code");
    case FullName:
        return tr("Name");
    case TransliteratedName:
        return tr("Transliterated Name");
    case PartyType:
        return tr("Type");
    case Status:
        return tr("Status");
    case BusinessCenterCode:
        return tr("Business Center");
    case Version:
        return tr("Version");
    case RecordedBy:
        return tr("Recorded By");
    case RecordedAt:
        return tr("Recorded At");
    default:
        return {};
    }
}

void ClientCounterpartyModel::refresh(bool replace) {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh (replace=" << replace << ").";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh counterparty model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    const std::uint32_t offset = replace ? 0 : static_cast<std::uint32_t>(counterparties_.size());

    if (replace) {
        if (!counterparties_.empty()) {
            beginResetModel();
            counterparties_.clear();
            recencyTracker_.clear();
            pulseManager_->stop_pulsing();
            total_available_count_ = 0;
            endResetModel();
        }
    }

    fetch_counterparties(offset, page_size_);
}

void ClientCounterpartyModel::load_page(std::uint32_t offset,
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

    if (!counterparties_.empty()) {
        beginResetModel();
        counterparties_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_counterparties(offset, limit);
}

void ClientCounterpartyModel::fetch_counterparties(std::uint32_t offset,
                                                     std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientCounterpartyModel> self = this;

    QFuture<FetchResult> future =
        QtConcurrent::run([self, offset, limit]() -> FetchResult {
            return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug) << "Making counterparties request with offset="
                                           << offset << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false, .counterparties = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                refdata::messaging::get_counterparties_request request;
                request.offset = offset;
                request.limit = limit;
                auto payload = request.serialize();

                frame request_frame(
                    message_type::get_counterparties_request,
                    0, std::move(payload)
                );

                auto response_result = self->clientManager_->sendRequest(
                    std::move(request_frame));
                if (!response_result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to send request";
                    return {.success = false, .counterparties = {},
                            .total_available_count = 0,
                            .error_message = "Failed to send request",
                            .error_details = {}};
                }

                // Check for server error response
                if (auto err = exception_helper::check_error_response(*response_result)) {
                    BOOST_LOG_SEV(lg(), error) << "Server error: "
                                               << err->message.toStdString();
                    return {.success = false, .counterparties = {},
                            .total_available_count = 0,
                            .error_message = err->message,
                            .error_details = err->details};
                }

                auto payload_result = response_result->decompressed_payload();
                if (!payload_result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to decompress response";
                    return {.success = false, .counterparties = {},
                            .total_available_count = 0,
                            .error_message = "Failed to decompress response",
                            .error_details = {}};
                }

                auto response = refdata::messaging::get_counterparties_response::
                    deserialize(*payload_result);
                if (!response) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
                    return {.success = false, .counterparties = {},
                            .total_available_count = 0,
                            .error_message = "Failed to deserialize response",
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug) << "Fetched " << response->counterparties.size()
                                           << " counterparties, total available: "
                                           << response->total_available_count;
                return {.success = true,
                        .counterparties = std::move(response->counterparties),
                        .total_available_count = response->total_available_count,
                        .error_message = {}, .error_details = {}};
            }, "counterparties");
        });

    watcher_->setFuture(future);
}

void ClientCounterpartyModel::onCounterpartysLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch counterparties: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    std::vector<refdata::domain::counterparty> new_counterparties;
    const auto received_count = result.counterparties.size();

    if (counterparties_.empty()) {
        // Full refresh or first page load, no duplicates to check.
        new_counterparties = std::move(result.counterparties);
    } else {
        // Appending data, check for duplicates.
        std::unordered_set<std::string> existing_codes;
        existing_codes.reserve(counterparties_.size());
        for (const auto& cp : counterparties_) {
            existing_codes.insert(cp.short_code);
        }

        new_counterparties.reserve(received_count);
        for (auto& cp : result.counterparties) {
            if (existing_codes.find(cp.short_code) == existing_codes.end()) {
                new_counterparties.push_back(std::move(cp));
                existing_codes.insert(cp.short_code);
            } else {
                BOOST_LOG_SEV(lg(), trace) << "Skipping duplicate counterparty: "
                                           << cp.short_code;
            }
        }
    }

    const int old_size = static_cast<int>(counterparties_.size());
    const int new_count = static_cast<int>(new_counterparties.size());

    if (new_count > 0) {
        beginInsertRows(QModelIndex(), old_size, old_size + new_count - 1);
        counterparties_.insert(counterparties_.end(),
            std::make_move_iterator(new_counterparties.begin()),
            std::make_move_iterator(new_counterparties.end()));
        endInsertRows();

        const bool has_recent = recencyTracker_.update(counterparties_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " counterparties newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " new counterparties "
                              << "(received " << received_count
                              << ", filtered " << (received_count - new_count)
                              << " duplicates). Total in model: " << counterparties_.size()
                              << ", Total available: " << total_available_count_;

    emit dataLoaded();
}

const refdata::domain::counterparty*
ClientCounterpartyModel::getCounterparty(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= counterparties_.size())
        return nullptr;
    return &counterparties_[idx];
}

bool ClientCounterpartyModel::canFetchMore(const QModelIndex& parent) const {
    if (parent.isValid())
        return false;

    const bool has_more = counterparties_.size() < total_available_count_;
    BOOST_LOG_SEV(lg(), trace) << "canFetchMore: " << has_more
                               << " (loaded: " << counterparties_.size()
                               << ", page_size: " << page_size_
                               << ", available: " << total_available_count_ << ")";
    return has_more && !is_fetching_;
}

void ClientCounterpartyModel::fetchMore(const QModelIndex& parent) {
    if (parent.isValid() || is_fetching_)
        return;

    BOOST_LOG_SEV(lg(), debug) << "fetchMore called, loading next page.";
    refresh(false);
}

void ClientCounterpartyModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

QVariant ClientCounterpartyModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientCounterpartyModel::onPulseStateChanged(bool /*isOn*/) {
    if (!counterparties_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientCounterpartyModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
