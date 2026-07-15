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
#include "ores.qt/ClientCurrencyPairModel.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/protocol.hpp"
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

namespace {
std::string currency_pair_key_extractor(const refdata::domain::currency_pair& e) {
    return e.pair_code;
}
}

ClientCurrencyPairModel::ClientCurrencyPairModel(ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent)
    , clientManager_(clientManager)
    , watcher_(new QFutureWatcher<FetchResult>(this))
    , recencyTracker_(currency_pair_key_extractor)
    , pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_,
            &QFutureWatcher<FetchResult>::finished,
            this,
            &ClientCurrencyPairModel::onPairsLoaded);

    connect(pulseManager_,
            &RecencyPulseManager::pulse_state_changed,
            this,
            &ClientCurrencyPairModel::onPulseStateChanged);
    connect(pulseManager_,
            &RecencyPulseManager::pulsing_complete,
            this,
            &ClientCurrencyPairModel::onPulsingComplete);
}

int ClientCurrencyPairModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(pairs_.size());
}

int ClientCurrencyPairModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientCurrencyPairModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= pairs_.size())
        return {};

    const auto& pair = pairs_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
            case PairCode:
                return QString::fromStdString(pair.pair_code);
            case BaseCurrency:
                return QString::fromStdString(pair.base_currency);
            case QuoteCurrency:
                return QString::fromStdString(pair.quote_currency);
            case Classification:
                return QString::fromStdString(pair.classification);
            case Version:
                return static_cast<qlonglong>(pair.version);
            case ModifiedBy:
                return QString::fromStdString(pair.modified_by);
            case RecordedAt:
                return relative_time_helper::format(pair.recorded_at);
            default:
                return {};
        }
    }

    if (role == Qt::DecorationRole && imageCache_) {
        if (index.column() == Column::PairCode)
            return currency_flag_icon(*imageCache_, pair.base_currency, pair.quote_currency);
        if (index.column() == Column::BaseCurrency)
            return currency_flag_icon(*imageCache_, pair.base_currency);
        if (index.column() == Column::QuoteCurrency)
            return currency_flag_icon(*imageCache_, pair.quote_currency);
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(pair.pair_code);
    }

    return {};
}

QVariant
ClientCurrencyPairModel::headerData(int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
        case PairCode:
            return tr("Pair");
        case BaseCurrency:
            return tr("Base");
        case QuoteCurrency:
            return tr("Quote");
        case Classification:
            return tr("Classification");
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

void ClientCurrencyPairModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh currency pair model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!pairs_.empty()) {
        beginResetModel();
        pairs_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_pairs(0, page_size_);
}

void ClientCurrencyPairModel::load_page(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!pairs_.empty()) {
        beginResetModel();
        pairs_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_pairs(offset, limit);
}

void ClientCurrencyPairModel::fetch_pairs(std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientCurrencyPairModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self, offset, limit]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>(
            [&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug) << "Making currency pairs request with offset=" << offset
                                           << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false,
                            .pairs = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                refdata::messaging::get_currency_pairs_request request;

                auto result =
                    self->clientManager_->process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to send request: " << result.error();
                    return {.success = false,
                            .pairs = {},
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
                            .pairs = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(result->message),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug)
                    << "Fetched " << result->pairs.size() << " currency pairs";
                const std::uint32_t count = static_cast<std::uint32_t>(result->pairs.size());
                return {.success = true,
                        .pairs = std::move(result->pairs),
                        .total_available_count = count,
                        .error_message = {},
                        .error_details = {}};
            },
            "currency pairs");
    });

    watcher_->setFuture(future);
}

void ClientCurrencyPairModel::onPairsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to fetch currency pairs: " << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.pairs.size());

    if (new_count > 0) {
        beginResetModel();
        pairs_ = std::move(result.pairs);
        endResetModel();

        const bool has_recent = recencyTracker_.update(pairs_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " currency pairs newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " currency pairs."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientCurrencyPairModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const refdata::domain::currency_pair* ClientCurrencyPairModel::getPair(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= pairs_.size())
        return nullptr;
    return &pairs_[idx];
}


QVariant ClientCurrencyPairModel::recency_foreground_color(const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientCurrencyPairModel::onPulseStateChanged(bool /*isOn*/) {
    if (!pairs_.empty()) {
        emit dataChanged(
            index(0, 0), index(rowCount() - 1, columnCount() - 1), {Qt::ForegroundRole});
    }
}

void ClientCurrencyPairModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
