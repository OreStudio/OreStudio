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
#include "ores.qt/ClientBondInstrumentModel.hpp"

#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include "ores.trading.api/messaging/instrument_protocol.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    std::string bond_instrument_key_extractor(
        const trading::domain::bond_instrument& t) {
        return boost::uuids::to_string(t.id);
    }
}

ClientBondInstrumentModel::ClientBondInstrumentModel(
    ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(bond_instrument_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientBondInstrumentModel::onBondInstrumentsLoaded);
    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientBondInstrumentModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientBondInstrumentModel::onPulsingComplete);
}

int ClientBondInstrumentModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return static_cast<int>(instruments_.size());
}

int ClientBondInstrumentModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return ColumnCount;
}

QVariant ClientBondInstrumentModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid()) return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= instruments_.size()) return {};

    const auto& inst = instruments_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Id: {
            const auto full = boost::uuids::to_string(inst.id);
            return QString::fromStdString(full.substr(0, 8));
        }
        case TradeType:
            return QString::fromStdString(inst.trade_type_code);
        case Issuer:
            return QString::fromStdString(inst.issuer);
        case Currency:
            return QString::fromStdString(inst.currency);
        case FaceValue:
            return QString::number(inst.face_value, 'f', 2);
        case CouponRate:
            return QString::number(inst.coupon_rate * 100.0, 'f', 4) + "%";
        case MaturityDate:
            return QString::fromStdString(inst.maturity_date);
        case Version:
            return static_cast<qlonglong>(inst.version);
        case ModifiedBy:
            return QString::fromStdString(inst.modified_by);
        case RecordedAt:
            return relative_time_helper::format(inst.recorded_at);
        default:
            return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(boost::uuids::to_string(inst.id));
    }

    return {};
}

QVariant ClientBondInstrumentModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole) return {};

    switch (section) {
    case Id:           return tr("ID");
    case TradeType:    return tr("Type");
    case Issuer:       return tr("Issuer");
    case Currency:     return tr("Currency");
    case FaceValue:    return tr("Face Value");
    case CouponRate:   return tr("Coupon Rate");
    case MaturityDate: return tr("Maturity");
    case Version:      return tr("Version");
    case ModifiedBy:   return tr("Modified By");
    case RecordedAt:   return tr("Recorded At");
    default:           return {};
    }
}

void ClientBondInstrumentModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, skipping.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!instruments_.empty()) {
        beginResetModel();
        instruments_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_bond_instruments(0, page_size_);
}

void ClientBondInstrumentModel::load_page(std::uint32_t offset,
                                          std::uint32_t limit) {
    if (is_fetching_) return;
    if (!clientManager_ || !clientManager_->isConnected()) return;

    if (!instruments_.empty()) {
        beginResetModel();
        instruments_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_bond_instruments(offset, limit);
}

void ClientBondInstrumentModel::fetch_bond_instruments(
    std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientBondInstrumentModel> self = this;

    QFuture<FetchResult> future =
        QtConcurrent::run([self, offset, limit]() -> FetchResult {
            return exception_helper::wrap_async_fetch<FetchResult>(
                [&]() -> FetchResult {
                    if (!self || !self->clientManager_) {
                        return {.success = false, .instruments = {},
                                .total_available_count = 0,
                                .error_message = "Model was destroyed",
                                .error_details = {}};
                    }

                    trading::messaging::get_bond_instruments_request request;
                    request.offset = static_cast<int>(offset);
                    request.limit = static_cast<int>(limit);

                    auto result = self->clientManager_->
                        process_authenticated_request(std::move(request));

                    if (!result) {
                        return {.success = false, .instruments = {},
                                .total_available_count = 0,
                                .error_message = QString::fromStdString(
                                    "Failed to fetch bond instruments: " +
                                    result.error()),
                                .error_details = {}};
                    }

                    auto count = static_cast<std::uint32_t>(
                        result->total_available_count);
                    return {.success = true,
                            .instruments = std::move(result->instruments),
                            .total_available_count = count,
                            .error_message = {}, .error_details = {}};
                }, "bond_instruments");
        });

    watcher_->setFuture(future);
}

void ClientBondInstrumentModel::onBondInstrumentsLoaded() {
    is_fetching_ = false;
    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch bond instruments: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;
    const int new_count = static_cast<int>(result.instruments.size());

    if (new_count > 0) {
        beginResetModel();
        instruments_ = std::move(result.instruments);
        endResetModel();

        const bool has_recent = recencyTracker_.update(instruments_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " bond instruments."
                              << " Total: " << total_available_count_;
    emit dataLoaded();
}

void ClientBondInstrumentModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        page_size_ = 100;
    } else {
        page_size_ = size;
    }
}

const trading::domain::bond_instrument*
ClientBondInstrumentModel::getBondInstrument(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= instruments_.size()) return nullptr;
    return &instruments_[idx];
}

QVariant ClientBondInstrumentModel::recency_foreground_color(
    const std::string& key) const {
    if (recencyTracker_.is_recent(key) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientBondInstrumentModel::onPulseStateChanged(bool /*isOn*/) {
    if (!instruments_.empty()) {
        emit dataChanged(index(0, 0),
            index(rowCount() - 1, columnCount() - 1), {Qt::ForegroundRole});
    }
}

void ClientBondInstrumentModel::onPulsingComplete() {
    recencyTracker_.clear();
}

}
