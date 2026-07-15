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
#include "ores.qt/ClientIrCurveTemplateEntryModel.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.synthetic.api/messaging/ir_curve_template_entry_protocol.hpp"
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

namespace {
std::string
ir_curve_template_entry_key_extractor(const synthetic::domain::ir_curve_template_entry& e) {
    return boost::uuids::to_string(e.id);
}
}

ClientIrCurveTemplateEntryModel::ClientIrCurveTemplateEntryModel(ClientManager* clientManager,
                                                                 QObject* parent)
    : AbstractClientModel(parent)
    , clientManager_(clientManager)
    , watcher_(new QFutureWatcher<FetchResult>(this))
    , recencyTracker_(ir_curve_template_entry_key_extractor)
    , pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_,
            &QFutureWatcher<FetchResult>::finished,
            this,
            &ClientIrCurveTemplateEntryModel::onEntriesLoaded);

    connect(pulseManager_,
            &RecencyPulseManager::pulse_state_changed,
            this,
            &ClientIrCurveTemplateEntryModel::onPulseStateChanged);
    connect(pulseManager_,
            &RecencyPulseManager::pulsing_complete,
            this,
            &ClientIrCurveTemplateEntryModel::onPulsingComplete);
}

int ClientIrCurveTemplateEntryModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(ir_curve_template_entries_.size());
}

int ClientIrCurveTemplateEntryModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientIrCurveTemplateEntryModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= ir_curve_template_entries_.size())
        return {};

    const auto& ir_curve_template_entry = ir_curve_template_entries_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
            case SequenceIndex:
                return static_cast<qlonglong>(ir_curve_template_entry.sequence_index);
            case StartTenorCode:
                return QString::fromStdString(ir_curve_template_entry.start_tenor_code);
            case EndTenorCode:
                return QString::fromStdString(ir_curve_template_entry.end_tenor_code);
            case InstrumentCode:
                return QString::fromStdString(ir_curve_template_entry.instrument_code);
            case Version:
                return static_cast<qlonglong>(ir_curve_template_entry.version);
            case ModifiedBy:
                return QString::fromStdString(ir_curve_template_entry.modified_by);
            case RecordedAt:
                return relative_time_helper::format(ir_curve_template_entry.recorded_at);
            default:
                return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(boost::uuids::to_string(ir_curve_template_entry.id));
    }

    return {};
}

QVariant ClientIrCurveTemplateEntryModel::headerData(int section,
                                                     Qt::Orientation orientation,
                                                     int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
        case SequenceIndex:
            return tr("Sequence Index");
        case StartTenorCode:
            return tr("Start Tenor");
        case EndTenorCode:
            return tr("End Tenor");
        case InstrumentCode:
            return tr("Instrument");
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

void ClientIrCurveTemplateEntryModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh IR curve template entry model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!ir_curve_template_entries_.empty()) {
        beginResetModel();
        ir_curve_template_entries_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_ir_curve_template_entries(0, page_size_);
}

void ClientIrCurveTemplateEntryModel::load_page(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!ir_curve_template_entries_.empty()) {
        beginResetModel();
        ir_curve_template_entries_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_ir_curve_template_entries(offset, limit);
}

void ClientIrCurveTemplateEntryModel::fetch_ir_curve_template_entries(std::uint32_t offset,
                                                                      std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientIrCurveTemplateEntryModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self, offset, limit]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>(
            [&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug)
                    << "Making IR curve template entries request with offset=" << offset
                    << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false,
                            .ir_curve_template_entries = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                synthetic::messaging::get_ir_curve_template_entries_request request;
                request.offset = offset;
                request.limit = limit;

                auto result =
                    self->clientManager_->process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to send request: " << result.error();
                    return {.success = false,
                            .ir_curve_template_entries = {},
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
                            .ir_curve_template_entries = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(result->message),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug) << "Fetched " << result->ir_curve_template_entries.size()
                                           << " IR curve template entries, total available: "
                                           << result->total_available_count;
                return {.success = true,
                        .ir_curve_template_entries = std::move(result->ir_curve_template_entries),
                        .total_available_count =
                            static_cast<std::uint32_t>(result->total_available_count),
                        .error_message = {},
                        .error_details = {}};
            },
            "IR curve template entries");
    });

    watcher_->setFuture(future);
}

void ClientIrCurveTemplateEntryModel::onEntriesLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to fetch IR curve template entries: " << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.ir_curve_template_entries.size());

    if (new_count > 0) {
        beginResetModel();
        ir_curve_template_entries_ = std::move(result.ir_curve_template_entries);
        endResetModel();

        const bool has_recent = recencyTracker_.update(ir_curve_template_entries_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " IR curve template entries newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " IR curve template entries."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientIrCurveTemplateEntryModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const synthetic::domain::ir_curve_template_entry*
ClientIrCurveTemplateEntryModel::getEntry(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= ir_curve_template_entries_.size())
        return nullptr;
    return &ir_curve_template_entries_[idx];
}


QVariant ClientIrCurveTemplateEntryModel::recency_foreground_color(const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientIrCurveTemplateEntryModel::onPulseStateChanged(bool /*isOn*/) {
    if (!ir_curve_template_entries_.empty()) {
        emit dataChanged(
            index(0, 0), index(rowCount() - 1, columnCount() - 1), {Qt::ForegroundRole});
    }
}

void ClientIrCurveTemplateEntryModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
