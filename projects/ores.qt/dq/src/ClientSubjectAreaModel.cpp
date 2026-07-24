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
#include "ores.qt/ClientSubjectAreaModel.hpp"
#include ""
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

namespace {
std::string subject_area_key_extractor(const& e) {
    return e.;
}
}

ClientSubjectAreaModel::ClientSubjectAreaModel(ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent)
    , clientManager_(clientManager)
    , watcher_(new QFutureWatcher<FetchResult>(this))
    , recencyTracker_(subject_area_key_extractor)
    , pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_,
            &QFutureWatcher<FetchResult>::finished,
            this,
            &ClientSubjectAreaModel::onAreasLoaded);

    connect(pulseManager_,
            &RecencyPulseManager::pulse_state_changed,
            this,
            &ClientSubjectAreaModel::onPulseStateChanged);
    connect(pulseManager_,
            &RecencyPulseManager::pulsing_complete,
            this,
            &ClientSubjectAreaModel::onPulsingComplete);
}

int ClientSubjectAreaModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(_.size());
}

int ClientSubjectAreaModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientSubjectAreaModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= _.size())
        return {};

    const auto& = _[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
            default:
                return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(.);
    }

    return {};
}

QVariant
ClientSubjectAreaModel::headerData(int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
        default:
            return {};
    }
}

void ClientSubjectAreaModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh subject area model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!_.empty()) {
        beginResetModel();
        _.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_(0, page_size_);
}

void ClientSubjectAreaModel::load_page(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!_.empty()) {
        beginResetModel();
        _.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_(offset, limit);
}

void ClientSubjectAreaModel::fetch_(std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientSubjectAreaModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self, offset, limit]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>(
            [&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug)
                    << "Making subject areas request with offset=" << offset << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false,
                            .= {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                request;

                auto result =
                    self->clientManager_->process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to send request: " << result.error();
                    return {.success = false,
                            .= {},
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
                            .= {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(result->message),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug)
                    << "Fetched " << result->areas.size() << " subject areas";
                const std::uint32_t count = static_cast<std::uint32_t>(result->areas.size());
                return {.success = true,
                        .= std::move(result->areas),
                        .total_available_count = count,
                        .error_message = {},
                        .error_details = {}};
            },
            "subject areas");
    });

    watcher_->setFuture(future);
}

void ClientSubjectAreaModel::onAreasLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to fetch subject areas: " << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result..size());

    if (new_count > 0) {
        beginResetModel();
        _ = std::move(result.);
        endResetModel();

        const bool has_recent = recencyTracker_.update(_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " subject areas newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " subject areas."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientSubjectAreaModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const* ClientSubjectAreaModel::getArea(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= _.size())
        return nullptr;
    return &_[idx];
}


QVariant ClientSubjectAreaModel::recency_foreground_color(const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientSubjectAreaModel::onPulseStateChanged(bool /*isOn*/) {
    if (!_.empty()) {
        emit dataChanged(
            index(0, 0), index(rowCount() - 1, columnCount() - 1), {Qt::ForegroundRole});
    }
}

void ClientSubjectAreaModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
