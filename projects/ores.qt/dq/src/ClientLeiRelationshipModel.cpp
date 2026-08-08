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
#include "ores.qt/ClientLeiRelationshipModel.hpp"
#include "ores.dq.api/messaging/lei_relationship_protocol.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

namespace {
std::string lei_relationship_key_extractor(const dq::domain::lei_relationship& e) {
    return e.relationship_start_node_node_id;
}
}

ClientLeiRelationshipModel::ClientLeiRelationshipModel(ClientManager* clientManager,
                                                       QObject* parent)
    : AbstractClientModel(parent)
    , clientManager_(clientManager)
    , watcher_(new QFutureWatcher<FetchResult>(this))
    , recencyTracker_(lei_relationship_key_extractor)
    , pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_,
            &QFutureWatcher<FetchResult>::finished,
            this,
            &ClientLeiRelationshipModel::onRelationshipsLoaded);

    connect(pulseManager_,
            &RecencyPulseManager::pulse_state_changed,
            this,
            &ClientLeiRelationshipModel::onPulseStateChanged);
    connect(pulseManager_,
            &RecencyPulseManager::pulsing_complete,
            this,
            &ClientLeiRelationshipModel::onPulsingComplete);
}

int ClientLeiRelationshipModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(relationships_.size());
}

int ClientLeiRelationshipModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientLeiRelationshipModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= relationships_.size())
        return {};

    const auto& relationship = relationships_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
            case StartNode:
                return QString::fromStdString(relationship.relationship_start_node_node_id);
            case EndNode:
                return QString::fromStdString(relationship.relationship_end_node_node_id);
            case RelType:
                return QString::fromStdString(relationship.relationship_relationship_type);
            default:
                return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(relationship.relationship_start_node_node_id);
    }

    return {};
}

QVariant
ClientLeiRelationshipModel::headerData(int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || (role != Qt::DisplayRole && role != Qt::ToolTipRole))
        return {};

    if (role == Qt::ToolTipRole) {
        switch (section) {
            default:
                return {};
        }
    }

    switch (section) {
        case StartNode:
            return tr("Start Node");
        case EndNode:
            return tr("End Node");
        case RelType:
            return tr("Type");
        default:
            return {};
    }
}

void ClientLeiRelationshipModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh LEI relationship model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!relationships_.empty()) {
        beginResetModel();
        relationships_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_relationships(0, page_size_);
}

void ClientLeiRelationshipModel::load_page(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!relationships_.empty()) {
        beginResetModel();
        relationships_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_relationships(offset, limit);
}

void ClientLeiRelationshipModel::fetch_relationships(std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientLeiRelationshipModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self, offset, limit]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>(
            [&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug)
                    << "Making LEI relationships request with offset=" << offset
                    << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false,
                            .relationships = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                dq::messaging::get_lei_relationships_request request;
                request.offset = offset;
                request.limit = limit;

                auto result =
                    self->clientManager_->process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to send request: " << result.error();
                    return {.success = false,
                            .relationships = {},
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
                            .relationships = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(result->message),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug)
                    << "Fetched " << result->relationships.size()
                    << " LEI relationships, total available: " << result->total_available_count;
                return {.success = true,
                        .relationships = std::move(result->relationships),
                        .total_available_count =
                            static_cast<std::uint32_t>(result->total_available_count),
                        .error_message = {},
                        .error_details = {}};
            },
            "LEI relationships");
    });

    watcher_->setFuture(future);
}

void ClientLeiRelationshipModel::onRelationshipsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to fetch LEI relationships: " << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.relationships.size());

    if (new_count > 0) {
        beginResetModel();
        relationships_ = std::move(result.relationships);
        endResetModel();

        const bool has_recent = recencyTracker_.update(relationships_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " LEI relationships newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " LEI relationships."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientLeiRelationshipModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const dq::domain::lei_relationship* ClientLeiRelationshipModel::getRelationship(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= relationships_.size())
        return nullptr;
    return &relationships_[idx];
}


QVariant ClientLeiRelationshipModel::recency_foreground_color(const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientLeiRelationshipModel::onPulseStateChanged(bool /*isOn*/) {
    if (!relationships_.empty()) {
        emit dataChanged(
            index(0, 0), index(rowCount() - 1, columnCount() - 1), {Qt::ForegroundRole});
    }
}

void ClientLeiRelationshipModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
