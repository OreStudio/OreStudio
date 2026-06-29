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
#include "ores.qt/ClientGmmComponentModel.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

ClientGmmComponentModel::ClientGmmComponentModel(ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent)
    , clientManager_(clientManager)
    , watcher_(new QFutureWatcher<FetchResult>(this)) {

    connect(watcher_,
            &QFutureWatcher<FetchResult>::finished,
            this,
            &ClientGmmComponentModel::onComponentsLoaded);
}

int ClientGmmComponentModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(components_.size());
}

int ClientGmmComponentModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientGmmComponentModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= components_.size())
        return {};

    const auto& component = components_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
            case ComponentIndex:
                return component.component_index;
            case Mean:
                return component.mean;
            case Stdev:
                return component.stdev;
            case Weight:
                return component.weight;
            case Version:
                return static_cast<qlonglong>(component.version);
            case RecordedAt:
                return relative_time_helper::format(component.recorded_at);
            default:
                return {};
        }
    }

    return {};
}

QVariant
ClientGmmComponentModel::headerData(int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
        case ComponentIndex:
            return tr("Component Index");
        case Mean:
            return tr("Mean");
        case Stdev:
            return tr("Stdev");
        case Weight:
            return tr("Weight");
        case Version:
            return tr("Version");
        case RecordedAt:
            return tr("Recorded At");
        default:
            return {};
    }
}

void ClientGmmComponentModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh GMM component model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!components_.empty()) {
        beginResetModel();
        components_.clear();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_components(0, page_size_);
}

void ClientGmmComponentModel::load_page(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!components_.empty()) {
        beginResetModel();
        components_.clear();
        endResetModel();
    }

    fetch_components(offset, limit);
}

void ClientGmmComponentModel::fetch_components(std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientGmmComponentModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self, offset, limit]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>(
            [&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug) << "Making GMM components request with offset=" << offset
                                           << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false,
                            .components = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                synthetic::messaging::get_gmm_components_request request{
                    .offset = offset, .limit = limit};

                auto result =
                    self->clientManager_->process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to send request: " << result.error();
                    return {.success = false,
                            .components = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(result.error()),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug)
                    << "Fetched " << result->gmm_components.size() << " GMM components";
                return {.success = true,
                        .components = std::move(result->gmm_components),
                        .total_available_count =
                            static_cast<std::uint32_t>(result->total_available_count),
                        .error_message = {},
                        .error_details = {}};
            },
            "GMM components");
    });

    watcher_->setFuture(future);
}

void ClientGmmComponentModel::onComponentsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to fetch GMM components: " << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.components.size());

    if (new_count > 0) {
        beginResetModel();
        components_ = std::move(result.components);
        endResetModel();
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " GMM components."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientGmmComponentModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const synthetic::domain::gmm_component* ClientGmmComponentModel::getComponent(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= components_.size())
        return nullptr;
    return &components_[idx];
}

}
