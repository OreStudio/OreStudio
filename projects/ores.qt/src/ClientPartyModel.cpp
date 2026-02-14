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
#include "ores.qt/ClientPartyModel.hpp"

#include <QtConcurrent>
#include "ores.refdata/messaging/party_protocol.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.qt/TextUtils.hpp"
#include "ores.comms/net/client_session.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    std::string party_key_extractor(const refdata::domain::party& e) {
        return e.short_code;
    }
}

ClientPartyModel::ClientPartyModel(
    ClientManager* clientManager, ImageCache* imageCache, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      imageCache_(imageCache),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(party_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientPartyModel::onPartysLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientPartyModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientPartyModel::onPulsingComplete);

    if (imageCache_) {
        connect(imageCache_, &ImageCache::imageLoaded,
                this, [this](const QString&) {
            if (!parties_.empty()) {
                emit dataChanged(index(0, Flag),
                    index(rowCount() - 1, Flag), {Qt::DecorationRole});
            }
        });
    }

    fetch_business_centres();
}

int ClientPartyModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(parties_.size());
}

int ClientPartyModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientPartyModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= parties_.size())
        return {};

    const auto& party = parties_[row];

    if (role == Qt::DecorationRole && index.column() == Flag) {
        if (imageCache_) {
            auto it = bc_code_to_image_id_.find(party.business_center_code);
            if (it != bc_code_to_image_id_.end() && !it->second.empty()) {
                return imageCache_->getIcon(it->second);
            }
            return imageCache_->getNoFlagIcon();
        }
        return {};
    }

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Flag:
            return {};
        case ShortCode:
            return QString::fromStdString(party.short_code);
        case FullName:
            return TextUtils::display_name_with_transliteration(
                party.full_name, party.transliterated_name);
        case TransliteratedName:
            return QString::fromStdString(party.transliterated_name.value_or(""));
        case PartyCategory:
            return QString::fromStdString(party.party_category);
        case PartyType:
            return QString::fromStdString(party.party_type);
        case Status:
            return QString::fromStdString(party.status);
        case BusinessCenterCode:
            return QString::fromStdString(party.business_center_code);
        case Version:
            return party.version;
        case ModifiedBy:
            return QString::fromStdString(party.modified_by);
        case RecordedAt:
            return relative_time_helper::format(party.recorded_at);
        default:
            return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(party.short_code);
    }

    return {};
}

QVariant ClientPartyModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case Flag:
        return tr("Flag");
    case ShortCode:
        return tr("Code");
    case FullName:
        return tr("Name");
    case TransliteratedName:
        return tr("Transliterated Name");
    case PartyCategory:
        return tr("Category");
    case PartyType:
        return tr("Type");
    case Status:
        return tr("Status");
    case BusinessCenterCode:
        return tr("Centre");
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

void ClientPartyModel::refresh(bool /*replace*/) {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh party model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!parties_.empty()) {
        beginResetModel();
        parties_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_parties(0, page_size_);
}

void ClientPartyModel::load_page(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!parties_.empty()) {
        beginResetModel();
        parties_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_parties(offset, limit);
}

void ClientPartyModel::fetch_parties(std::uint32_t offset,
                                      std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientPartyModel> self = this;

    QFuture<FetchResult> future =
        QtConcurrent::run([self, offset, limit]() -> FetchResult {
            return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug) << "Making a parties request with offset="
                                           << offset << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false, .parties = {}, .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                refdata::messaging::get_parties_request request;
                request.offset = offset;
                request.limit = limit;

                auto result = self->clientManager_->
                    process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to fetch parties: "
                                               << comms::net::to_string(result.error());
                    return {.success = false, .parties = {}, .total_available_count = 0,
                            .error_message = QString::fromStdString(
                                "Failed to fetch parties: " + comms::net::to_string(result.error())),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug) << "Received " << result->parties.size()
                                           << " parties, total available: "
                                           << result->total_available_count;

                return {.success = true, .parties = std::move(result->parties),
                        .total_available_count = result->total_available_count,
                        .error_message = {}, .error_details = {}};
            }, "parties");
        });

    watcher_->setFuture(future);
}

void ClientPartyModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

void ClientPartyModel::onPartysLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "On parties loaded event.";
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch parties: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.parties.size());

    if (new_count > 0) {
        beginResetModel();
        parties_ = std::move(result.parties);
        endResetModel();

        const bool has_recent = recencyTracker_.update(parties_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " parties newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " parties."
                              << " Total available: " << total_available_count_;
    emit dataLoaded();
}

const refdata::domain::party*
ClientPartyModel::getParty(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= parties_.size())
        return nullptr;
    return &parties_[idx];
}

QVariant ClientPartyModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientPartyModel::onPulseStateChanged(bool /*isOn*/) {
    if (!parties_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientPartyModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

void ClientPartyModel::fetch_business_centres() {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    using MapType = std::unordered_map<std::string, std::string>;
    QPointer<ClientPartyModel> self = this;

    auto* watcher = new QFutureWatcher<MapType>(this);
    connect(watcher, &QFutureWatcher<MapType>::finished,
            this, [self, watcher]() {
        auto mapping = watcher->result();
        watcher->deleteLater();
        if (!self || mapping.empty())
            return;

        self->bc_code_to_image_id_ = std::move(mapping);

        if (!self->parties_.empty()) {
            emit self->dataChanged(self->index(0, Flag),
                self->index(self->rowCount() - 1, Flag),
                {Qt::DecorationRole});
        }
    });

    watcher->setFuture(QtConcurrent::run(
        [cm = clientManager_]() { return fetch_business_centre_image_map(cm); }));
}

}
