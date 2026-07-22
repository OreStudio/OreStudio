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
#include "ores.qt/ClientAccountContactInformationModel.hpp"
#include "ores.iam.api/messaging/account_contact_information_protocol.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

namespace {
std::string
account_contact_information_key_extractor(const iam::domain::account_contact_information& e) {
    return e.full_name;
}
}

ClientAccountContactInformationModel::ClientAccountContactInformationModel(
    ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent)
    , clientManager_(clientManager)
    , watcher_(new QFutureWatcher<FetchResult>(this))
    , recencyTracker_(account_contact_information_key_extractor)
    , pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_,
            &QFutureWatcher<FetchResult>::finished,
            this,
            &ClientAccountContactInformationModel::onInformationsLoaded);

    connect(pulseManager_,
            &RecencyPulseManager::pulse_state_changed,
            this,
            &ClientAccountContactInformationModel::onPulseStateChanged);
    connect(pulseManager_,
            &RecencyPulseManager::pulsing_complete,
            this,
            &ClientAccountContactInformationModel::onPulsingComplete);
}

int ClientAccountContactInformationModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(account_contact_informations_.size());
}

int ClientAccountContactInformationModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientAccountContactInformationModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= account_contact_informations_.size())
        return {};

    const auto& accountContactInformation = account_contact_informations_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
            case FullName:
                return QString::fromStdString(accountContactInformation.full_name);
            case StreetLine1:
                return QString::fromStdString(accountContactInformation.street_line_1);
            case City:
                return QString::fromStdString(accountContactInformation.city);
            case CountryCode:
                return QString::fromStdString(accountContactInformation.country_code);
            case Phone:
                return QString::fromStdString(accountContactInformation.phone);
            case Email:
                return QString::fromStdString(accountContactInformation.email);
            case Version:
                return static_cast<qlonglong>(accountContactInformation.version);
            case ModifiedBy:
                return QString::fromStdString(accountContactInformation.modified_by);
            case RecordedAt:
                return relative_time_helper::format(accountContactInformation.recorded_at);
            default:
                return {};
        }
    }

    if (role == Qt::DecorationRole && imageCache_) {
        if (index.column() == Column::CountryCode)
            return country_flag_icon(*imageCache_, accountContactInformation.country_code);
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(accountContactInformation.full_name);
    }

    return {};
}

QVariant ClientAccountContactInformationModel::headerData(int section,
                                                          Qt::Orientation orientation,
                                                          int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
        case FullName:
            return tr("Full Name");
        case StreetLine1:
            return tr("Street");
        case City:
            return tr("City");
        case CountryCode:
            return tr("Country");
        case Phone:
            return tr("Phone");
        case Email:
            return tr("Email");
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

void ClientAccountContactInformationModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn)
            << "Cannot refresh account contact information model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!account_contact_informations_.empty()) {
        beginResetModel();
        account_contact_informations_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_account_contact_informations(0, page_size_);
}

void ClientAccountContactInformationModel::load_page(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!account_contact_informations_.empty()) {
        beginResetModel();
        account_contact_informations_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_account_contact_informations(offset, limit);
}

void ClientAccountContactInformationModel::fetch_account_contact_informations(std::uint32_t offset,
                                                                              std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientAccountContactInformationModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self, offset, limit]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>(
            [&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug)
                    << "Making account contact informations request with offset=" << offset
                    << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false,
                            .account_contact_informations = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                iam::messaging::get_account_contact_informations_request request;
                request.offset = offset;
                request.limit = limit;

                auto result =
                    self->clientManager_->process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to send request: " << result.error();
                    return {.success = false,
                            .account_contact_informations = {},
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
                            .account_contact_informations = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(result->message),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug)
                    << "Fetched " << result->account_contact_informations.size()
                    << " account contact informations, total available: "
                    << result->total_available_count;
                return {.success = true,
                        .account_contact_informations =
                            std::move(result->account_contact_informations),
                        .total_available_count =
                            static_cast<std::uint32_t>(result->total_available_count),
                        .error_message = {},
                        .error_details = {}};
            },
            "account contact informations");
    });

    watcher_->setFuture(future);
}

void ClientAccountContactInformationModel::onInformationsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch account contact informations: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.account_contact_informations.size());

    if (new_count > 0) {
        beginResetModel();
        account_contact_informations_ = std::move(result.account_contact_informations);
        endResetModel();

        const bool has_recent = recencyTracker_.update(account_contact_informations_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " account contact informations newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " account contact informations."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientAccountContactInformationModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const iam::domain::account_contact_information*
ClientAccountContactInformationModel::getInformation(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= account_contact_informations_.size())
        return nullptr;
    return &account_contact_informations_[idx];
}


QVariant
ClientAccountContactInformationModel::recency_foreground_color(const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientAccountContactInformationModel::onPulseStateChanged(bool /*isOn*/) {
    if (!account_contact_informations_.empty()) {
        emit dataChanged(
            index(0, 0), index(rowCount() - 1, columnCount() - 1), {Qt::ForegroundRole});
    }
}

void ClientAccountContactInformationModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
