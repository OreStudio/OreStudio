/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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

#include <QtConcurrent>
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.dq/messaging/data_organization_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

ClientSubjectAreaModel::ClientSubjectAreaModel(
    ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      pulse_timer_(new QTimer(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientSubjectAreaModel::onSubjectAreasLoaded);

    pulse_timer_->setInterval(pulse_interval_ms_);
    connect(pulse_timer_, &QTimer::timeout,
            this, &ClientSubjectAreaModel::onPulseTimerTimeout);
}

int ClientSubjectAreaModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return static_cast<int>(subject_areas_.size());
}

int ClientSubjectAreaModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return ColumnCount;
}

QVariant ClientSubjectAreaModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid() || index.row() >= static_cast<int>(subject_areas_.size()))
        return {};

    const auto& subject_area = subject_areas_[index.row()];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Name: return QString::fromStdString(subject_area.name);
        case DomainName: return QString::fromStdString(subject_area.domain_name);
        case Description: return QString::fromStdString(subject_area.description);
        case Version: return subject_area.version;
        case RecordedBy: return QString::fromStdString(subject_area.recorded_by);
        case RecordedAt: return relative_time_helper::format(subject_area.recorded_at);
        default: return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(subject_area.name, subject_area.domain_name);
    }

    return {};
}

QVariant ClientSubjectAreaModel::headerData(int section,
    Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case Name: return tr("Name");
    case DomainName: return tr("Domain");
    case Description: return tr("Description");
    case Version: return tr("Version");
    case RecordedBy: return tr("Recorded By");
    case RecordedAt: return tr("Recorded At");
    default: return {};
    }
}

void ClientSubjectAreaModel::refresh() {
    if (!clientManager_ || !clientManager_->isConnected() || is_fetching_)
        return;

    is_fetching_ = true;
    BOOST_LOG_SEV(lg(), debug) << "Fetching subject areas...";

    QPointer<ClientSubjectAreaModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
            if (!self || !self->clientManager_) {
                return {.success = false, .subject_areas = {},
                        .error_message = "Model was destroyed",
                        .error_details = {}};
            }

            dq::messaging::get_subject_areas_request request;
            auto payload = request.serialize();

            comms::messaging::frame request_frame(
                comms::messaging::message_type::get_subject_areas_request,
                0, std::move(payload));

            auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send request";
                return {.success = false, .subject_areas = {},
                        .error_message = "Failed to send request",
                        .error_details = {}};
            }

            // Check for server error response
            if (auto err = exception_helper::check_error_response(*response_result)) {
                BOOST_LOG_SEV(lg(), error) << "Server error: "
                                           << err->message.toStdString();
                return {.success = false, .subject_areas = {},
                        .error_message = err->message,
                        .error_details = err->details};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response";
                return {.success = false, .subject_areas = {},
                        .error_message = "Failed to decompress response",
                        .error_details = {}};
            }

            auto response = dq::messaging::get_subject_areas_response::deserialize(
                *payload_result);
            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
                return {.success = false, .subject_areas = {},
                        .error_message = "Failed to deserialize response",
                        .error_details = {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Fetched " << response->subject_areas.size()
                                       << " subject areas";
            return {.success = true, .subject_areas = std::move(response->subject_areas),
                    .error_message = {}, .error_details = {}};
        }, "subject areas");
    });

    watcher_->setFuture(future);
}

void ClientSubjectAreaModel::onSubjectAreasLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch subject areas: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    beginResetModel();
    subject_areas_ = std::move(result.subject_areas);
    update_recent_subject_areas();
    endResetModel();

    BOOST_LOG_SEV(lg(), debug) << "Loaded " << subject_areas_.size()
                               << " subject areas";
    emit dataLoaded();
}

const dq::domain::subject_area* ClientSubjectAreaModel::getSubjectArea(
    int row) const {
    if (row < 0 || row >= static_cast<int>(subject_areas_.size()))
        return nullptr;
    return &subject_areas_[row];
}

std::string ClientSubjectAreaModel::make_key(
    const std::string& name, const std::string& domain_name) const {
    return name + "|" + domain_name;
}

void ClientSubjectAreaModel::update_recent_subject_areas() {
    recent_keys_.clear();
    QDateTime now = QDateTime::currentDateTime();
    last_reload_time_ = now;

    for (const auto& subject_area : subject_areas_) {
        auto recorded = QDateTime::fromSecsSinceEpoch(
            std::chrono::duration_cast<std::chrono::seconds>(
                subject_area.recorded_at.time_since_epoch()).count());

        if (recorded.secsTo(now) <= 60) {
            recent_keys_.insert(make_key(subject_area.name,
                                         subject_area.domain_name));
        }
    }

    if (!recent_keys_.empty()) {
        pulse_count_ = 0;
        pulse_state_ = true;
        pulse_timer_->start();
    }
}

void ClientSubjectAreaModel::onPulseTimerTimeout() {
    pulse_state_ = !pulse_state_;
    pulse_count_++;

    if (pulse_count_ >= max_pulse_cycles_) {
        pulse_timer_->stop();
        recent_keys_.clear();
    }

    emit dataChanged(index(0, 0),
                     index(rowCount() - 1, columnCount() - 1),
                     {Qt::ForegroundRole});
}

QVariant ClientSubjectAreaModel::recency_foreground_color(
    const std::string& name, const std::string& domain_name) const {
    if (recent_keys_.contains(make_key(name, domain_name)) && pulse_state_) {
        return QColor(100, 200, 100);
    }
    return {};
}

}
