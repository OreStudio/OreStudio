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
#include "ores.qt/ClientCodingSchemeModel.hpp"

#include <QtConcurrent>
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.dq/messaging/coding_scheme_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

ClientCodingSchemeModel::ClientCodingSchemeModel(
    ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      pulse_timer_(new QTimer(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientCodingSchemeModel::onSchemesLoaded);

    pulse_timer_->setInterval(pulse_interval_ms_);
    connect(pulse_timer_, &QTimer::timeout,
            this, &ClientCodingSchemeModel::onPulseTimerTimeout);
}

int ClientCodingSchemeModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return static_cast<int>(schemes_.size());
}

int ClientCodingSchemeModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return ColumnCount;
}

QVariant ClientCodingSchemeModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid() || index.row() >= static_cast<int>(schemes_.size()))
        return {};

    const auto& scheme = schemes_[index.row()];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Code: return QString::fromStdString(scheme.code);
        case Name: return QString::fromStdString(scheme.name);
        case AuthorityType: return QString::fromStdString(scheme.authority_type);
        case SubjectArea: return QString::fromStdString(scheme.subject_area_name);
        case Domain: return QString::fromStdString(scheme.domain_name);
        case Description: return QString::fromStdString(scheme.description);
        case Version: return scheme.version;
        case RecordedBy: return QString::fromStdString(scheme.recorded_by);
        case RecordedAt: return relative_time_helper::format(scheme.recorded_at);
        default: return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(scheme.code);
    }

    return {};
}

QVariant ClientCodingSchemeModel::headerData(int section,
    Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case Code: return tr("Code");
    case Name: return tr("Name");
    case AuthorityType: return tr("Authority Type");
    case SubjectArea: return tr("Subject Area");
    case Domain: return tr("Domain");
    case Description: return tr("Description");
    case Version: return tr("Version");
    case RecordedBy: return tr("Recorded By");
    case RecordedAt: return tr("Recorded At");
    default: return {};
    }
}

void ClientCodingSchemeModel::refresh() {
    if (!clientManager_ || !clientManager_->isConnected() || is_fetching_)
        return;

    is_fetching_ = true;
    BOOST_LOG_SEV(lg(), debug) << "Fetching coding schemes...";

    auto task = [cm = clientManager_]() -> FetchResult {
        dq::messaging::get_coding_schemes_request request;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::get_coding_schemes_request,
            0, std::move(payload));

        auto response_result = cm->sendRequest(std::move(request_frame));
        if (!response_result) {
            return {false, {}};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {false, {}};
        }

        auto response = dq::messaging::get_coding_schemes_response::deserialize(
            *payload_result);
        if (!response) {
            return {false, {}};
        }

        return {true, std::move(response->schemes)};
    };

    watcher_->setFuture(QtConcurrent::run(task));
}

void ClientCodingSchemeModel::onSchemesLoaded() {
    is_fetching_ = false;
    auto result = watcher_->result();

    if (result.success) {
        beginResetModel();
        auto old_schemes = std::move(schemes_);
        schemes_ = std::move(result.schemes);
        update_recent_schemes();
        endResetModel();

        BOOST_LOG_SEV(lg(), debug) << "Loaded " << schemes_.size()
                                   << " coding schemes";
        emit dataLoaded();
    } else {
        BOOST_LOG_SEV(lg(), error) << "Failed to load coding schemes";
        emit loadError(tr("Failed to load coding schemes"));
    }
}

const dq::domain::coding_scheme* ClientCodingSchemeModel::getScheme(
    int row) const {
    if (row < 0 || row >= static_cast<int>(schemes_.size()))
        return nullptr;
    return &schemes_[row];
}

void ClientCodingSchemeModel::update_recent_schemes() {
    recent_scheme_codes_.clear();
    QDateTime now = QDateTime::currentDateTime();
    last_reload_time_ = now;

    for (const auto& scheme : schemes_) {
        auto recorded = QDateTime::fromSecsSinceEpoch(
            std::chrono::duration_cast<std::chrono::seconds>(
                scheme.recorded_at.time_since_epoch()).count());

        if (recorded.secsTo(now) <= 60) {
            recent_scheme_codes_.insert(scheme.code);
        }
    }

    if (!recent_scheme_codes_.empty()) {
        pulse_count_ = 0;
        pulse_state_ = true;
        pulse_timer_->start();
    }
}

void ClientCodingSchemeModel::onPulseTimerTimeout() {
    pulse_state_ = !pulse_state_;
    pulse_count_++;

    if (pulse_count_ >= max_pulse_cycles_) {
        pulse_timer_->stop();
        recent_scheme_codes_.clear();
    }

    emit dataChanged(index(0, 0),
                     index(rowCount() - 1, columnCount() - 1),
                     {Qt::ForegroundRole});
}

QVariant ClientCodingSchemeModel::recency_foreground_color(
    const std::string& code) const {
    if (recent_scheme_codes_.contains(code) && pulse_state_) {
        return QColor(100, 200, 100);
    }
    return {};
}

}
