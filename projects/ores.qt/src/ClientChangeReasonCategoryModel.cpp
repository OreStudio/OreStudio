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
#include "ores.qt/ClientChangeReasonCategoryModel.hpp"

#include <QtConcurrent>
#include <QBrush>
#include "ores.dq/messaging/change_management_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;
using ores::comms::messaging::frame;
using ores::comms::messaging::message_type;

ClientChangeReasonCategoryModel::ClientChangeReasonCategoryModel(
    ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      pulse_timer_(new QTimer(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientChangeReasonCategoryModel::onCategoriesLoaded);

    pulse_timer_->setInterval(pulse_interval_ms_);
    connect(pulse_timer_, &QTimer::timeout,
            this, &ClientChangeReasonCategoryModel::onPulseTimerTimeout);
}

int ClientChangeReasonCategoryModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(categories_.size());
}

int ClientChangeReasonCategoryModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientChangeReasonCategoryModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= categories_.size())
        return {};

    const auto& category = categories_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Code:
            return QString::fromStdString(category.code);
        case Description:
            return QString::fromStdString(category.description);
        case Version:
            return category.version;
        case RecordedBy:
            return QString::fromStdString(category.recorded_by);
        case RecordedAt:
            return relative_time_helper::format(category.recorded_at);
        default:
            return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(category.code);
    }

    return {};
}

QVariant ClientChangeReasonCategoryModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case Code:
        return tr("Code");
    case Description:
        return tr("Description");
    case Version:
        return tr("Version");
    case RecordedBy:
        return tr("Recorded By");
    case RecordedAt:
        return tr("Recorded At");
    default:
        return {};
    }
}

void ClientChangeReasonCategoryModel::refresh() {
    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), debug) << "Already fetching, skipping refresh";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        emit loadError("Not connected to server");
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Starting category fetch";
    is_fetching_ = true;

    QPointer<ClientChangeReasonCategoryModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self]() -> FetchResult {
        if (!self || !self->clientManager_) {
            return {false, {}};
        }

        dq::messaging::get_change_reason_categories_request request;
        auto payload = request.serialize();

        frame request_frame(
            message_type::get_change_reason_categories_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));
        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to send request";
            return {false, {}};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to decompress response";
            return {false, {}};
        }

        auto response = dq::messaging::get_change_reason_categories_response::
            deserialize(*payload_result);
        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
            return {false, {}};
        }

        BOOST_LOG_SEV(lg(), debug) << "Fetched " << response->categories.size()
                                   << " categories";
        return {true, std::move(response->categories)};
    });

    watcher_->setFuture(future);
}

void ClientChangeReasonCategoryModel::onCategoriesLoaded() {
    is_fetching_ = false;

    auto result = watcher_->result();
    if (!result.success) {
        emit loadError("Failed to fetch categories from server");
        return;
    }

    beginResetModel();
    categories_ = std::move(result.categories);
    endResetModel();

    update_recent_categories();
    last_reload_time_ = QDateTime::currentDateTime();

    BOOST_LOG_SEV(lg(), info) << "Loaded " << categories_.size() << " categories";
    emit dataLoaded();
}

const dq::domain::change_reason_category*
ClientChangeReasonCategoryModel::getCategory(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= categories_.size())
        return nullptr;
    return &categories_[idx];
}

void ClientChangeReasonCategoryModel::update_recent_categories() {
    recent_category_codes_.clear();

    if (!last_reload_time_.isValid()) {
        return;
    }

    // Check for categories modified since last reload
    for (const auto& category : categories_) {
        auto recorded_at = QDateTime::fromSecsSinceEpoch(
            std::chrono::duration_cast<std::chrono::seconds>(
                category.recorded_at.time_since_epoch()).count());

        if (recorded_at > last_reload_time_) {
            recent_category_codes_.insert(category.code);
        }
    }

    if (!recent_category_codes_.empty()) {
        pulse_count_ = 0;
        pulse_state_ = false;
        pulse_timer_->start();
    }
}

QVariant ClientChangeReasonCategoryModel::recency_foreground_color(
    const std::string& code) const {
    if (recent_category_codes_.find(code) == recent_category_codes_.end()) {
        return {};
    }

    if (pulse_state_) {
        return QBrush(QColor(255, 200, 0)); // Yellow highlight
    }
    return {};
}

void ClientChangeReasonCategoryModel::onPulseTimerTimeout() {
    pulse_state_ = !pulse_state_;
    pulse_count_++;

    if (pulse_count_ >= max_pulse_cycles_ * 2) {
        pulse_timer_->stop();
        recent_category_codes_.clear();
    }

    // Notify views to repaint
    if (!categories_.empty()) {
        emit dataChanged(index(0, 0),
            index(static_cast<int>(categories_.size()) - 1, ColumnCount - 1));
    }
}

}
