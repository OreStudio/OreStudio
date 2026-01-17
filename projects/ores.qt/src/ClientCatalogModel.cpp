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
#include "ores.qt/ClientCatalogModel.hpp"

#include <QBrush>
#include <QColor>
#include <QFutureWatcher>
#include <QtConcurrent>
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.dq/messaging/data_organization_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

ClientCatalogModel::ClientCatalogModel(ClientManager* clientManager,
                                       QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager) {
}

int ClientCatalogModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(catalogs_.size());
}

int ClientCatalogModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientCatalogModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid() || index.row() >= static_cast<int>(catalogs_.size()))
        return {};

    const auto& catalog = catalogs_[index.row()];
    const QString key = QString::fromStdString(catalog.name);

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Name:
            return QString::fromStdString(catalog.name);
        case Description:
            return QString::fromStdString(catalog.description);
        case Owner:
            return catalog.owner
                ? QString::fromStdString(*catalog.owner)
                : QString();
        case Version:
            return catalog.version;
        case RecordedBy:
            return QString::fromStdString(catalog.recorded_by);
        case RecordedAt:
            return relative_time_helper::format(catalog.recorded_at);
        default:
            return {};
        }
    }

    if (role == Qt::BackgroundRole && isRecentlyModified(key)) {
        return QBrush(QColor(70, 130, 180, 50));
    }

    return {};
}

QVariant ClientCatalogModel::headerData(int section,
                                        Qt::Orientation orientation,
                                        int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case Name: return tr("Name");
    case Description: return tr("Description");
    case Owner: return tr("Owner");
    case Version: return tr("Version");
    case RecordedBy: return tr("Recorded By");
    case RecordedAt: return tr("Recorded At");
    default: return {};
    }
}

void ClientCatalogModel::loadData() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorOccurred("Not connected to server");
        return;
    }

    emit loadStarted();

    QPointer<ClientCatalogModel> self = this;

    struct LoadResult {
        bool success;
        std::string message;
        std::vector<dq::domain::catalog> catalogs;
    };

    auto task = [self]() -> LoadResult {
        if (!self || !self->clientManager_)
            return {false, "Model destroyed", {}};

        dq::messaging::get_catalogs_request request;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::get_catalogs_request,
            0, std::move(payload));

        auto response_result =
            self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result)
            return {false, "Failed to communicate with server", {}};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result)
            return {false, "Failed to decompress response", {}};

        auto response =
            dq::messaging::get_catalogs_response::deserialize(*payload_result);
        if (!response)
            return {false, "Invalid server response", {}};

        return {response->success, response->message,
                std::move(response->catalogs)};
    };

    auto* watcher = new QFutureWatcher<LoadResult>(this);
    connect(watcher, &QFutureWatcher<LoadResult>::finished, this,
            [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (result.success) {
            QSet<QString> oldKeys;
            for (const auto& c : self->catalogs_) {
                oldKeys.insert(QString::fromStdString(c.name));
            }

            self->beginResetModel();
            self->catalogs_ = std::move(result.catalogs);
            self->endResetModel();

            self->recentlyModifiedKeys_.clear();
            for (const auto& c : self->catalogs_) {
                QString key = QString::fromStdString(c.name);
                if (!oldKeys.contains(key)) {
                    self->markRecentlyModified(key);
                }
            }

            self->lastLoadTime_ = std::chrono::steady_clock::now();
            emit self->loadFinished();

            BOOST_LOG_SEV(lg(), debug)
                << "Loaded " << self->catalogs_.size() << " catalogs";
        } else {
            emit self->errorOccurred(QString::fromStdString(result.message));
        }
    });

    watcher->setFuture(QtConcurrent::run(task));
}

const dq::domain::catalog& ClientCatalogModel::catalogAt(int row) const {
    return catalogs_.at(row);
}

void ClientCatalogModel::markRecentlyModified(const QString& name) {
    recentlyModifiedKeys_.insert(name);
}

bool ClientCatalogModel::isRecentlyModified(const QString& name) const {
    return recentlyModifiedKeys_.contains(name);
}

}
