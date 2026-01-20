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
#include "ores.qt/ClientCatalogDependencyModel.hpp"

#include <QFutureWatcher>
#include <QtConcurrent>
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.dq/messaging/catalog_dependency_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

ClientCatalogDependencyModel::ClientCatalogDependencyModel(
    ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager) {
}

int ClientCatalogDependencyModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(dependencies_.size());
}

int ClientCatalogDependencyModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientCatalogDependencyModel::data(const QModelIndex& index,
                                            int role) const {
    if (!index.isValid() || index.row() >= static_cast<int>(dependencies_.size()))
        return {};

    if (role != Qt::DisplayRole)
        return {};

    const auto& dep = dependencies_[index.row()];

    switch (index.column()) {
    case CatalogName:
        return QString::fromStdString(dep.catalog_name);
    case DependencyName:
        return QString::fromStdString(dep.dependency_name);
    case RecordedBy:
        return QString::fromStdString(dep.recorded_by);
    case RecordedAt:
        return relative_time_helper::format(dep.recorded_at);
    default:
        return {};
    }
}

QVariant ClientCatalogDependencyModel::headerData(int section,
                                                  Qt::Orientation orientation,
                                                  int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case CatalogName: return tr("Catalog");
    case DependencyName: return tr("Depends On");
    case RecordedBy: return tr("Recorded By");
    case RecordedAt: return tr("Recorded At");
    default: return {};
    }
}

void ClientCatalogDependencyModel::loadData() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorOccurred("Not connected to server");
        return;
    }

    emit loadStarted();

    QPointer<ClientCatalogDependencyModel> self = this;

    struct LoadResult {
        bool success;
        std::vector<dq::domain::catalog_dependency> dependencies;
        QString error_message;
        QString error_details;
    };

    auto task = [self]() -> LoadResult {
        return exception_helper::wrap_async_fetch<LoadResult>([&]() -> LoadResult {
            if (!self || !self->clientManager_) {
                return {.success = false, .dependencies = {},
                        .error_message = "Model was destroyed",
                        .error_details = {}};
            }

            dq::messaging::get_catalog_dependencies_request request;
            auto payload = request.serialize();

            comms::messaging::frame request_frame(
                comms::messaging::message_type::get_catalog_dependencies_request,
                0, std::move(payload));

            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));
            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send request";
                return {.success = false, .dependencies = {},
                        .error_message = "Failed to communicate with server",
                        .error_details = {}};
            }

            if (auto err = exception_helper::check_error_response(*response_result)) {
                BOOST_LOG_SEV(lg(), error) << "Server error: "
                                           << err->message.toStdString();
                return {.success = false, .dependencies = {},
                        .error_message = err->message,
                        .error_details = err->details};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response";
                return {.success = false, .dependencies = {},
                        .error_message = "Failed to decompress response",
                        .error_details = {}};
            }

            auto response =
                dq::messaging::get_catalog_dependencies_response::deserialize(
                    *payload_result);
            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
                return {.success = false, .dependencies = {},
                        .error_message = "Invalid server response",
                        .error_details = {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Fetched "
                                       << response->dependencies.size()
                                       << " catalog dependencies";
            return {.success = true,
                    .dependencies = std::move(response->dependencies),
                    .error_message = {}, .error_details = {}};
        }, "catalog dependencies");
    };

    auto* watcher = new QFutureWatcher<LoadResult>(this);
    connect(watcher, &QFutureWatcher<LoadResult>::finished, this,
            [self, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error) << "Failed to fetch catalog dependencies: "
                                       << result.error_message.toStdString();
            emit self->errorOccurred(result.error_message, result.error_details);
            return;
        }

        self->beginResetModel();
        self->dependencies_ = std::move(result.dependencies);
        self->endResetModel();

        emit self->loadFinished();

        BOOST_LOG_SEV(lg(), debug)
            << "Loaded " << self->dependencies_.size() << " catalog dependencies";
    });

    watcher->setFuture(QtConcurrent::run(task));
}

void ClientCatalogDependencyModel::loadDataByCatalog(
    const std::string& catalog_name) {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorOccurred("Not connected to server");
        return;
    }

    emit loadStarted();

    QPointer<ClientCatalogDependencyModel> self = this;
    std::string name_copy = catalog_name;

    struct LoadResult {
        bool success;
        std::vector<dq::domain::catalog_dependency> dependencies;
        QString error_message;
        QString error_details;
    };

    auto task = [self, name_copy]() -> LoadResult {
        return exception_helper::wrap_async_fetch<LoadResult>([&]() -> LoadResult {
            if (!self || !self->clientManager_) {
                return {.success = false, .dependencies = {},
                        .error_message = "Model was destroyed",
                        .error_details = {}};
            }

            dq::messaging::get_catalog_dependencies_by_catalog_request request;
            request.catalog_name = name_copy;
            auto payload = request.serialize();

            comms::messaging::frame request_frame(
                comms::messaging::message_type::get_catalog_dependencies_by_catalog_request,
                0, std::move(payload));

            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));
            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send request";
                return {.success = false, .dependencies = {},
                        .error_message = "Failed to communicate with server",
                        .error_details = {}};
            }

            if (auto err = exception_helper::check_error_response(*response_result)) {
                BOOST_LOG_SEV(lg(), error) << "Server error: "
                                           << err->message.toStdString();
                return {.success = false, .dependencies = {},
                        .error_message = err->message,
                        .error_details = err->details};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response";
                return {.success = false, .dependencies = {},
                        .error_message = "Failed to decompress response",
                        .error_details = {}};
            }

            auto response =
                dq::messaging::get_catalog_dependencies_by_catalog_response::deserialize(
                    *payload_result);
            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
                return {.success = false, .dependencies = {},
                        .error_message = "Invalid server response",
                        .error_details = {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Fetched "
                                       << response->dependencies.size()
                                       << " dependencies for catalog: "
                                       << name_copy;
            return {.success = true,
                    .dependencies = std::move(response->dependencies),
                    .error_message = {}, .error_details = {}};
        }, "catalog dependencies by catalog");
    };

    auto* watcher = new QFutureWatcher<LoadResult>(this);
    connect(watcher, &QFutureWatcher<LoadResult>::finished, this,
            [self, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error) << "Failed to fetch catalog dependencies: "
                                       << result.error_message.toStdString();
            emit self->errorOccurred(result.error_message, result.error_details);
            return;
        }

        self->beginResetModel();
        self->dependencies_ = std::move(result.dependencies);
        self->endResetModel();

        emit self->loadFinished();

        BOOST_LOG_SEV(lg(), debug)
            << "Loaded " << self->dependencies_.size() << " catalog dependencies";
    });

    watcher->setFuture(QtConcurrent::run(task));
}

const std::vector<dq::domain::catalog_dependency>&
ClientCatalogDependencyModel::dependencies() const {
    return dependencies_;
}

std::vector<dq::domain::catalog_dependency>
ClientCatalogDependencyModel::dependenciesForCatalog(
    const std::string& catalog_name) const {
    std::vector<dq::domain::catalog_dependency> result;
    for (const auto& dep : dependencies_) {
        if (dep.catalog_name == catalog_name) {
            result.push_back(dep);
        }
    }
    return result;
}

}
