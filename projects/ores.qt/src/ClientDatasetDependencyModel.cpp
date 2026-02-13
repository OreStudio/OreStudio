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
#include "ores.qt/ClientDatasetDependencyModel.hpp"

#include <QFutureWatcher>
#include <QtConcurrent>
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.dq/messaging/dataset_dependency_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

ClientDatasetDependencyModel::ClientDatasetDependencyModel(
    ClientManager* clientManager, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager) {
}

int ClientDatasetDependencyModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(dependencies_.size());
}

int ClientDatasetDependencyModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientDatasetDependencyModel::data(const QModelIndex& index,
                                            int role) const {
    if (!index.isValid() || index.row() >= static_cast<int>(dependencies_.size()))
        return {};

    if (role != Qt::DisplayRole)
        return {};

    const auto& dep = dependencies_[index.row()];

    switch (index.column()) {
    case DatasetCode:
        return QString::fromStdString(dep.dataset_code);
    case DependencyCode:
        return QString::fromStdString(dep.dependency_code);
    case Role:
        return QString::fromStdString(dep.role);
    case ModifiedBy:
        return QString::fromStdString(dep.modified_by);
    case RecordedAt:
        return relative_time_helper::format(dep.recorded_at);
    default:
        return {};
    }
}

QVariant ClientDatasetDependencyModel::headerData(int section,
                                                  Qt::Orientation orientation,
                                                  int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case DatasetCode: return tr("Dataset Code");
    case DependencyCode: return tr("Depends On");
    case Role: return tr("Role");
    case ModifiedBy: return tr("Modified By");
    case RecordedAt: return tr("Recorded At");
    default: return {};
    }
}

void ClientDatasetDependencyModel::loadData() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorOccurred("Not connected to server");
        return;
    }

    emit loadStarted();

    QPointer<ClientDatasetDependencyModel> self = this;

    struct LoadResult {
        bool success;
        std::vector<dq::domain::dataset_dependency> dependencies;
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

            dq::messaging::get_dataset_dependencies_request request;
            auto payload = request.serialize();

            comms::messaging::frame request_frame(
                comms::messaging::message_type::get_dataset_dependencies_request,
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
                dq::messaging::get_dataset_dependencies_response::deserialize(
                    *payload_result);
            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
                return {.success = false, .dependencies = {},
                        .error_message = "Invalid server response",
                        .error_details = {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Fetched "
                                       << response->dependencies.size()
                                       << " dataset dependencies";
            return {.success = true,
                    .dependencies = std::move(response->dependencies),
                    .error_message = {}, .error_details = {}};
        }, "dataset dependencies");
    };

    auto* watcher = new QFutureWatcher<LoadResult>(this);
    connect(watcher, &QFutureWatcher<LoadResult>::finished, this,
            [self, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error) << "Failed to fetch dataset dependencies: "
                                       << result.error_message.toStdString();
            emit self->errorOccurred(result.error_message, result.error_details);
            return;
        }

        self->beginResetModel();
        self->dependencies_ = std::move(result.dependencies);
        self->endResetModel();

        emit self->loadFinished();

        BOOST_LOG_SEV(lg(), debug)
            << "Loaded " << self->dependencies_.size() << " dataset dependencies";
    });

    watcher->setFuture(QtConcurrent::run(task));
}

void ClientDatasetDependencyModel::loadDataByDataset(
    const std::string& dataset_code) {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorOccurred("Not connected to server");
        return;
    }

    emit loadStarted();

    QPointer<ClientDatasetDependencyModel> self = this;
    std::string code_copy = dataset_code;

    struct LoadResult {
        bool success;
        std::vector<dq::domain::dataset_dependency> dependencies;
        QString error_message;
        QString error_details;
    };

    auto task = [self, code_copy]() -> LoadResult {
        return exception_helper::wrap_async_fetch<LoadResult>([&]() -> LoadResult {
            if (!self || !self->clientManager_) {
                return {.success = false, .dependencies = {},
                        .error_message = "Model was destroyed",
                        .error_details = {}};
            }

            dq::messaging::get_dataset_dependencies_by_dataset_request request;
            request.dataset_code = code_copy;
            auto payload = request.serialize();

            comms::messaging::frame request_frame(
                comms::messaging::message_type::get_dataset_dependencies_by_dataset_request,
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
                dq::messaging::get_dataset_dependencies_by_dataset_response::deserialize(
                    *payload_result);
            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
                return {.success = false, .dependencies = {},
                        .error_message = "Invalid server response",
                        .error_details = {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Fetched "
                                       << response->dependencies.size()
                                       << " dependencies for dataset: "
                                       << code_copy;
            return {.success = true,
                    .dependencies = std::move(response->dependencies),
                    .error_message = {}, .error_details = {}};
        }, "dataset dependencies by dataset");
    };

    auto* watcher = new QFutureWatcher<LoadResult>(this);
    connect(watcher, &QFutureWatcher<LoadResult>::finished, this,
            [self, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error) << "Failed to fetch dataset dependencies: "
                                       << result.error_message.toStdString();
            emit self->errorOccurred(result.error_message, result.error_details);
            return;
        }

        self->beginResetModel();
        self->dependencies_ = std::move(result.dependencies);
        self->endResetModel();

        emit self->loadFinished();

        BOOST_LOG_SEV(lg(), debug)
            << "Loaded " << self->dependencies_.size() << " dataset dependencies";
    });

    watcher->setFuture(QtConcurrent::run(task));
}

const std::vector<dq::domain::dataset_dependency>&
ClientDatasetDependencyModel::dependencies() const {
    return dependencies_;
}

std::vector<dq::domain::dataset_dependency>
ClientDatasetDependencyModel::dependenciesForDataset(
    const std::string& dataset_code) const {
    std::vector<dq::domain::dataset_dependency> result;
    for (const auto& dep : dependencies_) {
        if (dep.dataset_code == dataset_code) {
            result.push_back(dep);
        }
    }
    return result;
}

}
