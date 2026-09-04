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
#include "ores.qt/ClientYieldCurveProcessParameterDefinitionModel.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.synthetic.api/messaging/yield_curve_process_parameter_definition_protocol.hpp"
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

namespace {
std::string yield_curve_process_parameter_definition_key_extractor(
    const synthetic::domain::yield_curve_process_parameter_definition& e) {
    return e.parameter_name;
}
}

ClientYieldCurveProcessParameterDefinitionModel::ClientYieldCurveProcessParameterDefinitionModel(
    ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent)
    , clientManager_(clientManager)
    , watcher_(new QFutureWatcher<FetchResult>(this))
    , recencyTracker_(yield_curve_process_parameter_definition_key_extractor)
    , pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_,
            &QFutureWatcher<FetchResult>::finished,
            this,
            &ClientYieldCurveProcessParameterDefinitionModel::onDefinitionsLoaded);

    connect(pulseManager_,
            &RecencyPulseManager::pulse_state_changed,
            this,
            &ClientYieldCurveProcessParameterDefinitionModel::onPulseStateChanged);
    connect(pulseManager_,
            &RecencyPulseManager::pulsing_complete,
            this,
            &ClientYieldCurveProcessParameterDefinitionModel::onPulsingComplete);
}

int ClientYieldCurveProcessParameterDefinitionModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(parameter_definitions_.size());
}

int ClientYieldCurveProcessParameterDefinitionModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientYieldCurveProcessParameterDefinitionModel::data(const QModelIndex& index,
                                                               int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= parameter_definitions_.size())
        return {};

    const auto& parameter_definition = parameter_definitions_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
            case ProcessTypeCode:
                return QString::fromStdString(parameter_definition.process_type_code);
            case ParameterName:
                return QString::fromStdString(parameter_definition.parameter_name);
            case DisplayName:
                return QString::fromStdString(parameter_definition.display_name);
            case Symbol:
                return parameter_definition.symbol ?
                           QString::fromStdString(*parameter_definition.symbol) :
                           QString{};
            case ShortLabel:
                return QString::fromStdString(parameter_definition.short_label);
            case Description:
                return QString::fromStdString(parameter_definition.description);
            case DataType:
                return QString::fromStdString(parameter_definition.data_type);
            case DefaultValue:
                return parameter_definition.default_value;
            case MinValue:
                return parameter_definition.min_value ? QVariant(*parameter_definition.min_value) :
                                                        QVariant();
            case MaxValue:
                return parameter_definition.max_value ? QVariant(*parameter_definition.max_value) :
                                                        QVariant();
            case DisplayOrder:
                return static_cast<qlonglong>(parameter_definition.display_order);
            case Version:
                return static_cast<qlonglong>(parameter_definition.version);
            case ModifiedBy:
                return QString::fromStdString(parameter_definition.modified_by);
            case RecordedAt:
                return relative_time_helper::format(parameter_definition.recorded_at);
            default:
                return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(parameter_definition.parameter_name);
    }

    return {};
}

QVariant ClientYieldCurveProcessParameterDefinitionModel::headerData(int section,
                                                                     Qt::Orientation orientation,
                                                                     int role) const {
    if (orientation != Qt::Horizontal || (role != Qt::DisplayRole && role != Qt::ToolTipRole))
        return {};

    if (role == Qt::ToolTipRole) {
        switch (section) {
            default:
                return {};
        }
    }

    switch (section) {
        case ProcessTypeCode:
            return tr("Process Type");
        case ParameterName:
            return tr("Parameter");
        case DisplayName:
            return tr("Display Name");
        case Symbol:
            return tr("Symbol");
        case ShortLabel:
            return tr("Short Label");
        case Description:
            return tr("Description");
        case DataType:
            return tr("Data Type");
        case DefaultValue:
            return tr("Default");
        case MinValue:
            return tr("Min");
        case MaxValue:
            return tr("Max");
        case DisplayOrder:
            return tr("Display Order");
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

void ClientYieldCurveProcessParameterDefinitionModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn)
            << "Cannot refresh yield curve process parameter definition model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!parameter_definitions_.empty()) {
        beginResetModel();
        parameter_definitions_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_parameter_definitions(0, page_size_);
}

void ClientYieldCurveProcessParameterDefinitionModel::load_page(std::uint32_t offset,
                                                                std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!parameter_definitions_.empty()) {
        beginResetModel();
        parameter_definitions_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_parameter_definitions(offset, limit);
}

void ClientYieldCurveProcessParameterDefinitionModel::fetch_parameter_definitions(
    std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientYieldCurveProcessParameterDefinitionModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self, offset, limit]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>(
            [&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug)
                    << "Making yield curve process parameter definitions request with offset="
                    << offset << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false,
                            .parameter_definitions = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                synthetic::messaging::get_yield_curve_process_parameter_definitions_request request;
                request.offset = offset;
                request.limit = limit;

                auto result =
                    self->clientManager_->process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to send request: " << result.error();
                    return {.success = false,
                            .parameter_definitions = {},
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
                            .parameter_definitions = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(result->message),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug)
                    << "Fetched " << result->parameter_definitions.size()
                    << " yield curve process parameter definitions, total available: "
                    << result->total_available_count;
                return {.success = true,
                        .parameter_definitions = std::move(result->parameter_definitions),
                        .total_available_count =
                            static_cast<std::uint32_t>(result->total_available_count),
                        .error_message = {},
                        .error_details = {}};
            },
            "yield curve process parameter definitions");
    });

    watcher_->setFuture(future);
}

void ClientYieldCurveProcessParameterDefinitionModel::onDefinitionsLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch yield curve process parameter definitions: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.parameter_definitions.size());

    if (new_count > 0) {
        beginResetModel();
        parameter_definitions_ = std::move(result.parameter_definitions);
        endResetModel();

        const bool has_recent = recencyTracker_.update(parameter_definitions_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug)
                << "Found " << recencyTracker_.recent_count()
                << " yield curve process parameter definitions newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count
                              << " yield curve process parameter definitions."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientYieldCurveProcessParameterDefinitionModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const synthetic::domain::yield_curve_process_parameter_definition*
ClientYieldCurveProcessParameterDefinitionModel::getDefinition(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= parameter_definitions_.size())
        return nullptr;
    return &parameter_definitions_[idx];
}


QVariant ClientYieldCurveProcessParameterDefinitionModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientYieldCurveProcessParameterDefinitionModel::onPulseStateChanged(bool /*isOn*/) {
    if (!parameter_definitions_.empty()) {
        emit dataChanged(
            index(0, 0), index(rowCount() - 1, columnCount() - 1), {Qt::ForegroundRole});
    }
}

void ClientYieldCurveProcessParameterDefinitionModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
