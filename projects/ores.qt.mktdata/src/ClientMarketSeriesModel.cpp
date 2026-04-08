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
#include "ores.qt/ClientMarketSeriesModel.hpp"

#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPointer>
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.marketdata.api/messaging/market_series_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {

QString asset_class_label(marketdata::domain::asset_class ac) {
    using namespace marketdata::domain;
    switch (ac) {
    case asset_class::fx:          return "FX";
    case asset_class::rates:       return "Rates";
    case asset_class::credit:      return "Credit";
    case asset_class::equity:      return "Equity";
    case asset_class::commodity:   return "Commodity";
    case asset_class::inflation:   return "Inflation";
    case asset_class::bond:        return "Bond";
    case asset_class::cross_asset: return "Cross Asset";
    }
    return "Unknown";
}

QString subclass_label(marketdata::domain::series_subclass sc) {
    using namespace marketdata::domain;
    switch (sc) {
    case series_subclass::spot:         return "Spot";
    case series_subclass::forward:      return "Forward";
    case series_subclass::volatility:   return "Volatility";
    case series_subclass::yield:        return "Yield";
    case series_subclass::basis:        return "Basis";
    case series_subclass::fra:          return "FRA";
    case series_subclass::xccy:         return "X-Ccy";
    case series_subclass::spread:       return "Spread";
    case series_subclass::index_credit: return "Credit Index";
    case series_subclass::recovery:     return "Recovery";
    case series_subclass::swap:         return "Swap";
    case series_subclass::capfloor:     return "Cap/Floor";
    case series_subclass::seasonality:  return "Seasonality";
    case series_subclass::price:        return "Price";
    case series_subclass::correlation:  return "Correlation";
    }
    return "Unknown";
}

} // namespace

ClientMarketSeriesModel::ClientMarketSeriesModel(
    ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)) {
    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientMarketSeriesModel::onDataLoaded);
}

int ClientMarketSeriesModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return static_cast<int>(entries_.size());
}

int ClientMarketSeriesModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return ColumnCount;
}

QVariant ClientMarketSeriesModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid() || index.row() >= static_cast<int>(entries_.size()))
        return {};

    const auto& s = entries_[static_cast<std::size_t>(index.row())];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case SeriesType:  return QString::fromStdString(s.series_type);
        case Metric:      return QString::fromStdString(s.metric);
        case Qualifier:   return QString::fromStdString(s.qualifier);
        case AssetClass:  return asset_class_label(s.asset_class);
        case Subclass:    return subclass_label(s.subclass);
        case IsScalar:    return s.is_scalar ? "Yes" : "No";
        case Version:     return s.version;
        case ModifiedBy:  return QString::fromStdString(s.modified_by);
        case RecordedAt:  return relative_time_helper::format(s.recorded_at);
        default: break;
        }
    }
    return {};
}

QVariant ClientMarketSeriesModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case SeriesType:  return "Type";
    case Metric:      return "Metric";
    case Qualifier:   return "Qualifier";
    case AssetClass:  return "Asset Class";
    case Subclass:    return "Subclass";
    case IsScalar:    return "Scalar";
    case Version:     return "Ver";
    case ModifiedBy:  return "Modified By";
    case RecordedAt:  return "Recorded At";
    default: return {};
    }
}

const marketdata::domain::market_series*
ClientMarketSeriesModel::getSeries(int row) const {
    if (row < 0 || row >= static_cast<int>(entries_.size()))
        return nullptr;
    return &entries_[static_cast<std::size_t>(row)];
}

void ClientMarketSeriesModel::set_series_type_filter(const std::string& t) {
    series_type_filter_ = t;
}

void ClientMarketSeriesModel::set_page_size(std::uint32_t size) {
    page_size_ = size > 0 ? size : 500;
}

void ClientMarketSeriesModel::refresh(bool /*replace*/) {
    if (is_fetching_) return;
    fetch_data(0, page_size_);
}

void ClientMarketSeriesModel::load_page(std::uint32_t offset, std::uint32_t limit) {
    if (is_fetching_) return;
    fetch_data(offset, limit);
}

void ClientMarketSeriesModel::fetch_data(
    std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientMarketSeriesModel> self = this;

    QFuture<FetchResult> future =
        QtConcurrent::run([self, offset, limit]() -> FetchResult {
            return exception_helper::wrap_async_fetch<FetchResult>(
                [&]() -> FetchResult {
                    if (!self || !self->clientManager_) {
                        return {.success = false, .entries = {},
                                .total_available_count = 0,
                                .error_message = "Model destroyed",
                                .error_details = {}};
                    }
                    marketdata::messaging::get_market_series_request req;
                    req.offset = static_cast<int>(offset);
                    req.limit  = static_cast<int>(limit);
                    req.series_type = self->series_type_filter_;

                    auto result =
                        self->clientManager_->process_authenticated_request(
                            std::move(req));
                    if (!result) {
                        return {.success = false, .entries = {},
                                .total_available_count = 0,
                                .error_message = QString::fromStdString(
                                    "Failed to fetch market series: " +
                                    result.error()),
                                .error_details = {}};
                    }
                    return {.success = true,
                            .entries = std::move(result->series),
                            .total_available_count =
                                static_cast<std::uint32_t>(
                                    result->total_available_count),
                            .error_message = {},
                            .error_details = {}};
                }, "market series");
        });

    watcher_->setFuture(future);
}

void ClientMarketSeriesModel::onDataLoaded() {
    is_fetching_ = false;
    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch market series: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    beginResetModel();
    entries_ = std::move(result.entries);
    total_available_count_ = result.total_available_count;
    endResetModel();

    emit dataLoaded();
}

}
