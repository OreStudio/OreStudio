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
#include "ores.qt/ClientPortfolioModel.hpp"

#include <QtConcurrent>
#include "ores.refdata/messaging/portfolio_protocol.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.comms/net/client_session.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    std::string portfolio_key_extractor(const refdata::domain::portfolio& e) {
        return e.name;
    }
}

ClientPortfolioModel::ClientPortfolioModel(
    ClientManager* clientManager, ImageCache* imageCache, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      imageCache_(imageCache),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(portfolio_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientPortfolioModel::onPortfoliosLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientPortfolioModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientPortfolioModel::onPulsingComplete);

    if (imageCache_) {
        connect(imageCache_, &ImageCache::imagesLoaded, this, [this]() {
            if (!portfolios_.empty()) {
                emit dataChanged(index(0, 0),
                    index(rowCount() - 1, columnCount() - 1),
                    {Qt::DecorationRole});
            }
        });
        connect(imageCache_, &ImageCache::imageLoaded, this, [this](const QString&) {
            if (!portfolios_.empty()) {
                emit dataChanged(index(0, 0),
                    index(rowCount() - 1, columnCount() - 1),
                    {Qt::DecorationRole});
            }
        });
    }
}

int ClientPortfolioModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(portfolios_.size());
}

int ClientPortfolioModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientPortfolioModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= portfolios_.size())
        return {};

    const auto& portfolio = portfolios_[row];

    if (role == Qt::DecorationRole && index.column() == Column::AggregationCcy) {
        if (imageCache_ && !portfolio.aggregation_ccy.empty()) {
            return imageCache_->getCurrencyFlagIcon(portfolio.aggregation_ccy);
        }
        return {};
    }

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Name:
            return QString::fromStdString(portfolio.name);
        case AggregationCcy:
            return QString::fromStdString(portfolio.aggregation_ccy);
        case PurposeType:
            return QString::fromStdString(portfolio.purpose_type);
        case IsVirtual:
            return portfolio.is_virtual != 0 ? tr("Virtual") : QString{};
        case Status:
            return QString::fromStdString(portfolio.status);
        case Version:
            return portfolio.version;
        case ModifiedBy:
            return QString::fromStdString(portfolio.modified_by);
        case RecordedAt:
            return relative_time_helper::format(portfolio.recorded_at);
        default:
            return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(portfolio.name);
    }

    return {};
}

QVariant ClientPortfolioModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case Name:
        return tr("Name");
    case AggregationCcy:
        return tr("Agg. Currency");
    case PurposeType:
        return tr("Purpose");
    case IsVirtual:
        return tr("Type");
    case Status:
        return tr("Status");
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

void ClientPortfolioModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh portfolio model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!portfolios_.empty()) {
        beginResetModel();
        portfolios_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_portfolios(0, page_size_);
}

void ClientPortfolioModel::load_page(std::uint32_t offset,
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

    if (!portfolios_.empty()) {
        beginResetModel();
        portfolios_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_portfolios(offset, limit);
}

void ClientPortfolioModel::fetch_portfolios(
    std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientPortfolioModel> self = this;

    QFuture<FetchResult> future =
        QtConcurrent::run([self, offset, limit]() -> FetchResult {
            return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug) << "Making portfolios request with offset="
                                           << offset << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false, .portfolios = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                refdata::messaging::get_portfolios_request request;
                request.offset = offset;
                request.limit = limit;

                auto result = self->clientManager_->
                    process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to fetch portfolios: "
                                               << comms::net::to_string(result.error());
                    return {.success = false, .portfolios = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(
                                "Failed to fetch portfolios: " + comms::net::to_string(result.error())),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug) << "Fetched " << result->portfolios.size()
                                           << " portfolios, total available: "
                                           << result->total_available_count;
                return {.success = true,
                        .portfolios = std::move(result->portfolios),
                        .total_available_count = result->total_available_count,
                        .error_message = {}, .error_details = {}};
            }, "portfolios");
        });

    watcher_->setFuture(future);
}

void ClientPortfolioModel::onPortfoliosLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch portfolios: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.portfolios.size());

    if (new_count > 0) {
        beginResetModel();
        portfolios_ = std::move(result.portfolios);
        endResetModel();

        const bool has_recent = recencyTracker_.update(portfolios_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " portfolios newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " portfolios."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientPortfolioModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const refdata::domain::portfolio*
ClientPortfolioModel::getPortfolio(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= portfolios_.size())
        return nullptr;
    return &portfolios_[idx];
}

QVariant ClientPortfolioModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientPortfolioModel::onPulseStateChanged(bool /*isOn*/) {
    if (!portfolios_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientPortfolioModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
