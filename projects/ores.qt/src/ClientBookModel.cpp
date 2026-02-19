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
#include "ores.qt/ClientBookModel.hpp"

#include <QtConcurrent>
#include "ores.refdata/messaging/book_protocol.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.comms/net/client_session.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    std::string book_key_extractor(const refdata::domain::book& e) {
        return e.name;
    }
}

ClientBookModel::ClientBookModel(
    ClientManager* clientManager, ImageCache* imageCache, QObject* parent)
    : QAbstractTableModel(parent),
      clientManager_(clientManager),
      imageCache_(imageCache),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      recencyTracker_(book_key_extractor),
      pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &ClientBookModel::onBooksLoaded);

    connect(pulseManager_, &RecencyPulseManager::pulse_state_changed,
            this, &ClientBookModel::onPulseStateChanged);
    connect(pulseManager_, &RecencyPulseManager::pulsing_complete,
            this, &ClientBookModel::onPulsingComplete);

    if (imageCache_) {
        connect(imageCache_, &ImageCache::imagesLoaded, this, [this]() {
            if (!books_.empty()) {
                emit dataChanged(index(0, 0),
                    index(rowCount() - 1, columnCount() - 1),
                    {Qt::DecorationRole});
            }
        });
        connect(imageCache_, &ImageCache::imageLoaded, this, [this](const QString&) {
            if (!books_.empty()) {
                emit dataChanged(index(0, 0),
                    index(rowCount() - 1, columnCount() - 1),
                    {Qt::DecorationRole});
            }
        });
    }
}

int ClientBookModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(books_.size());
}

int ClientBookModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientBookModel::data(
    const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= books_.size())
        return {};

    const auto& book = books_[row];

    if (role == Qt::DecorationRole && index.column() == Column::LedgerCcy) {
        if (imageCache_ && !book.ledger_ccy.empty()) {
            return imageCache_->getCurrencyFlagIcon(book.ledger_ccy);
        }
        return {};
    }

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Name:
            return QString::fromStdString(book.name);
        case LedgerCcy:
            return QString::fromStdString(book.ledger_ccy);
        case BookStatus:
            return QString::fromStdString(book.book_status);
        case IsTradingBook:
            return book.is_trading_book != 0 ? tr("Trading") : tr("Banking");
        case CostCenter:
            return QString::fromStdString(book.cost_center);
        case Version:
            return book.version;
        case ModifiedBy:
            return QString::fromStdString(book.modified_by);
        case RecordedAt:
            return relative_time_helper::format(book.recorded_at);
        default:
            return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(book.name);
    }

    return {};
}

QVariant ClientBookModel::headerData(
    int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case Name:
        return tr("Name");
    case LedgerCcy:
        return tr("Ledger Ccy");
    case BookStatus:
        return tr("Status");
    case IsTradingBook:
        return tr("Book Type");
    case CostCenter:
        return tr("Cost Centre");
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

void ClientBookModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh book model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!books_.empty()) {
        beginResetModel();
        books_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_books(0, page_size_);
}

void ClientBookModel::load_page(std::uint32_t offset,
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

    if (!books_.empty()) {
        beginResetModel();
        books_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_books(offset, limit);
}

void ClientBookModel::fetch_books(
    std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientBookModel> self = this;

    QFuture<FetchResult> future =
        QtConcurrent::run([self, offset, limit]() -> FetchResult {
            return exception_helper::wrap_async_fetch<FetchResult>([&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug) << "Making books request with offset="
                                           << offset << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false, .books = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                refdata::messaging::get_books_request request;
                request.offset = offset;
                request.limit = limit;

                auto result = self->clientManager_->
                    process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to fetch books: "
                                               << comms::net::to_string(result.error());
                    return {.success = false, .books = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(
                                "Failed to fetch books: " + comms::net::to_string(result.error())),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug) << "Fetched " << result->books.size()
                                           << " books, total available: "
                                           << result->total_available_count;
                return {.success = true,
                        .books = std::move(result->books),
                        .total_available_count = result->total_available_count,
                        .error_message = {}, .error_details = {}};
            }, "books");
        });

    watcher_->setFuture(future);
}

void ClientBookModel::onBooksLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch books: "
                                   << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.books.size());

    if (new_count > 0) {
        beginResetModel();
        books_ = std::move(result.books);
        endResetModel();

        const bool has_recent = recencyTracker_.update(books_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " books newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " books."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientBookModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const refdata::domain::book*
ClientBookModel::getBook(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= books_.size())
        return nullptr;
    return &books_[idx];
}

QVariant ClientBookModel::recency_foreground_color(
    const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientBookModel::onPulseStateChanged(bool /*isOn*/) {
    if (!books_.empty()) {
        emit dataChanged(index(0, 0), index(rowCount() - 1, columnCount() - 1),
            {Qt::ForegroundRole});
    }
}

void ClientBookModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
