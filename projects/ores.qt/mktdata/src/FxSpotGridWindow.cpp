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
#include "ores.qt/FxSpotGridWindow.hpp"
#include "ores.marketdata.api/messaging/market_series_protocol.hpp"
#include <QHeaderView>
#include <QMetaObject>
#include <QPointer>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <chrono>

namespace ores::qt {

using namespace ores::logging;

// Tick age thresholds for status derivation.
static constexpr auto k_live_threshold = std::chrono::seconds(5);
static constexpr auto k_stale_threshold = std::chrono::seconds(30);

// Flash duration in milliseconds.
static constexpr int k_flash_ms = 300;

// Staleness poll interval.
static constexpr int k_stale_poll_ms = 2000;

FxSpotGridWindow::FxSpotGridWindow(ClientManager* clientManager, QWidget* parent)
    : QWidget(parent)
    , clientManager_(clientManager)
    , table_(new QTableWidget(0, ColumnCount, this))
    , staleTimer_(new QTimer(this))
    , loadWatcher_(new QFutureWatcher<LoadResult>(this)) {

    setupUi();
    connect(loadWatcher_, &QFutureWatcher<LoadResult>::finished, this,
            &FxSpotGridWindow::onLoadFinished);
    connect(staleTimer_, &QTimer::timeout, this, &FxSpotGridWindow::onStaleCheck);
    staleTimer_->start(k_stale_poll_ms);
    reload();
}

void FxSpotGridWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->addWidget(table_);

    table_->setHorizontalHeaderLabels({tr("Pair"), tr("Mid"), tr("24h Chg"), tr("Status")});
    table_->horizontalHeader()->setSectionResizeMode(ColPair, QHeaderView::ResizeToContents);
    table_->horizontalHeader()->setSectionResizeMode(ColMid, QHeaderView::ResizeToContents);
    table_->horizontalHeader()->setSectionResizeMode(ColChange, QHeaderView::ResizeToContents);
    table_->horizontalHeader()->setSectionResizeMode(ColStatus, QHeaderView::Stretch);
    table_->verticalHeader()->hide();
    table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    table_->setSelectionBehavior(QAbstractItemView::SelectRows);
    table_->setAlternatingRowColors(true);
}

void FxSpotGridWindow::reload() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorOccurred(tr("Not connected to server"));
        return;
    }

    rows_.clear();
    table_->setRowCount(0);
    emit statusChanged(tr("Loading FX spot series…"));

    auto* cm = clientManager_;
    auto task = [cm]() -> LoadResult {
        namespace m = marketdata::messaging;
        LoadResult r;
        auto resp = cm->process_authenticated_request(
            m::get_market_series_request{.offset = 0, .limit = 1000, .series_type = "FX"});
        if (!resp) {
            r.error = QString::fromStdString(resp.error());
            return r;
        }
        // Keep only scalar spot series (FX spot rates, not surfaces or curves).
        for (auto& s : resp->series) {
            if (s.is_scalar)
                r.series.push_back(std::move(s));
        }
        r.success = true;
        return r;
    };

    loadWatcher_->setFuture(QtConcurrent::run(task));
}

void FxSpotGridWindow::onLoadFinished() {
    auto result = loadWatcher_->result();
    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to load FX series: " << result.error.toStdString();
        emit errorOccurred(result.error);
        return;
    }
    buildRows(result.series);
    emit statusChanged(tr("Loaded %1 FX spot series").arg(result.series.size()));
}

void FxSpotGridWindow::buildRows(const std::vector<marketdata::domain::market_series>& series) {
    rows_.clear();
    table_->setRowCount(0);

    int row = 0;
    for (const auto& s : series) {
        const std::string ore_key = s.series_type + "/" + s.metric + "/" + s.qualifier;

        table_->insertRow(row);

        auto* pairItem = new QTableWidgetItem(QString::fromStdString(s.qualifier));
        pairItem->setTextAlignment(Qt::AlignLeft | Qt::AlignVCenter);
        table_->setItem(row, ColPair, pairItem);

        auto* midItem = new QTableWidgetItem(tr("—"));
        midItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
        table_->setItem(row, ColMid, midItem);

        auto* chgItem = new QTableWidgetItem(tr("0.00%"));
        chgItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
        table_->setItem(row, ColChange, chgItem);

        auto* statusItem = new QTableWidgetItem(statusText(FeedStatus::Pending));
        statusItem->setForeground(statusColor(FeedStatus::Pending));
        statusItem->setTextAlignment(Qt::AlignCenter);
        table_->setItem(row, ColStatus, statusItem);

        RowState rs;
        rs.row = row;
        rs.ore_key = ore_key;
        rows_.emplace(ore_key, std::move(rs));
        ++row;
    }

    // Subscribe after all rows are in the map so the callback can find them.
    for (auto& [key, rs] : rows_)
        subscribe(rs);
}

void FxSpotGridWindow::subscribe(RowState& rs) {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    const std::string ore_key = rs.ore_key;
    QPointer<FxSpotGridWindow> self = this;

    try {
        rs.subscription = std::make_unique<marketdata::client::fx_spot_subscription>(
            clientManager_->nats_client(),
            ore_key,
            [self, ore_key](const marketdata::domain::fx_spot_tick& tick) {
                const double mid = tick.mid;
                const auto when = tick.datetime;
                QMetaObject::invokeMethod(
                    self,
                    [self, ore_key, mid, when]() {
                        if (self)
                            self->applyTick(ore_key, mid, when);
                    },
                    Qt::QueuedConnection);
            });
        BOOST_LOG_SEV(lg(), debug) << "Subscribed to " << ore_key;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Subscribe failed for " << ore_key << ": " << e.what();
    }
}

void FxSpotGridWindow::applyTick(const std::string& ore_key, double mid,
                                  std::chrono::system_clock::time_point when) {
    auto it = rows_.find(ore_key);
    if (it == rows_.end())
        return;

    RowState& rs = it->second;
    const bool up = mid >= rs.last_mid;
    const QColor flashColor = up ? QColor(0, 200, 80, 180) : QColor(220, 50, 50, 180);

    rs.last_mid = mid;
    rs.last_tick = when;
    rs.ever_ticked = true;

    // Update mid cell.
    auto* midItem = table_->item(rs.row, ColMid);
    if (midItem)
        midItem->setText(QString::number(mid, 'f', 5));

    // Flash mid and pair cells.
    for (int col : {ColPair, ColMid}) {
        auto* item = table_->item(rs.row, col);
        if (item)
            item->setBackground(flashColor);
    }

    // Reset flash after k_flash_ms.
    const int row = rs.row;
    QPointer<FxSpotGridWindow> self = this;
    QTimer::singleShot(k_flash_ms, this, [self, row]() {
        if (!self)
            return;
        for (int col : {ColPair, ColMid}) {
            auto* item = self->table_->item(row, col);
            if (item)
                item->setBackground(QBrush());
        }
    });

    updateStatusCell(rs, FeedStatus::Live);
}

void FxSpotGridWindow::onStaleCheck() {
    for (auto& [key, rs] : rows_)
        updateStatusCell(rs, deriveStatus(rs));
}

FxSpotGridWindow::FeedStatus FxSpotGridWindow::deriveStatus(const RowState& rs) {
    if (!rs.ever_ticked)
        return FeedStatus::Pending;
    const auto age = std::chrono::system_clock::now() - rs.last_tick;
    if (age < k_live_threshold)
        return FeedStatus::Live;
    if (age < k_stale_threshold)
        return FeedStatus::Stale;
    return FeedStatus::Disconnected;
}

void FxSpotGridWindow::updateStatusCell(RowState& rs, FeedStatus status) {
    auto* item = table_->item(rs.row, ColStatus);
    if (!item)
        return;
    item->setText(statusText(status));
    item->setForeground(statusColor(status));
}

QString FxSpotGridWindow::statusText(FeedStatus s) {
    switch (s) {
    case FeedStatus::Pending: return QStringLiteral("PENDING");
    case FeedStatus::Live: return QStringLiteral("LIVE");
    case FeedStatus::Stale: return QStringLiteral("STALE");
    case FeedStatus::Disconnected: return QStringLiteral("DISCONNECTED");
    }
    return {};
}

QColor FxSpotGridWindow::statusColor(FeedStatus s) {
    switch (s) {
    case FeedStatus::Pending: return QColor(150, 150, 150);
    case FeedStatus::Live: return QColor(0, 180, 80);
    case FeedStatus::Stale: return QColor(200, 140, 0);
    case FeedStatus::Disconnected: return QColor(200, 50, 50);
    }
    return {};
}

} // namespace ores::qt
