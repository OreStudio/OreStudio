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
#include "ores.marketdata.api/messaging/feed_binding_protocol.hpp"
#include <QHeaderView>
#include <QLabel>
#include <QMetaObject>
#include <QPointer>
#include <QVBoxLayout>
#include <QWidget>
#include <QtConcurrent>
#include <chrono>
#include <sstream>

namespace ores::qt {

using namespace ores::logging;

static constexpr auto k_live_threshold = std::chrono::seconds(10);
static constexpr auto k_stale_threshold = std::chrono::seconds(60);
static constexpr int k_stale_poll_ms = 2000;

// ── badge colours ──────────────────────────────────────────────────────────
static const QColor k_pending_bg{100, 100, 100};
static const QColor k_pending_fg{220, 220, 220};
static const QColor k_live_bg{22, 163, 74};
static const QColor k_live_fg{255, 255, 255};
static const QColor k_stale_bg{180, 120, 0};
static const QColor k_stale_fg{255, 255, 255};
static const QColor k_disconnected_bg{185, 28, 28};
static const QColor k_disconnected_fg{255, 255, 255};

// ── rate colours ───────────────────────────────────────────────────────────
static const QColor k_up_color{34, 197, 94};   // green-400
static const QColor k_down_color{239, 68, 68}; // red-400
static const QColor k_flat_color{180, 180, 180};

static QString badge_style(const QColor& bg, const QColor& fg) {
    return QString("QLabel {"
                   "  background-color: %1;"
                   "  color: %2;"
                   "  border-radius: 8px;"
                   "  padding: 2px 10px;"
                   "  font-size: 11px;"
                   "  font-weight: bold;"
                   "}")
        .arg(bg.name(), fg.name());
}

static QLabel* make_badge(QWidget* parent) {
    auto* lbl = new QLabel(parent);
    lbl->setAlignment(Qt::AlignCenter);
    lbl->setMinimumWidth(110);
    return lbl;
}

static QColor pair_color_for_status(FxSpotGridWindow::FeedStatus s) {
    switch (s) {
        case FxSpotGridWindow::FeedStatus::Live:
            return k_up_color;
        case FxSpotGridWindow::FeedStatus::Stale:
            return QColor(200, 140, 0);
        case FxSpotGridWindow::FeedStatus::Disconnected:
            return k_down_color;
        default:
            return k_flat_color;
    }
}

static void apply_badge(QLabel* lbl,
                        FxSpotGridWindow::FeedStatus s,
                        std::chrono::system_clock::time_point last_tick) {
    using namespace std::chrono;
    switch (s) {
        case FxSpotGridWindow::FeedStatus::Pending:
            lbl->setText(QStringLiteral("PENDING"));
            lbl->setStyleSheet(badge_style(k_pending_bg, k_pending_fg));
            break;
        case FxSpotGridWindow::FeedStatus::Live:
            lbl->setText(QStringLiteral("● LIVE"));
            lbl->setStyleSheet(badge_style(k_live_bg, k_live_fg));
            break;
        case FxSpotGridWindow::FeedStatus::Stale: {
            const auto age = duration_cast<seconds>(system_clock::now() - last_tick).count();
            lbl->setText(QStringLiteral("⏱ STALE: %1s").arg(age));
            lbl->setStyleSheet(badge_style(k_stale_bg, k_stale_fg));
            break;
        }
        case FxSpotGridWindow::FeedStatus::Disconnected:
            lbl->setText(QStringLiteral("✕ DISCONNECTED"));
            lbl->setStyleSheet(badge_style(k_disconnected_bg, k_disconnected_fg));
            break;
    }
}

// ── helpers ────────────────────────────────────────────────────────────────

static QString pair_from_ore_key(const std::string& ore_key) {
    std::istringstream ss(ore_key);
    std::string seg;
    int skip = 0;
    std::string qualifier;
    while (std::getline(ss, seg, '/')) {
        if (skip < 2) {
            ++skip;
            continue;
        }
        if (!qualifier.empty())
            qualifier += '/';
        qualifier += seg;
    }
    return qualifier.empty() ? QString::fromStdString(ore_key) : QString::fromStdString(qualifier);
}

// ── FxSpotGridWindow ───────────────────────────────────────────────────────

FxSpotGridWindow::FxSpotGridWindow(ClientManager* clientManager, QWidget* parent)
    : QWidget(parent)
    , clientManager_(clientManager)
    , table_(new QTableWidget(0, ColumnCount, this))
    , staleTimer_(new QTimer(this))
    , loadWatcher_(new QFutureWatcher<LoadResult>(this)) {

    setupUi();
    connect(loadWatcher_,
            &QFutureWatcher<LoadResult>::finished,
            this,
            &FxSpotGridWindow::onLoadFinished);
    connect(staleTimer_, &QTimer::timeout, this, &FxSpotGridWindow::onStaleCheck);
    staleTimer_->start(k_stale_poll_ms);
    reload();
}

void FxSpotGridWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->addWidget(table_);

    table_->setHorizontalHeaderLabels(
        {tr("Currency Pair"), tr("Mid"), tr("24h Chg"), tr("Status")});
    table_->horizontalHeader()->setSectionResizeMode(ColPair, QHeaderView::ResizeToContents);
    table_->horizontalHeader()->setSectionResizeMode(ColMid, QHeaderView::ResizeToContents);
    table_->horizontalHeader()->setSectionResizeMode(ColChange, QHeaderView::ResizeToContents);
    table_->horizontalHeader()->setSectionResizeMode(ColStatus, QHeaderView::Stretch);
    table_->verticalHeader()->hide();
    table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    table_->setSelectionBehavior(QAbstractItemView::SelectRows);
    table_->setShowGrid(false);
    table_->setAlternatingRowColors(true);
    table_->verticalHeader()->setDefaultSectionSize(36);
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
            m::get_feed_bindings_request{.offset = 0, .limit = 1000});
        if (!resp || !resp->success) {
            r.error =
                resp ? QString::fromStdString(resp->message) : QString::fromStdString(resp.error());
            return r;
        }
        for (auto& b : resp->feed_bindings)
            if (b.enabled)
                r.bindings.push_back(std::move(b));
        r.success = true;
        return r;
    };

    loadWatcher_->setFuture(QtConcurrent::run(task));
}

void FxSpotGridWindow::onLoadFinished() {
    auto result = loadWatcher_->result();
    if (!result.success) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to load feed bindings: " << result.error.toStdString();
        emit errorOccurred(result.error);
        return;
    }
    buildRows(result.bindings);
    emit statusChanged(tr("Loaded %1 feed binding(s)").arg(result.bindings.size()));
}

void FxSpotGridWindow::buildRows(const std::vector<marketdata::domain::feed_binding>& bindings) {
    rows_.clear();
    table_->setRowCount(0);

    int row = 0;
    for (const auto& b : bindings) {
        const std::string& ore_key = b.ore_key;
        table_->insertRow(row);

        // Pair
        auto* pairItem = new QTableWidgetItem(pair_from_ore_key(ore_key));
        QFont pf = pairItem->font();
        pf.setBold(true);
        pairItem->setFont(pf);
        pairItem->setTextAlignment(Qt::AlignLeft | Qt::AlignVCenter);
        table_->setItem(row, ColPair, pairItem);

        // Mid
        auto* midItem = new QTableWidgetItem(QStringLiteral("—"));
        midItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
        midItem->setForeground(k_flat_color);
        table_->setItem(row, ColMid, midItem);

        // Change
        auto* chgItem = new QTableWidgetItem(QStringLiteral("—"));
        chgItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
        chgItem->setForeground(k_flat_color);
        table_->setItem(row, ColChange, chgItem);

        // Status badge (widget-in-cell so it renders as a pill)
        auto* container = new QWidget(table_);
        auto* cl = new QVBoxLayout(container);
        cl->setContentsMargins(6, 4, 6, 4);
        auto* badge = make_badge(container);
        apply_badge(badge, FeedStatus::Pending, {});
        cl->addWidget(badge);
        table_->setCellWidget(row, ColStatus, container);

        RowState rs;
        rs.row = row;
        rs.ore_key = ore_key;
        rs.badge = badge;
        rows_.emplace(ore_key, std::move(rs));
        ++row;
    }

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
            clientManager_->currentTenantId(),
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

void FxSpotGridWindow::applyTick(const std::string& ore_key,
                                 double mid,
                                 std::chrono::system_clock::time_point when) {
    auto it = rows_.find(ore_key);
    if (it == rows_.end())
        return;

    RowState& rs = it->second;
    const bool first = !rs.ever_ticked;
    const bool up = mid >= rs.last_mid;

    rs.last_mid = mid;
    rs.last_tick = when;
    rs.ever_ticked = true;

    const QColor dirColor = first ? k_flat_color : (up ? k_up_color : k_down_color);

    // Pair name and mid both follow tick direction.
    if (auto* p = table_->item(rs.row, ColPair))
        p->setForeground(dirColor);

    auto* midItem = table_->item(rs.row, ColMid);
    if (midItem) {
        const QString arrow =
            first ? QString{} : (up ? QStringLiteral("↑ ") : QStringLiteral("↓ "));
        midItem->setText(arrow + QString::number(mid, 'f', 5));
        midItem->setForeground(dirColor);
    }

    if (rs.last_status != FeedStatus::Live) {
        rs.last_status = FeedStatus::Live;
        apply_badge(rs.badge, FeedStatus::Live, when);
    }
}

void FxSpotGridWindow::onStaleCheck() {
    for (auto& [key, rs] : rows_) {
        if (!rs.ever_ticked)
            continue;
        const auto status = deriveStatus(rs);
        // Always repaint STALE so the elapsed seconds counter updates.
        // For other statuses, only repaint on transition.
        if (status == FeedStatus::Stale || status != rs.last_status) {
            rs.last_status = status;
            apply_badge(rs.badge, status, rs.last_tick);
            if (auto* p = table_->item(rs.row, ColPair))
                p->setForeground(pair_color_for_status(status));
        }
    }
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

} // namespace ores::qt
