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
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ImageCache.hpp"
#include <QHBoxLayout>
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

// ── status colours ─────────────────────────────────────────────────────────
static const QColor k_pending_color{140, 140, 140};
static const QColor k_live_color{22, 163, 74};
static const QColor k_stale_color{200, 140, 0};
static const QColor k_disconnected_color{220, 38, 38};

// ── rate colours ───────────────────────────────────────────────────────────
static const QColor k_up_color{34, 197, 94};   // green-400
static const QColor k_down_color{239, 68, 68}; // red-400
static const QColor k_flat_color{180, 180, 180};

static QColor status_color(FxSpotGridWindow::FeedStatus s) {
    switch (s) {
        case FxSpotGridWindow::FeedStatus::Live:
            return k_live_color;
        case FxSpotGridWindow::FeedStatus::Stale:
            return k_stale_color;
        case FxSpotGridWindow::FeedStatus::Disconnected:
            return k_disconnected_color;
        case FxSpotGridWindow::FeedStatus::Pending:
            return k_pending_color;
    }
    return k_pending_color;
}

static Icon status_icon(FxSpotGridWindow::FeedStatus s) {
    switch (s) {
        case FxSpotGridWindow::FeedStatus::Live:
            return Icon::FeedLive;
        case FxSpotGridWindow::FeedStatus::Stale:
            return Icon::FeedStale;
        case FxSpotGridWindow::FeedStatus::Disconnected:
            return Icon::PlugDisconnected;
        case FxSpotGridWindow::FeedStatus::Pending:
            return Icon::FeedPending;
    }
    return Icon::FeedPending;
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

namespace {

// Icon + short text label inline in the Status column — no pill/badge
// background. The icon *shape* differs per state (not just its color),
// so status doesn't rely on color alone.
struct StatusIndicator {
    QLabel* icon_label = nullptr;
    QLabel* text_label = nullptr;
};

}

static StatusIndicator make_status_indicator(QWidget* parent) {
    StatusIndicator ind;
    ind.icon_label = new QLabel(parent);
    ind.icon_label->setFixedSize(16, 16);
    ind.text_label = new QLabel(parent);
    QFont f = ind.text_label->font();
    f.setPointSizeF(f.pointSizeF() - 1);
    ind.text_label->setFont(f);
    return ind;
}

// Text only — cheap enough to call on every stale-poll tick (every
// k_stale_poll_ms) to refresh the "STALE: Ns" counter without re-rendering
// the (unchanged) icon/colour.
static void update_status_text(const StatusIndicator& ind,
                               FxSpotGridWindow::FeedStatus s,
                               std::chrono::system_clock::time_point last_tick) {
    using namespace std::chrono;

    QString text;
    switch (s) {
        case FxSpotGridWindow::FeedStatus::Pending:
            text = QStringLiteral("PENDING");
            break;
        case FxSpotGridWindow::FeedStatus::Live:
            text = QStringLiteral("LIVE");
            break;
        case FxSpotGridWindow::FeedStatus::Stale: {
            const auto age = duration_cast<seconds>(system_clock::now() - last_tick).count();
            text = QStringLiteral("STALE: %1s").arg(age);
            break;
        }
        case FxSpotGridWindow::FeedStatus::Disconnected:
            text = QStringLiteral("DISCONNECTED");
            break;
    }
    ind.text_label->setText(text);
}

// Full repaint (icon + colour + text) — call only on a status transition.
// IconUtils::createRecoloredIcon rasterizes the SVG at six sizes, so this
// is too expensive to run on every stale-poll tick for rows that stay in
// the same status between polls.
static void apply_status_indicator(const StatusIndicator& ind,
                                   FxSpotGridWindow::FeedStatus s,
                                   std::chrono::system_clock::time_point last_tick) {
    const QColor color = status_color(s);
    const QIcon icon = IconUtils::createRecoloredIcon(status_icon(s), color);
    ind.icon_label->setPixmap(icon.pixmap(16, 16));
    ind.text_label->setStyleSheet(QStringLiteral("color: %1; font-weight: 600;").arg(color.name()));
    update_status_text(ind, s, last_tick);
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

// This window has no separate base/quote columns to put one flag each on
// (just a single "GBP/USD"-style cell), so it needs the composited pair icon
// (see pair_flag_icon() in FlagIconHelper) rather than a single flag.
static QIcon pair_icon_for(ImageCache& imageCache, const QString& pairText) {
    const QStringList parts = pairText.split(QLatin1Char('/'));
    if (parts.size() != 2)
        return {};
    return pair_flag_icon(imageCache, parts[0].toStdString(), parts[1].toStdString());
}

// ── FxSpotGridWindow ───────────────────────────────────────────────────────

FxSpotGridWindow::FxSpotGridWindow(ClientManager* clientManager,
                                   ImageCache* imageCache,
                                   QWidget* parent)
    : QWidget(parent)
    , clientManager_(clientManager)
    , imageCache_(imageCache)
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

    // Flags may still be loading (async) when a row is first built — re-apply
    // once ImageCache actually has them rather than leaving rows stuck with
    // whatever placeholder was available at buildRows() time.
    if (imageCache_) {
        const auto refreshFlags = [this]() {
            if (!imageCache_)
                return;
            for (const auto& [ore_key, rs] : rows_) {
                if (auto* item = table_->item(rs.row, ColPair))
                    item->setIcon(pair_icon_for(*imageCache_, item->text()));
            }
        };
        connect(imageCache_, &ImageCache::imagesLoaded, this, refreshFlags);
        connect(imageCache_, &ImageCache::allLoaded, this, refreshFlags);
    }

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
    // Qt's default view iconSize (~16-24px) would otherwise downscale the
    // composited flag pixmap regardless of its actual size.
    table_->setIconSize(pair_flag_icon_size());
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
        const QString pairText = pair_from_ore_key(ore_key);
        auto* pairItem = new QTableWidgetItem(pairText);
        QFont pf = pairItem->font();
        pf.setBold(true);
        pairItem->setFont(pf);
        pairItem->setTextAlignment(Qt::AlignLeft | Qt::AlignVCenter);
        if (imageCache_)
            pairItem->setIcon(pair_icon_for(*imageCache_, pairText));
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

        // Status indicator: icon + text inline, no pill background.
        auto* container = new QWidget(table_);
        auto* cl = new QHBoxLayout(container);
        cl->setContentsMargins(6, 4, 6, 4);
        cl->setSpacing(6);
        auto indicator = make_status_indicator(container);
        apply_status_indicator(indicator, FeedStatus::Pending, {});
        cl->addWidget(indicator.icon_label);
        cl->addWidget(indicator.text_label);
        cl->addStretch();
        table_->setCellWidget(row, ColStatus, container);

        RowState rs;
        rs.row = row;
        rs.ore_key = ore_key;
        rs.status_icon_label = indicator.icon_label;
        rs.status_text_label = indicator.text_label;
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
        apply_status_indicator(
            {rs.status_icon_label, rs.status_text_label}, FeedStatus::Live, when);
    }
}

void FxSpotGridWindow::onStaleCheck() {
    for (auto& [key, rs] : rows_) {
        if (!rs.ever_ticked)
            continue;
        const auto status = deriveStatus(rs);
        const StatusIndicator indicator{rs.status_icon_label, rs.status_text_label};
        if (status != rs.last_status) {
            // Transition: icon/colour actually changed, full repaint.
            rs.last_status = status;
            apply_status_indicator(indicator, status, rs.last_tick);
            if (auto* p = table_->item(rs.row, ColPair))
                p->setForeground(pair_color_for_status(status));
        } else if (status == FeedStatus::Stale) {
            // Same status, still stale: only the elapsed-seconds text
            // changes — skip re-rendering the (unchanged) icon.
            update_status_text(indicator, status, rs.last_tick);
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
