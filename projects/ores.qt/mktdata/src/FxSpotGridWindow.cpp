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
#include "ores.marketdata.api/domain/asset_class.hpp"
#include "ores.marketdata.api/messaging/feed_binding_protocol.hpp"
#include "ores.qt.headless/FontUtils.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.refdata.client/presentation/currency_pair_rate_formatter.hpp"
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QMetaObject>
#include <QPointer>
#include <QVBoxLayout>
#include <QWidget>
#include <QtConcurrent>
#include <algorithm>
#include <cctype>
#include <chrono>
#include <string_view>

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
static const QColor k_error_color{220, 38, 38};

// ── rate colours ───────────────────────────────────────────────────────────
static const QColor k_up_color{34, 197, 94};   // green-400
static const QColor k_down_color{239, 68, 68}; // red-400
static const QColor k_flat_color{180, 180, 180};

// Currency-pair label colour: fixed, never tick- or status-driven. Per the
// FX Spot Monitor UI/UX audit (Strategy 1), colour is reserved exclusively
// for directional price deltas (Mid/24h Chg) -- painting the whole pair
// label red/green on every tick was the "Christmas tree" fatigue problem.
static const QColor k_pair_label_color{226, 232, 240}; // slate-200

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
        case FxSpotGridWindow::FeedStatus::Error:
            return k_error_color;
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
        case FxSpotGridWindow::FeedStatus::Error:
            return Icon::Error;
    }
    return Icon::FeedPending;
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
            // No text: the green dot alone is the signal. Repeating "LIVE" on every
            // row once the whole grid is connected is exactly the redundant-status
            // noise the UI/UX audit flagged -- Stale/Disconnected/Pending keep their
            // text since those states need explicit attention, not just a colour.
            break;
        case FxSpotGridWindow::FeedStatus::Stale: {
            const auto age = duration_cast<seconds>(system_clock::now() - last_tick).count();
            text = QStringLiteral("STALE: %1s").arg(age);
            break;
        }
        case FxSpotGridWindow::FeedStatus::Disconnected:
            text = QStringLiteral("DISCONNECTED");
            break;
        case FxSpotGridWindow::FeedStatus::Error:
            text = QStringLiteral("ERROR");
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
                                   std::chrono::system_clock::time_point last_tick,
                                   const QString& tooltip = {}) {
    const QColor color = status_color(s);
    const QIcon icon = IconUtils::createRecoloredIcon(status_icon(s), color);
    ind.icon_label->setPixmap(icon.pixmap(16, 16));
    ind.text_label->setStyleSheet(QStringLiteral("color: %1; font-weight: 600;").arg(color.name()));
    update_status_text(ind, s, last_tick);
    ind.icon_label->setToolTip(tooltip);
    ind.text_label->setToolTip(tooltip);
}

// ── helpers ────────────────────────────────────────────────────────────────

// GUI-side derivation from the oresmd fx spot URI (e.g.
// oresmd://fx/eurusd?type=quote&quote=spot), mirroring the oresmd layer's
// to_quote_key -- service-layer code isn't linkable from Qt. The host is the
// base+quote pair concatenated; the wire key stays ORE-shaped ("FX/RATE/...")
// until the identity story's step 4 moves the wire to URIs.
static QString pair_from_uri(const std::string& uri) {
    constexpr std::string_view prefix = "oresmd://fx/";
    if (uri.rfind(prefix, 0) != 0)
        return {};
    const auto pair_start = prefix.size();
    const auto pair_end = uri.find('?', pair_start);
    const auto host = uri.substr(
        pair_start, pair_end == std::string::npos ? std::string::npos : pair_end - pair_start);
    if (host.size() < 2 || host.size() % 2 != 0)
        return QString::fromStdString(host);
    const auto half = host.size() / 2;
    auto base = host.substr(0, half);
    auto quote = host.substr(half);
    std::transform(base.begin(), base.end(), base.begin(), [](unsigned char c) {
        return std::toupper(c);
    });
    std::transform(quote.begin(), quote.end(), quote.begin(), [](unsigned char c) {
        return std::toupper(c);
    });
    return QString::fromStdString(base + "/" + quote);
}

static std::string wire_key_of(const std::string& uri) {
    const auto pair = pair_from_uri(uri);
    if (pair.isEmpty())
        return {};
    return "FX/RATE/" + pair.toStdString();
}

// This window has no separate base/quote columns to put one flag each on
// (just a single "GBP/USD"-style cell), so it needs the composited pair icon
// (see currency_flag_icon() in FlagIconHelper) rather than a single flag.
static QIcon pair_icon_for(ImageCache& imageCache, const QString& pairText) {
    const QStringList parts = pairText.split(QLatin1Char('/'));
    if (parts.size() != 2)
        return {};
    return currency_flag_icon(imageCache, parts[0].toStdString(), parts[1].toStdString());
}

// Resolves pairText's currency_pair_convention -- direct first, then reversed
// (a convention is only ever stored for one direction of a pair) -- mirroring
// crm_rate_display_service::rates()'s own resolution, so this window and the
// CRM matrix derive decimal places/tick size the same way instead of each
// hand-rolling precision rules (see the "Rate display conventions" story).
struct ResolvedConvention {
    std::optional<refdata::domain::currency_pair_convention> convention;
    bool reversed = false;
};

static ResolvedConvention
resolve_convention(refdata::service::cache::currency_pair_convention_cache& cache,
                   const std::string& tenantId,
                   const QString& pairText) {
    const QStringList parts = pairText.split(QLatin1Char('/'));
    if (parts.size() != 2)
        return {};
    const auto base = parts[0].toStdString();
    const auto quote = parts[1].toStdString();
    if (auto direct = cache.lookup(tenantId, base + "/" + quote))
        return {.convention = direct, .reversed = false};
    if (auto reverse = cache.lookup(tenantId, quote + "/" + base))
        return {.convention = reverse, .reversed = true};
    return {};
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
            for (const auto& [uri, rs] : rows_) {
                if (auto* item = table_->item(rs.row, ColPair))
                    item->setIcon(pair_icon_for(*imageCache_, item->text()));
            }
        };
        connect(imageCache_, &ImageCache::imagesLoaded, this, refreshFlags);
        connect(imageCache_, &ImageCache::allLoaded, this, refreshFlags);
    }

    if (clientManager_) {
        // Snapshot synchronously on the GUI thread rather than handing the
        // cache a token_provider that re-reads ClientManager's session state
        // from load()'s own background thread later -- safe here since
        // load() only ever fires once, at construction, mirroring
        // CrmCrossRatesMatrixMdiWindow's identical pattern/reasoning.
        const auto authToken = clientManager_->currentAuthToken();
        conventionCache_ =
            std::make_shared<refdata::service::cache::currency_pair_convention_cache>(
                clientManager_->nats_client(), [authToken](bool /*force*/) { return authToken; });
        QPointer<FxSpotGridWindow> self = this;
        const auto tenantId = clientManager_->currentTenantId();
        auto conventionCache = conventionCache_;
        auto* cacheWatcher = new QFutureWatcher<QString>(this);
        connect(cacheWatcher,
                &QFutureWatcher<QString>::finished,
                this,
                [self, cacheWatcher, tenantId]() {
                    const auto error = cacheWatcher->result();
                    cacheWatcher->deleteLater();
                    if (!self)
                        return;
                    if (!error.isEmpty()) {
                        BOOST_LOG_SEV(lg(), warn) << "Currency pair convention cache load failed: "
                                                  << error.toStdString();
                        return;
                    }
                    // Re-resolve every row already built before the cache finished
                    // loading -- format_rate() falls back to default precision with
                    // no convention, so rows built in that window need their
                    // convention filled in retroactively.
                    for (auto& [key, rs] : self->rows_) {
                        if (auto* item = self->table_->item(rs.row, ColPair)) {
                            const auto resolved =
                                resolve_convention(*self->conventionCache_, tenantId, item->text());
                            rs.convention = resolved.convention;
                            rs.convention_reversed = resolved.reversed;
                        }
                    }
                });
        cacheWatcher->setFuture(QtConcurrent::run([conventionCache, tenantId]() -> QString {
            return QString::fromStdString(conventionCache->load(tenantId));
        }));
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
    // Unlike a single flag (close enough to square that Qt's default square
    // iconSize looks fine), a pair icon is ~2x as wide as tall — without an
    // explicit iconSize it gets squeezed into that square box and looks
    // squished. currency_pair_icon_size()'s default height (16) matches
    // Qt's typical single-icon default, so this reads at the same scale as
    // Currency's single-flag columns, just correctly proportioned for two.
    table_->setIconSize(currency_pair_icon_size());
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
            if (b.enabled && b.asset_class == marketdata::domain::asset_class::fx)
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
        const std::string& uri = b.oresmd_uri;
        table_->insertRow(row);

        // Pair — fixed neutral colour always (see k_pair_label_color's own comment);
        // never recoloured by tick direction or connection status.
        const QString pairText = pair_from_uri(uri);
        auto* pairItem = new QTableWidgetItem(pairText);
        QFont pf = pairItem->font();
        pf.setBold(true);
        pairItem->setFont(pf);
        pairItem->setTextAlignment(Qt::AlignLeft | Qt::AlignVCenter);
        pairItem->setForeground(k_pair_label_color);
        if (imageCache_)
            pairItem->setIcon(pair_icon_for(*imageCache_, pairText));
        table_->setItem(row, ColPair, pairItem);

        // Mid/Change: monospace + right-aligned so decimal points stack vertically
        // for fast scanning (UI/UX audit Strategy 2) -- a proportional font still
        // drifts column-to-column under right-alignment alone since digit widths
        // differ.
        const QFont numFont = FontUtils::monospace();

        // Mid
        auto* midItem = new QTableWidgetItem(QStringLiteral("—"));
        midItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
        midItem->setForeground(k_flat_color);
        midItem->setFont(numFont);
        table_->setItem(row, ColMid, midItem);

        // Change
        auto* chgItem = new QTableWidgetItem(QStringLiteral("—"));
        chgItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
        chgItem->setForeground(k_flat_color);
        chgItem->setFont(numFont);
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
        rs.oresmd_uri = uri;
        if (conventionCache_ && clientManager_) {
            const auto resolved =
                resolve_convention(*conventionCache_, clientManager_->currentTenantId(), pairText);
            rs.convention = resolved.convention;
            rs.convention_reversed = resolved.reversed;
        }
        rs.status_icon_label = indicator.icon_label;
        rs.status_text_label = indicator.text_label;
        rows_.emplace(uri, std::move(rs));
        ++row;
    }

    for (auto& [key, rs] : rows_)
        subscribe(rs);
}

void FxSpotGridWindow::subscribe(RowState& rs) {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    const std::string uri = rs.oresmd_uri;
    const std::string wire_key = wire_key_of(uri);
    if (wire_key.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Skipping subscribe, not an fx spot URI: " << uri;
        return;
    }
    QPointer<FxSpotGridWindow> self = this;

    try {
        rs.subscription = std::make_unique<marketdata::client::fx_spot_subscription>(
            clientManager_->nats_client(),
            wire_key,
            clientManager_->currentTenantId(),
            [self, uri](const marketdata::domain::fx_spot_tick& tick) {
                const double mid = tick.mid;
                const auto when = tick.datetime;
                QMetaObject::invokeMethod(
                    self,
                    [self, uri, mid, when]() {
                        if (self)
                            self->applyTick(uri, mid, when);
                    },
                    Qt::QueuedConnection);
            },
            [self, uri](const std::string& reason) {
                // Not rate-limited here either: a consistently-failing
                // stream (e.g. the wire-format mismatch this callback was
                // added to catch) is a real, actionable problem, not
                // per-tick noise to suppress -- see fx_spot_subscription's
                // own error_handler docstring.
                QMetaObject::invokeMethod(
                    self,
                    [self, uri, reason]() {
                        if (!self)
                            return;
                        // Persistent per-row indicator, not just the transient
                        // status-bar blip errorOccurred() produces below --
                        // under a consistently-failing stream this fires many
                        // times per second, which floods/overwrites the status
                        // bar faster than a user can read it, leaving the row
                        // stuck showing PENDING with no visible explanation.
                        self->applyError(uri, reason);
                        emit self->errorOccurred(tr("Failed to parse FX tick for %1: %2")
                                                     .arg(pair_from_uri(uri),
                                                          QString::fromStdString(reason)));
                    },
                    Qt::QueuedConnection);
            });
        BOOST_LOG_SEV(lg(), debug) << "Subscribed to " << wire_key;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Subscribe failed for " << wire_key << ": " << e.what();
    }
}

void FxSpotGridWindow::applyTick(const std::string& oresmd_uri,
                                 double mid,
                                 std::chrono::system_clock::time_point when) {
    auto it = rows_.find(oresmd_uri);
    if (it == rows_.end())
        return;

    RowState& rs = it->second;
    const bool first = !rs.ever_ticked;
    const bool up = mid >= rs.last_mid;

    rs.last_mid = mid;
    rs.last_tick = when;
    rs.ever_ticked = true;

    const QColor dirColor = first ? k_flat_color : (up ? k_up_color : k_down_color);

    // Only the Mid value carries directional colour -- the pair label stays
    // neutral (k_pair_label_color, set once in buildRows and never touched here).
    auto* midItem = table_->item(rs.row, ColMid);
    if (midItem) {
        const QString arrow =
            first ? QString{} : (up ? QStringLiteral("↑ ") : QStringLiteral("↓ "));
        const auto rateText =
            refdata::client::presentation::currency_pair_rate_formatter::format_rate(
                mid, rs.convention, rs.convention_reversed);
        midItem->setText(arrow + QString::fromStdString(rateText));
        midItem->setForeground(dirColor);
    }

    if (rs.last_status != FeedStatus::Live) {
        rs.last_status = FeedStatus::Live;
        apply_status_indicator(
            {rs.status_icon_label, rs.status_text_label}, FeedStatus::Live, when);
    }
}

void FxSpotGridWindow::applyError(const std::string& oresmd_uri, const std::string& reason) {
    auto it = rows_.find(oresmd_uri);
    if (it == rows_.end())
        return;

    RowState& rs = it->second;
    rs.last_error = QString::fromStdString(reason);
    // Guard, matching applyTick()'s pattern: errors are deliberately not
    // rate-limited upstream (a consistently-failing stream can call this
    // many times per second), so avoid regenerating the status icon and
    // repainting the stylesheet/tooltip on every single failure once
    // already showing Error.
    if (rs.last_status != FeedStatus::Error) {
        rs.last_status = FeedStatus::Error;
        apply_status_indicator({rs.status_icon_label, rs.status_text_label},
                               FeedStatus::Error,
                               rs.last_tick,
                               tr("Failed to decode tick: %1").arg(rs.last_error));
    }
}

void FxSpotGridWindow::onStaleCheck() {
    for (auto& [key, rs] : rows_) {
        // A row currently showing Error must not be silently reclaimed by
        // the generic Stale/Disconnected transition below: applyError()
        // doesn't touch last_tick, so once its age crosses the
        // live/stale thresholds this loop would otherwise overwrite the
        // decode-error tooltip with an empty one, erasing the very
        // explanation this status exists to show. The Error state is
        // cleared only by a subsequent successful tick (applyTick()
        // unconditionally repaints to Live).
        if (!rs.ever_ticked || rs.last_status == FeedStatus::Error)
            continue;
        const auto status = deriveStatus(rs);
        const StatusIndicator indicator{rs.status_icon_label, rs.status_text_label};
        if (status != rs.last_status) {
            // Transition: icon/colour actually changed, full repaint. Pair label
            // itself stays k_pair_label_color regardless -- connection status is
            // the status indicator's job, not another thing tinting the ticker text.
            rs.last_status = status;
            apply_status_indicator(indicator, status, rs.last_tick);
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
