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
#include "ores.qt/MarketSimulatorWindow.hpp"
#include "ores.marketdata.api/domain/feed_binding.hpp"
#include "ores.marketdata.api/domain/fx_spot_tick.hpp"
#include "ores.marketdata.api/messaging/feed_binding_protocol.hpp"
#include "ores.marketdata.api/messaging/market_feed_config_protocol.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/FeedDialog.hpp"
#include "ores.qt/FxSpotRateEditor.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/WatermarkChartView.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
#include "ores.synthetic.api/messaging/market_data_generation_config_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include <QColor>
#include <QFont>
#include <QFontDatabase>
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QIcon>
#include <QMdiArea>
#include <QMessageBox>
#include <QPainter>
#include <QPainterPath>
#include <QPalette>
#include <QPixmap>
#include <QPointF>
#include <QPointer>
#include <QStackedLayout>
#include <QVBoxLayout>
#include <QtCharts/QChart>
#include <QtCharts/QChartView>
#include <QtCharts/QLineSeries>
#include <QtCharts/QScatterSeries>
#include <QtCharts/QValueAxis>
#include <QtConcurrent>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <atomic>
#include <cmath>
#include <memory>
#include <rfl/json.hpp>
#include <set>

namespace ores::qt {

using namespace ores::logging;

namespace {

// Tree item roles.
constexpr int NodeTypeRole = Qt::UserRole + 1;
constexpr int NodeIdRole = Qt::UserRole + 2;

boost::uuids::uuid uuid_from_string(const std::string& s) {
    try {
        return boost::lexical_cast<boost::uuids::uuid>(s);
    } catch (...) {
        return boost::uuids::uuid{};
    }
}

// Remove every row from a QFormLayout (deleting the field/label widgets).
void clear_form(QFormLayout* form) {
    while (form->rowCount() > 0)
        form->removeRow(0);
}

// Clip a flag pixmap into a circle of the given diameter (device-independent
// pixels), scaling to cover so the circle is fully filled.
QPixmap circular_flag(const QPixmap& src, int diameter, qreal dpr) {
    QPixmap canvas(QSize(diameter, diameter) * dpr);
    canvas.setDevicePixelRatio(dpr);
    canvas.fill(Qt::transparent);

    QPainter p(&canvas);
    p.setRenderHint(QPainter::Antialiasing);
    p.setRenderHint(QPainter::SmoothPixmapTransform);

    QPainterPath clip;
    clip.addEllipse(0, 0, diameter, diameter);
    p.setClipPath(clip);

    QPixmap scaled = src.scaled(
        QSize(diameter, diameter) * dpr, Qt::KeepAspectRatioByExpanding, Qt::SmoothTransformation);
    scaled.setDevicePixelRatio(dpr);
    const qreal x = (diameter - scaled.width() / dpr) / 2.0;
    const qreal y = (diameter - scaled.height() / dpr) / 2.0;
    p.drawPixmap(QPointF(x, y), scaled);
    return canvas;
}

// Compose a TradingView-style overlapping pair badge: the base-currency flag
// sits front/lower-left over the quote-currency flag behind/upper-right, with
// a separator ring around the front flag.
QPixmap
pair_flags_badge(const QPixmap& base, const QPixmap& quote, const QColor& ringColour, qreal dpr) {
    constexpr int d = 56;   // flag diameter
    constexpr int off = 30; // overlap offset
    constexpr int m = 3;    // separator ring width
    const int total = d + off + 2 * m;

    QPixmap canvas(QSize(total, total) * dpr);
    canvas.setDevicePixelRatio(dpr);
    canvas.fill(Qt::transparent);

    QPainter p(&canvas);
    p.setRenderHint(QPainter::Antialiasing);

    // Quote flag behind, top-right.
    p.drawPixmap(QPointF(m + off, m), circular_flag(quote, d, dpr));

    // Separator ring behind the front (base) flag.
    p.setPen(Qt::NoPen);
    p.setBrush(ringColour);
    p.drawEllipse(QPointF(m + d / 2.0, m + off + d / 2.0), d / 2.0 + m, d / 2.0 + m);

    // Base flag front, lower-left.
    p.drawPixmap(QPointF(m, m + off), circular_flag(base, d, dpr));
    return canvas;
}

}

MarketSimulatorWindow::MarketSimulatorWindow(ClientManager* clientManager,
                                             const QString& username,
                                             ImageCache* imageCache,
                                             ChangeReasonCache* changeReasonCache,
                                             QWidget* parent)
    : QWidget(parent)
    , clientManager_(clientManager)
    , username_(username)
    , imageCache_(imageCache)
    , changeReasonCache_(changeReasonCache)
    , mainSplitter_(new QSplitter(Qt::Horizontal, this))
    , toolbar_(new QToolBar(this))
    , feedsTree_(new QTreeView(this))
    , treeModel_(new QStandardItemModel(this))
    , emptyHintLabel_(new QLabel(this))
    , statusLabel_(new QLabel(this))
    , summaryStack_(new QStackedWidget(this)) {

    BOOST_LOG_SEV(lg(), debug) << "Creating Market Simulator window";

    setupUi();
    setupToolbar();
    setupLeftPanel();
    setupRightPanel();
    setupConnections();

    updateToolbarState();
    reload();
}

void MarketSimulatorWindow::setupUi() {
    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(0, 0, 0, 0);
    mainLayout->setSpacing(0);

    mainLayout->addWidget(toolbar_);
    mainLayout->addWidget(mainSplitter_, 1);

    statusLabel_->setContentsMargins(6, 2, 6, 4);
    mainLayout->addWidget(statusLabel_);

    mainSplitter_->setHandleWidth(1);
    mainSplitter_->setChildrenCollapsible(false);
}

void MarketSimulatorWindow::setupToolbar() {
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowSync, IconUtils::DefaultIconColor), tr("Reload"));
    reloadAction_->setToolTip(tr("Reload all feeds and FX rates"));

    toolbar_->addSeparator();

    newFeedAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor), tr("New Feed"));
    newFeedAction_->setToolTip(
        tr("New feed — a named market-data simulation you can enable and run."));

    newFxRateAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor), tr("New FX Rate"));
    newFxRateAction_->setToolTip(
        tr("New FX rate — a currency pair to simulate (e.g. EUR/USD) and its price "
           "model; belongs to the selected feed."));

    toolbar_->addSeparator();

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Edit, IconUtils::DefaultIconColor), tr("Edit"));
    editAction_->setToolTip(tr("Edit the selected item."));

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor), tr("Delete"));
    deleteAction_->setToolTip(tr("Delete the selected entity"));

    toolbar_->addSeparator();

    startFeedAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::PlayFilled, IconUtils::DefaultIconColor), tr("Start"));
    startFeedAction_->setToolTip(tr("Start generating live ticks for the selected FX rate(s)."));

    stopFeedAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::StopFilled, IconUtils::DefaultIconColor), tr("Stop"));
    stopFeedAction_->setToolTip(tr("Stop generating ticks for the selected FX rate(s)."));

    toolbar_->addSeparator();

    startAllAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::PlayFilled, IconUtils::DefaultIconColor),
        tr("Start all"));
    startAllAction_->setToolTip(tr("Start generating live ticks for all FX rates."));

    stopAllAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::StopFilled, IconUtils::DefaultIconColor),
        tr("Stop all"));
    stopAllAction_->setToolTip(tr("Stop generating ticks for all FX rates."));
}

void MarketSimulatorWindow::setupLeftPanel() {
    auto* leftPanel = new QWidget(this);
    auto* leftLayout = new QVBoxLayout(leftPanel);
    leftLayout->setContentsMargins(0, 0, 0, 0);
    leftLayout->setSpacing(8);

    auto* label = new QLabel(tr("<b>Market Data Feeds</b>"), leftPanel);
    leftLayout->addWidget(label);

    feedsTree_->setModel(treeModel_);
    feedsTree_->setHeaderHidden(true);
    feedsTree_->setIconSize(QSize(44, 22)); // room for the two-flag pair icon
    feedsTree_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    feedsTree_->setSelectionMode(QAbstractItemView::ExtendedSelection);
    feedsTree_->setFrameShape(QFrame::StyledPanel);
    feedsTree_->setFrameShadow(QFrame::Sunken);
    leftLayout->addWidget(feedsTree_, 1);

    emptyHintLabel_->setWordWrap(true);
    emptyHintLabel_->setText(tr("No feeds yet. Click 'New Feed' to define a market-data feed, then "
                                "add an FX rate (currency pair) and its price model."));
    emptyHintLabel_->setStyleSheet("color: gray;");
    emptyHintLabel_->setContentsMargins(4, 0, 4, 4);
    leftLayout->addWidget(emptyHintLabel_);

    leftPanel->setMinimumWidth(300);
    leftPanel->setMaximumWidth(480);

    mainSplitter_->addWidget(leftPanel);
}

void MarketSimulatorWindow::setupRightPanel() {
    // Empty/default page.
    auto* emptyPage = new QWidget(this);
    auto* emptyLayout = new QVBoxLayout(emptyPage);
    auto* emptyLabel = new QLabel(tr("Select a feed or FX rate to see its details."), emptyPage);
    emptyLabel->setAlignment(Qt::AlignCenter);
    emptyLabel->setStyleSheet("color: gray;");
    emptyLayout->addWidget(emptyLabel);

    // Read-only summary page.
    summaryPage_ = new QWidget(this);
    auto* summaryLayout = new QVBoxLayout(summaryPage_);

    // TradingView-style hero header (shown for FX pairs only): big overlapping
    // circular flags next to a bold pair title.
    summaryHero_ = new QWidget(summaryPage_);
    auto* heroLayout = new QHBoxLayout(summaryHero_);
    heroLayout->setContentsMargins(0, 4, 0, 12);
    heroLayout->setSpacing(16);
    heroFlags_ = new QLabel(summaryHero_);
    heroFlags_->setAlignment(Qt::AlignVCenter | Qt::AlignLeft);
    heroLayout->addWidget(heroFlags_, 0, Qt::AlignVCenter);
    auto* heroText = new QWidget(summaryHero_);
    auto* heroTextLayout = new QVBoxLayout(heroText);
    heroTextLayout->setContentsMargins(0, 0, 0, 0);
    heroTextLayout->setSpacing(2);
    heroTextLayout->addStretch(1);
    heroTitle_ = new QLabel(heroText);
    {
        QFont f = heroTitle_->font();
        f.setPointSize(f.pointSize() + 14);
        f.setBold(true);
        heroTitle_->setFont(f);
    }
    heroSubtitle_ = new QLabel(heroText);
    heroSubtitle_->setStyleSheet("color: gray;");
    heroStatus_ = new QLabel(heroText);
    heroTextLayout->addWidget(heroTitle_);
    heroTextLayout->addWidget(heroSubtitle_);
    heroTextLayout->addWidget(heroStatus_);
    heroTextLayout->addStretch(1);
    heroLayout->addWidget(heroText, 1);
    summaryLayout->addWidget(summaryHero_);
    summaryHero_->setVisible(false);

    summaryTitle_ = new QLabel(summaryPage_);
    summaryLayout->addWidget(summaryTitle_);
    summaryForm_ = new QFormLayout();
    summaryLayout->addLayout(summaryForm_);

    // Feed-level start/stop buttons, shown only when a Feed node is selected.
    feedStatsLabel_ = new QLabel(summaryPage_);
    feedStatsLabel_->setStyleSheet("color: gray;");
    summaryLayout->addWidget(feedStatsLabel_);

    auto* feedBtnRow = new QWidget(summaryPage_);
    auto* feedBtnLayout = new QHBoxLayout(feedBtnRow);
    feedBtnLayout->setContentsMargins(0, 4, 0, 0);
    feedStartButton_ =
        new QPushButton(IconUtils::createRecoloredIcon(Icon::Play, IconUtils::DefaultIconColor),
                        tr("Start all rates"),
                        feedBtnRow);
    feedStopButton_ =
        new QPushButton(IconUtils::createRecoloredIcon(Icon::Stop, IconUtils::DefaultIconColor),
                        tr("Stop all rates"),
                        feedBtnRow);
    feedBtnLayout->addWidget(feedStartButton_);
    feedBtnLayout->addWidget(feedStopButton_);
    feedBtnLayout->addStretch();
    summaryLayout->addWidget(feedBtnRow);
    feedBtnRow->setVisible(false); // shown only for Feed nodes
    feedStatsLabel_->setVisible(false);
    // Store the row widget pointer so we can toggle it; reuse feedStopButton_ parent.
    feedStartButton_->setProperty("feedBtnRow",
                                  QVariant::fromValue(static_cast<QObject*>(feedBtnRow)));

    connect(feedStartButton_, &QPushButton::clicked, this, [this]() {
        startPairsAsync(fxPairsForFeed(currentNodeId()));
    });
    connect(feedStopButton_, &QPushButton::clicked, this, [this]() {
        stopPairsAsync(fxPairsForFeed(currentNodeId()));
    });

    // Tick chart — shows live synthetic ticks when an FX pair is selected and running.
    {
        const QColor upGreen(0x22, 0xC5, 0x5E);
        const QColor grid(255, 255, 255, 18);
        const QColor plotBg(0x12, 0x17, 0x1F);
        const QColor labelColor(0xCB, 0xD5, 0xE1);
        QFont labelFont = QFontDatabase::hasFamily("Inter") ? QFont("Inter") : font();
        labelFont.setPointSizeF(8.0);

        tickSeries_ = new QLineSeries();
        tickSeries_->setPointsVisible(false);
        {
            QPen pen(upGreen);
            pen.setWidthF(2.0);
            pen.setCosmetic(true);
            tickSeries_->setPen(pen);
        }

        tickPosMarker_ = new QScatterSeries();
        tickPosMarker_->setMarkerShape(QScatterSeries::MarkerShapeCircle);
        tickPosMarker_->setColor(upGreen);
        tickPosMarker_->setBorderColor(QColor(0xE2, 0xF5, 0xEA));
        tickPosMarker_->setMarkerSize(9.0);

        tickAxisX_ = new QValueAxis();
        tickAxisX_->setRange(0, 1); // resized by refreshTickChart() as data arrives
        tickAxisX_->setLabelFormat("%d");
        tickAxisX_->setGridLineColor(grid);
        tickAxisX_->setMinorGridLineVisible(false);
        tickAxisX_->setLabelsColor(labelColor);
        tickAxisX_->setLabelsFont(labelFont);
        tickAxisX_->setLinePenColor(grid);
        tickAxisX_->setTickCount(7);

        tickAxisY_ = new QValueAxis();
        tickAxisY_->setLabelFormat("%.5f");
        tickAxisY_->setGridLineColor(grid);
        tickAxisY_->setMinorGridLineVisible(false);
        tickAxisY_->setLabelsColor(labelColor);
        tickAxisY_->setLabelsFont(labelFont);
        tickAxisY_->setLinePenColor(grid);

        auto* chart = new QChart();
        chart->setTheme(QChart::ChartThemeDark);
        chart->setBackgroundRoundness(0);
        chart->legend()->setVisible(false);
        chart->setMargins(QMargins(8, 8, 8, 8));
        chart->setPlotAreaBackgroundBrush(plotBg);
        chart->setPlotAreaBackgroundVisible(true);
        chart->addSeries(tickSeries_);
        chart->addSeries(tickPosMarker_);
        chart->addAxis(tickAxisX_, Qt::AlignBottom);
        chart->addAxis(tickAxisY_, Qt::AlignRight);
        tickSeries_->attachAxis(tickAxisX_);
        tickSeries_->attachAxis(tickAxisY_);
        tickPosMarker_->attachAxis(tickAxisX_);
        tickPosMarker_->attachAxis(tickAxisY_);

        tickChartContainer_ = new QWidget(summaryPage_);
        auto* tickLayout = new QVBoxLayout(tickChartContainer_);
        tickLayout->setContentsMargins(0, 0, 0, 0);

        tickChartView_ = new WatermarkChartView(chart, tickChartContainer_, {});
        tickChartView_->setRenderHint(QPainter::Antialiasing);
        tickChartView_->setMinimumHeight(200);
        tickLayout->addWidget(tickChartView_);

        // "Feed not running" overlay: child of tickChartView_ so it appears on top.
        tickChartPlaceholder_ = new QLabel(tr("Feed not running"), tickChartView_);
        tickChartPlaceholder_->setAlignment(Qt::AlignCenter);
        tickChartPlaceholder_->setStyleSheet(
            "color: rgba(203,213,225,180); font-size: 14px; background: transparent;");
        tickChartPlaceholder_->setAttribute(Qt::WA_TransparentForMouseEvents);
        tickChartPlaceholder_->setGeometry(tickChartView_->rect());
        tickChartPlaceholder_->raise();

        summaryLayout->addWidget(tickChartContainer_, 1);
        tickChartContainer_->setVisible(false);

        tickFlashTimer_ = new QTimer(this);
        tickFlashTimer_->setInterval(550);
        connect(tickFlashTimer_, &QTimer::timeout, this, &MarketSimulatorWindow::onTickChartFlash);
    }

    summaryStack_->addWidget(emptyPage);    // index 0
    summaryStack_->addWidget(summaryPage_); // index 1

    auto* rightPanel = new QWidget(this);
    auto* rightLayout = new QVBoxLayout(rightPanel);
    rightLayout->setContentsMargins(8, 8, 8, 8);
    auto* heading = new QLabel(tr("<b>Details</b>"), rightPanel);
    rightLayout->addWidget(heading);
    rightLayout->addWidget(summaryStack_, 1);

    mainSplitter_->addWidget(rightPanel);
    mainSplitter_->setSizes({380, 1120});
}

void MarketSimulatorWindow::setupConnections() {
    connect(feedsTree_->selectionModel(),
            &QItemSelectionModel::currentChanged,
            this,
            &MarketSimulatorWindow::onTreeSelectionChanged);
    connect(feedsTree_->selectionModel(),
            &QItemSelectionModel::selectionChanged,
            this,
            [this](const QItemSelection&, const QItemSelection&) { updateToolbarState(); });
    connect(
        feedsTree_, &QTreeView::doubleClicked, this, &MarketSimulatorWindow::onTreeDoubleClicked);

    connect(reloadAction_, &QAction::triggered, this, &MarketSimulatorWindow::onReloadClicked);
    connect(newFeedAction_, &QAction::triggered, this, &MarketSimulatorWindow::onNewFeedClicked);
    connect(
        newFxRateAction_, &QAction::triggered, this, &MarketSimulatorWindow::onNewFxRateClicked);
    connect(editAction_, &QAction::triggered, this, &MarketSimulatorWindow::onEditClicked);
    connect(deleteAction_, &QAction::triggered, this, &MarketSimulatorWindow::onDeleteClicked);
    connect(
        startFeedAction_, &QAction::triggered, this, &MarketSimulatorWindow::onStartFeedClicked);
    connect(stopFeedAction_, &QAction::triggered, this, &MarketSimulatorWindow::onStopFeedClicked);
    connect(startAllAction_, &QAction::triggered, this, &MarketSimulatorWindow::onStartAllClicked);
    connect(stopAllAction_, &QAction::triggered, this, &MarketSimulatorWindow::onStopAllClicked);
}

void MarketSimulatorWindow::onReloadClicked() {
    reload();
}

void MarketSimulatorWindow::reload() {
    if (loading_) {
        BOOST_LOG_SEV(lg(), warn) << "Reload already in progress.";
        return;
    }
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorOccurred(tr("Not connected to server"));
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Reloading feeds, fx rates and components.";

    loading_ = true;
    statusLabel_->setText(tr("Loading..."));

    struct FetchResult {
        bool success = false;
        std::vector<synthetic::domain::market_data_generation_config> feeds;
        std::vector<synthetic::domain::fx_spot_generation_config> fxPairs;
        std::vector<synthetic::domain::gmm_component> components;
        std::unordered_map<std::string, std::string> currencyNames;
        std::vector<std::string> runningSourceNames;
        QString error;
    };

    QPointer<MarketSimulatorWindow> self = this;
    auto* cm = clientManager_;

    auto task = [cm]() -> FetchResult {
        namespace m = synthetic::messaging;
        FetchResult r;

        auto feedsResp = cm->process_authenticated_request(
            m::get_market_data_generation_configs_request{.offset = 0, .limit = 1000});
        if (!feedsResp) {
            r.error = QString::fromStdString(feedsResp.error());
            return r;
        }
        r.feeds = std::move(feedsResp->market_data_generation_configs);

        auto fxResp = cm->process_authenticated_request(
            m::get_fx_spot_generation_configs_request{.offset = 0, .limit = 1000});
        if (!fxResp) {
            r.error = QString::fromStdString(fxResp.error());
            return r;
        }
        r.fxPairs = std::move(fxResp->fx_spot_generation_configs);

        auto compResp = cm->process_authenticated_request(
            m::get_gmm_components_request{.offset = 0, .limit = 1000});
        if (!compResp) {
            r.error = QString::fromStdString(compResp.error());
            return r;
        }
        r.components = std::move(compResp->gmm_components);

        // Currency display names (best-effort; an empty map just falls back to
        // ISO codes in the hero title).
        r.currencyNames = fetch_currency_names(cm);

        // Query which feeds the synthetic service currently has running so the
        // UI shows correct Running/Stopped status even after a restart.
        auto listResp = cm->process_authenticated_request(
            ores::marketdata::messaging::list_market_feed_configs_request{});
        if (listResp && listResp->success)
            r.runningSourceNames = std::move(listResp->running_source_names);

        r.success = true;
        return r;
    };

    auto* watcher = new QFutureWatcher<FetchResult>(self);
    connect(watcher, &QFutureWatcher<FetchResult>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;

        self->loading_ = false;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error) << "Reload failed: " << result.error.toStdString();
            self->statusLabel_->setText(self->tr("Error: %1").arg(result.error));
            emit self->errorOccurred(result.error);
            return;
        }

        self->feeds_.clear();
        self->fxPairs_.clear();
        self->components_.clear();
        self->currencyNames_ = std::move(result.currencyNames);
        self->runningSourceNames_ = {result.runningSourceNames.begin(),
                                     result.runningSourceNames.end()};

        for (auto& f : result.feeds)
            self->feeds_[boost::uuids::to_string(f.id)] = std::move(f);
        for (auto& fx : result.fxPairs)
            self->fxPairs_[boost::uuids::to_string(fx.id)] = std::move(fx);
        for (auto& c : result.components)
            self->components_[boost::uuids::to_string(c.id)] = std::move(c);

        BOOST_LOG_SEV(lg(), info) << "Loaded " << self->feeds_.size() << " feeds, "
                                  << self->fxPairs_.size() << " fx pairs, "
                                  << self->components_.size() << " components.";

        self->buildTree();
        self->updateStatusCounts();
        self->updateEmptyState();
        self->clearSummary();
        self->updateToolbarState();
    });

    watcher->setFuture(QtConcurrent::run(task));
}

static QString feedItemText(const synthetic::domain::market_data_generation_config& feed) {
    QString text = QString::fromStdString(feed.name);
    if (!feed.enabled)
        text += QStringLiteral(" (disabled)");
    return text;
}

// Returns the coloured status icon for a feed given its running/total counts.
static QIcon feedStatusIcon(int running, int total) {
    if (running == 0)
        return IconUtils::createRecoloredIcon(Icon::PauseCircleFilled, QColor(140, 140, 140));
    if (running == total)
        return IconUtils::createRecoloredIcon(Icon::CheckmarkCircleFilled, QColor(60, 180, 80));
    // Partially running
    return IconUtils::createRecoloredIcon(Icon::CheckmarkCircleFilled, QColor(200, 160, 40));
}

// Returns the coloured status icon for a single FX pair.
static QIcon fxPairStatusIcon(bool running) {
    if (running)
        return IconUtils::createRecoloredIcon(Icon::CheckmarkCircleFilled, QColor(60, 180, 80));
    return IconUtils::createRecoloredIcon(Icon::PauseCircleFilled, QColor(140, 140, 140));
}

void MarketSimulatorWindow::buildTree() {
    treeModel_->clear();
    treeModel_->setColumnCount(2);
    auto* root = treeModel_->invisibleRootItem();

    for (const auto& [feedId, feed] : feeds_) {
        int total = 0, running = 0;
        for (const auto& [fxId, fx] : fxPairs_) {
            if (boost::uuids::to_string(fx.config_id) != feedId)
                continue;
            ++total;
            if (runningSourceNames_.count(fx.source_name))
                ++running;
        }

        auto* feedItem = new QStandardItem(feedItemText(feed));
        feedItem->setData(static_cast<int>(NodeType::Feed), NodeTypeRole);
        feedItem->setData(QString::fromStdString(feedId), NodeIdRole);
        feedItem->setIcon(
            IconUtils::createRecoloredIcon(Icon::Folder, IconUtils::DefaultIconColor));

        auto* feedStatus = new QStandardItem();
        feedStatus->setIcon(feedStatusIcon(running, total));
        feedStatus->setData(static_cast<int>(NodeType::Feed), NodeTypeRole);
        feedStatus->setData(QString::fromStdString(feedId), NodeIdRole);

        for (const auto& [fxId, fx] : fxPairs_) {
            if (boost::uuids::to_string(fx.config_id) != feedId)
                continue;

            const bool fxRunning = runningSourceNames_.count(fx.source_name) > 0;
            QString fxText =
                QString::fromStdString(fx.base_currency_code + "/" + fx.quote_currency_code);
            auto* fxItem = new QStandardItem(fxText);
            fxItem->setData(static_cast<int>(NodeType::FxPair), NodeTypeRole);
            fxItem->setData(QString::fromStdString(fxId), NodeIdRole);
            if (imageCache_) {
                const QPixmap basePm =
                    imageCache_->getCurrencyFlagIcon(fx.base_currency_code).pixmap(22, 22);
                const QPixmap quotePm =
                    imageCache_->getCurrencyFlagIcon(fx.quote_currency_code).pixmap(22, 22);
                QPixmap combined(48, 22);
                combined.fill(Qt::transparent);
                QPainter painter(&combined);
                painter.drawPixmap(0, 0, basePm);
                painter.drawPixmap(26, 0, quotePm);
                painter.end();
                fxItem->setIcon(QIcon(combined));
            } else {
                fxItem->setIcon(
                    IconUtils::createRecoloredIcon(Icon::Currency, IconUtils::DefaultIconColor));
            }

            auto* fxStatus = new QStandardItem();
            fxStatus->setIcon(fxPairStatusIcon(fxRunning));
            fxStatus->setData(static_cast<int>(NodeType::FxPair), NodeTypeRole);
            fxStatus->setData(QString::fromStdString(fxId), NodeIdRole);

            feedItem->appendRow({fxItem, fxStatus});
        }

        root->appendRow({feedItem, feedStatus});
    }

    feedsTree_->expandAll();
    feedsTree_->setColumnWidth(0, 220);
    feedsTree_->header()->setStretchLastSection(false);
    feedsTree_->header()->setSectionResizeMode(0, QHeaderView::Stretch);
    feedsTree_->header()->setSectionResizeMode(1, QHeaderView::Fixed);
    feedsTree_->setColumnWidth(1, 28);
}

void MarketSimulatorWindow::updateEmptyState() {
    emptyHintLabel_->setVisible(feeds_.empty());
}

void MarketSimulatorWindow::updateStatusCounts() {
    statusLabel_->setText(tr("%1 feeds, %2 FX rates").arg(feeds_.size()).arg(fxPairs_.size()));
    emit statusChanged(statusLabel_->text());
}

MarketSimulatorWindow::NodeType MarketSimulatorWindow::currentNodeType() const {
    const auto idx = feedsTree_->currentIndex();
    if (!idx.isValid())
        return NodeType::Feed;
    return static_cast<NodeType>(idx.data(NodeTypeRole).toInt());
}

std::string MarketSimulatorWindow::currentNodeId() const {
    const auto idx = feedsTree_->currentIndex();
    if (!idx.isValid())
        return {};
    return idx.data(NodeIdRole).toString().toStdString();
}

std::vector<synthetic::domain::fx_spot_generation_config>
MarketSimulatorWindow::selectedFxPairs() const {
    // The tree has 2 columns; both column items carry the same NodeId, so we
    // deduplicate by id to avoid sending 2 start/stop requests per pair.
    std::vector<synthetic::domain::fx_spot_generation_config> result;
    std::set<std::string> seen;
    const auto indexes = feedsTree_->selectionModel()->selectedIndexes();
    for (const auto& idx : indexes) {
        if (static_cast<NodeType>(idx.data(NodeTypeRole).toInt()) != NodeType::FxPair)
            continue;
        const auto id = idx.data(NodeIdRole).toString().toStdString();
        if (!seen.insert(id).second)
            continue;
        auto it = fxPairs_.find(id);
        if (it != fxPairs_.end())
            result.push_back(it->second);
    }
    return result;
}

std::string MarketSimulatorWindow::resolveFeedId() const {
    const auto type = currentNodeType();
    const auto id = currentNodeId();
    if (id.empty())
        return {};
    if (type == NodeType::Feed)
        return id;
    // FxPair: return its owning feed.
    auto it = fxPairs_.find(id);
    if (it != fxPairs_.end())
        return boost::uuids::to_string(it->second.config_id);
    return {};
}

QString MarketSimulatorWindow::feedNameFor(const std::string& feedId) const {
    auto it = feeds_.find(feedId);
    return it != feeds_.end() ? QString::fromStdString(it->second.name) : QString();
}

void MarketSimulatorWindow::onTreeSelectionChanged(const QModelIndex& /*current*/,
                                                   const QModelIndex& /*previous*/) {
    showSummaryForCurrent();
    updateToolbarState();
}

void MarketSimulatorWindow::onTreeDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;
    const auto type = static_cast<NodeType>(index.data(NodeTypeRole).toInt());
    const auto id = index.data(NodeIdRole).toString().toStdString();
    editEntity(type, id);
}

void MarketSimulatorWindow::clearSummary() {
    summaryStack_->setCurrentIndex(0);
}

void MarketSimulatorWindow::showSummaryForCurrent() {
    const auto idx = feedsTree_->currentIndex();
    if (!idx.isValid()) {
        clearSummary();
        return;
    }

    const auto type = static_cast<NodeType>(idx.data(NodeTypeRole).toInt());
    const auto id = idx.data(NodeIdRole).toString().toStdString();

    switch (type) {
        case NodeType::Feed: {
            auto it = feeds_.find(id);
            if (it != feeds_.end())
                showFeedSummary(it->second);
            break;
        }
        case NodeType::FxPair: {
            auto it = fxPairs_.find(id);
            if (it != fxPairs_.end())
                showFxPairSummary(it->second);
            break;
        }
    }
}

void MarketSimulatorWindow::showFeedSummary(
    const synthetic::domain::market_data_generation_config& feed) {
    unsubscribeTickChart();
    fxSummaryId_.clear();
    if (tickChartContainer_)
        tickChartContainer_->setVisible(false);
    clear_form(summaryForm_);
    summaryHero_->setVisible(false);
    summaryTitle_->setVisible(true);
    summaryTitle_->setText(tr("<b>Feed</b>"));
    summaryForm_->addRow(tr("Name"), new QLabel(QString::fromStdString(feed.name), summaryPage_));
    if (!feed.description.empty())
        summaryForm_->addRow(tr("Description"),
                             new QLabel(QString::fromStdString(feed.description), summaryPage_));
    summaryForm_->addRow(tr("Enabled"),
                         new QLabel(feed.enabled ? tr("Yes") : tr("No"), summaryPage_));

    // Tally FX rates and how many are currently running (client-side knowledge).
    const auto feedId = boost::uuids::to_string(feed.id);
    int total = 0, running = 0;
    for (const auto& [id, fx] : fxPairs_) {
        if (boost::uuids::to_string(fx.config_id) != feedId)
            continue;
        ++total;
        if (runningSourceNames_.count(fx.source_name))
            ++running;
    }
    summaryForm_->addRow(tr("FX rates"), new QLabel(QString::number(total), summaryPage_));

    const QString statusText = running == 0     ? tr("Stopped") :
                               running == total ? tr("Running") :
                                                  tr("%1 of %2 running").arg(running).arg(total);
    auto* statusLbl = new QLabel(statusText, summaryPage_);
    statusLbl->setStyleSheet(running > 0 ? "color: green; font-weight: bold;" : "color: gray;");
    summaryForm_->addRow(tr("Status"), statusLbl);

    feedStatsLabel_->setVisible(false); // stats folded into the form
    auto* feedBtnRow =
        qobject_cast<QWidget*>(feedStartButton_->property("feedBtnRow").value<QObject*>());
    if (feedBtnRow) {
        feedBtnRow->setVisible(total > 0);
        feedStartButton_->setEnabled(running < total);
        feedStopButton_->setEnabled(running > 0);
    }

    feedSummaryId_ = feedId;
    fxSummaryId_.clear();
    summaryStack_->setCurrentWidget(summaryPage_);
}

void MarketSimulatorWindow::markRunning(const std::vector<std::string>& sourceNames) {
    for (const auto& s : sourceNames) {
        runningSourceNames_.insert(s);
        startCacheSubscription(s); // begin caching ticks immediately
    }
    refreshFeedTreeItems();
    refreshFeedSummaryIfCurrent(feedSummaryId_);
    refreshFxSummaryIfCurrent();
}

void MarketSimulatorWindow::markStopped(const std::vector<std::string>& sourceNames) {
    for (const auto& s : sourceNames) {
        runningSourceNames_.erase(s);
        stopCacheSubscription(s); // stop caching and clear cache
    }
    refreshFeedTreeItems();
    refreshFeedSummaryIfCurrent(feedSummaryId_);
    refreshFxSummaryIfCurrent();
}

void MarketSimulatorWindow::refreshFeedTreeItems() {
    auto* root = treeModel_->invisibleRootItem();
    for (int fi = 0; fi < root->rowCount(); ++fi) {
        auto* feedItem = root->child(fi);
        if (!feedItem)
            continue;
        const auto feedId = feedItem->data(NodeIdRole).toString().toStdString();
        auto it = feeds_.find(feedId);
        if (it == feeds_.end())
            continue;

        int total = 0, running = 0;
        for (const auto& [fxId, fx] : fxPairs_) {
            if (boost::uuids::to_string(fx.config_id) != feedId)
                continue;
            ++total;
            if (runningSourceNames_.count(fx.source_name))
                ++running;
        }

        // Update feed status icon in col 1.
        if (auto* feedStatusItem = treeModel_->item(fi, 1))
            feedStatusItem->setIcon(feedStatusIcon(running, total));

        // Update child FX pair status icons.
        for (int xi = 0; xi < feedItem->rowCount(); ++xi) {
            auto* fxItem = feedItem->child(xi, 0);
            auto* fxStatusItem = feedItem->child(xi, 1);
            if (!fxItem || !fxStatusItem)
                continue;
            const auto fxId = fxItem->data(NodeIdRole).toString().toStdString();
            auto fxit = fxPairs_.find(fxId);
            if (fxit == fxPairs_.end())
                continue;
            const bool fxRunning = runningSourceNames_.count(fxit->second.source_name) > 0;
            fxStatusItem->setIcon(fxPairStatusIcon(fxRunning));
        }
    }
}

void MarketSimulatorWindow::refreshFxSummaryIfCurrent() {
    if (fxSummaryId_.empty() || currentNodeType() != NodeType::FxPair ||
        currentNodeId() != fxSummaryId_)
        return;
    auto it = fxPairs_.find(fxSummaryId_);
    if (it == fxPairs_.end())
        return;
    const bool running = runningSourceNames_.count(it->second.source_name) > 0;
    heroStatus_->setText(running ? tr("●  Running") : tr("○  Stopped"));
    heroStatus_->setStyleSheet(running ? "color: rgb(60,180,80); font-weight: bold;" :
                                         "color: gray;");
    if (tickChartContainer_) {
        if (running && !tickSubscription_) {
            tickSamples_.clear();
            if (tickSeries_)
                tickSeries_->clear();
            if (tickPosMarker_)
                tickPosMarker_->clear();
            tickChartPlaceholder_->setVisible(false);
            subscribeTickChart(it->second.source_name);
        } else if (!running && tickSubscription_) {
            unsubscribeTickChart();
            tickChartPlaceholder_->setVisible(true);
        }
    }
}

void MarketSimulatorWindow::refreshFeedSummaryIfCurrent(const std::string& feedId) {
    if (feedId.empty() || currentNodeType() != NodeType::Feed || currentNodeId() != feedId)
        return;
    auto it = feeds_.find(feedId);
    if (it != feeds_.end())
        showFeedSummary(it->second);
}

void MarketSimulatorWindow::showFxPairSummary(
    const synthetic::domain::fx_spot_generation_config& fx) {
    // Tear down any existing tick subscription before switching pairs. Without
    // this, the old subscription's alive flag is never set false, so it can
    // still deliver messages into the new pair's chart during the drain window.
    unsubscribeTickChart();

    // Hide feed-level controls when switching to an FX pair view.
    if (auto* feedBtnRow =
            qobject_cast<QWidget*>(feedStartButton_->property("feedBtnRow").value<QObject*>()))
        feedBtnRow->setVisible(false);
    feedStatsLabel_->setVisible(false);
    feedSummaryId_.clear();
    fxSummaryId_ = boost::uuids::to_string(fx.id);

    clear_form(summaryForm_);

    const auto fxId = boost::uuids::to_string(fx.id);
    int componentCount = 0;
    for (const auto& [compId, comp] : components_) {
        if (boost::uuids::to_string(comp.fx_spot_config_id) == fxId)
            ++componentCount;
    }

    // Populate the TradingView-style hero header in place of a plain title.
    summaryTitle_->setVisible(false);
    const QString baseCode = QString::fromStdString(fx.base_currency_code);
    const QString quoteCode = QString::fromStdString(fx.quote_currency_code);
    if (imageCache_) {
        const qreal dpr = devicePixelRatioF();
        const QColor ring = summaryPage_->palette().color(QPalette::Window);
        const int h = static_cast<int>(std::lround(56 * dpr));
        const QPixmap basePm = imageCache_->getCurrencyFlagPixmap(fx.base_currency_code, h);
        const QPixmap quotePm = imageCache_->getCurrencyFlagPixmap(fx.quote_currency_code, h);
        heroFlags_->setPixmap(pair_flags_badge(basePm, quotePm, ring, dpr));
    }
    const auto displayName = [this](const std::string& code, const QString& fallback) {
        auto it = currencyNames_.find(code);
        return (it != currencyNames_.end() && !it->second.empty()) ?
                   QString::fromStdString(it->second) :
                   fallback;
    };
    heroTitle_->setText(displayName(fx.base_currency_code, baseCode) + " / " +
                        displayName(fx.quote_currency_code, quoteCode));
    heroSubtitle_->setText(baseCode + quoteCode +
                           (fx.source_name.empty() ?
                                QString() :
                                QString::fromUtf8(" • ") + QString::fromStdString(fx.source_name)));
    const bool isRunning = runningSourceNames_.count(fx.source_name) > 0;
    heroStatus_->setText(isRunning ? tr("●  Running") : tr("○  Stopped"));
    heroStatus_->setStyleSheet(isRunning ? "color: rgb(60,180,80); font-weight: bold;" :
                                           "color: gray;");
    summaryHero_->setVisible(true);

    summaryForm_->addRow(tr("Initial price"),
                         new QLabel(QString::number(fx.gmm_initial_price), summaryPage_));
    {
        const int seconds =
            fx.ticks_per_hour > 0 ?
                std::max(1, static_cast<int>(std::lround(3600.0 / fx.ticks_per_hour))) :
                1;
        summaryForm_->addRow(tr("Update frequency"),
                             new QLabel(tr("every %1 s").arg(seconds), summaryPage_));
    }
    summaryForm_->addRow(tr("Components"),
                         new QLabel(QString::number(componentCount), summaryPage_));

    // Show the tick chart; subscribe if the feed is currently running.
    if (tickChartContainer_) {
        tickSamples_.clear();
        if (tickSeries_)
            tickSeries_->clear();
        if (tickPosMarker_)
            tickPosMarker_->clear();
        tickChartContainer_->setVisible(true);

        // Update chart title and watermark for the selected pair.
        const QString pairCode = baseCode + "/" + quoteCode;
        if (auto* ch = tickChartView_->chart()) {
            ch->setTitle(QString::fromStdString(fx.ore_key) + " (Mid)");
            QFont tf = ch->titleFont();
            tf.setPointSizeF(8.5);
            ch->setTitleFont(tf);
            ch->setTitleBrush(QBrush(QColor(0xCB, 0xD5, 0xE1)));
        }
        if (tickChartView_)
            tickChartView_->setText(pairCode);
        if (isRunning) {
            tickChartPlaceholder_->setVisible(false);
            subscribeTickChart(fx.source_name);
        } else {
            tickChartPlaceholder_->setVisible(true);
        }
    }

    summaryStack_->setCurrentWidget(summaryPage_);
}

void MarketSimulatorWindow::onNewFeedClicked() {
    BOOST_LOG_SEV(lg(), info) << "Opening New Feed dialog.";
    FeedDialog dlg(clientManager_, username_, this);
    if (dlg.exec() == QDialog::Accepted)
        reload();
}

void MarketSimulatorWindow::onNewFxRateClicked() {
    const auto feedId = resolveFeedId();
    if (feedId.empty()) {
        emit errorOccurred(tr("Select a feed first."));
        QMessageBox::information(this,
                                 tr("Select a feed"),
                                 tr("Select a feed (or one of its FX rates) before adding an "
                                    "FX rate."));
        return;
    }
    openFxEditorForNew(feedId);
}

void MarketSimulatorWindow::openFxEditorForNew(const std::string& feedId) {
    BOOST_LOG_SEV(lg(), info) << "Opening FX spot rate editor (new) under feed " << feedId << ".";

    auto* editor = new FxSpotRateEditor(clientManager_,
                                        imageCache_,
                                        changeReasonCache_,
                                        username_,
                                        uuid_from_string(feedId),
                                        feedNameFor(feedId),
                                        this);

    auto* sub = new DetachableMdiSubWindow(window());
    sub->setWidget(editor);
    sub->setWindowTitle(tr("New FX Rate"));
    sub->setAttribute(Qt::WA_DeleteOnClose);

    connect(editor, &FxSpotRateEditor::savedOk, this, &MarketSimulatorWindow::reload);
    connect(editor, &FxSpotRateEditor::statusChanged, this, &MarketSimulatorWindow::statusChanged);
    connect(editor, &FxSpotRateEditor::errorOccurred, this, &MarketSimulatorWindow::errorOccurred);
    connect(editor, &DetailDialogBase::closeRequested, sub, &QWidget::close);

    if (auto* mdi = window()->findChild<QMdiArea*>())
        mdi->addSubWindow(sub);
    sub->resize(editor->sizeHint());
    sub->show();
}

void MarketSimulatorWindow::openFxEditorForEdit(
    const synthetic::domain::fx_spot_generation_config& fx) {
    const auto fxId = boost::uuids::to_string(fx.id);
    BOOST_LOG_SEV(lg(), info) << "Opening FX spot rate editor (edit) for " << fxId << ".";

    // Gather this fx's components from the already-loaded set.
    std::vector<synthetic::domain::gmm_component> comps;
    for (const auto& [compId, comp] : components_) {
        if (boost::uuids::to_string(comp.fx_spot_config_id) == fxId)
            comps.push_back(comp);
    }

    const auto feedId = boost::uuids::to_string(fx.config_id);
    auto* editor = new FxSpotRateEditor(clientManager_,
                                        imageCache_,
                                        changeReasonCache_,
                                        username_,
                                        fx,
                                        feedNameFor(feedId),
                                        comps,
                                        this);

    auto* sub = new DetachableMdiSubWindow(window());
    sub->setWidget(editor);
    sub->setWindowTitle(QString::fromStdString(fx.ore_key));
    sub->setAttribute(Qt::WA_DeleteOnClose);

    connect(editor, &FxSpotRateEditor::savedOk, this, &MarketSimulatorWindow::reload);
    connect(editor, &FxSpotRateEditor::statusChanged, this, &MarketSimulatorWindow::statusChanged);
    connect(editor, &FxSpotRateEditor::errorOccurred, this, &MarketSimulatorWindow::errorOccurred);
    connect(editor, &DetailDialogBase::closeRequested, sub, &QWidget::close);

    if (auto* mdi = window()->findChild<QMdiArea*>())
        mdi->addSubWindow(sub);
    sub->resize(editor->sizeHint());
    sub->show();
}

void MarketSimulatorWindow::onEditClicked() {
    const auto type = currentNodeType();
    const auto id = currentNodeId();
    if (id.empty()) {
        emit errorOccurred(tr("Nothing selected to edit."));
        return;
    }
    editEntity(type, id);
}

void MarketSimulatorWindow::editEntity(NodeType type, const std::string& id) {
    switch (type) {
        case NodeType::Feed: {
            auto it = feeds_.find(id);
            if (it == feeds_.end())
                return;
            BOOST_LOG_SEV(lg(), info) << "Opening Edit Feed dialog for " << id << ".";
            FeedDialog dlg(clientManager_, username_, it->second, this);
            if (dlg.exec() == QDialog::Accepted)
                reload();
            break;
        }
        case NodeType::FxPair: {
            auto it = fxPairs_.find(id);
            if (it == fxPairs_.end())
                return;
            openFxEditorForEdit(it->second);
            break;
        }
    }
}

void MarketSimulatorWindow::onDeleteClicked() {
    const auto type = currentNodeType();
    const auto id = currentNodeId();
    if (id.empty()) {
        emit errorOccurred(tr("Nothing to delete."));
        return;
    }

    const char* typeName = type == NodeType::Feed ? "feed" : "fx rate";

    // Confirm before this destructive, irreversible action — deleting a feed
    // also removes all its child FX rates.
    const QString prompt =
        type == NodeType::Feed ?
            tr("Delete this feed and all of its FX rates? This cannot be undone.") :
            tr("Delete this FX rate? This cannot be undone.");
    if (QMessageBox::question(this,
                              tr("Confirm delete"),
                              prompt,
                              QMessageBox::Yes | QMessageBox::No,
                              QMessageBox::No) != QMessageBox::Yes)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting " << typeName << " " << id << ".";

    QPointer<MarketSimulatorWindow> self = this;
    auto* cm = clientManager_;

    auto task = [cm, type, id]() -> std::pair<bool, QString> {
        namespace m = synthetic::messaging;
        if (type == NodeType::Feed) {
            auto resp = cm->process_authenticated_request(
                m::delete_market_data_generation_config_request{.ids = {id}});
            if (!resp)
                return {false, QString::fromStdString(resp.error())};
            if (!resp->success)
                return {false, QString::fromStdString(resp->message)};
        } else {
            auto resp = cm->process_authenticated_request(
                m::delete_fx_spot_generation_config_request{.ids = {id}});
            if (!resp)
                return {false, QString::fromStdString(resp.error())};
            if (!resp->success)
                return {false, QString::fromStdString(resp->message)};
        }
        return {true, {}};
    };

    const std::string deletedId = id;
    auto* watcher = new QFutureWatcher<std::pair<bool, QString>>(self);
    connect(watcher,
            &QFutureWatcher<std::pair<bool, QString>>::finished,
            self,
            [self, watcher, deletedId]() {
                auto [ok, err] = watcher->result();
                watcher->deleteLater();
                if (!self)
                    return;
                if (!ok) {
                    BOOST_LOG_SEV(lg(), error)
                        << "Delete failed for " << deletedId << ": " << err.toStdString();
                    emit self->errorOccurred(err);
                    QMessageBox::critical(self, self->tr("Delete failed"), err);
                    return;
                }
                BOOST_LOG_SEV(lg(), info) << "Deleted " << deletedId << ".";
                self->statusLabel_->setText(self->tr("Deleted."));
                emit self->statusChanged(self->tr("Deleted."));
                self->reload();
            });
    watcher->setFuture(QtConcurrent::run(task));
}

void MarketSimulatorWindow::updateToolbarState() {
    const bool hasSelection = feedsTree_->currentIndex().isValid();
    const bool hasFeedContext = !resolveFeedId().empty();

    const bool isFxNode = hasSelection && currentNodeType() == NodeType::FxPair;
    const bool isFeedNode = hasSelection && currentNodeType() == NodeType::Feed;
    const bool anyFxSelected = !selectedFxPairs().empty();

    // A Feed node selection also activates start/stop for all its children.
    bool startEnabled = anyFxSelected;
    bool stopEnabled = anyFxSelected;
    if (isFeedNode) {
        const auto pairs = fxPairsForFeed(currentNodeId());
        startEnabled = !pairs.empty();
        stopEnabled = !pairs.empty();
    }

    const bool hasFxPairs = !fxPairs_.empty();

    newFxRateAction_->setEnabled(hasFeedContext);
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    startFeedAction_->setEnabled(startEnabled);
    stopFeedAction_->setEnabled(stopEnabled);
    startAllAction_->setEnabled(hasFxPairs);
    stopAllAction_->setEnabled(hasFxPairs);
    (void)isFxNode;
}

std::vector<synthetic::domain::fx_spot_generation_config>
MarketSimulatorWindow::fxPairsForFeed(const std::string& feedId) const {
    std::vector<synthetic::domain::fx_spot_generation_config> result;
    for (const auto& [id, fx] : fxPairs_)
        if (boost::uuids::to_string(fx.config_id) == feedId)
            result.push_back(fx);
    return result;
}

void MarketSimulatorWindow::onStartFeedClicked() {
    // Prefer the explicit tree selection (what the user highlighted) over the
    // current/focused item. Only fall back to "all in feed" when the selection
    // contains no FX pairs and the focused item is a Feed node — this handles
    // the toolbar button click after clicking the folder row itself.
    auto pairs = selectedFxPairs();
    if (pairs.empty() && feedsTree_->currentIndex().isValid() &&
        currentNodeType() == NodeType::Feed)
        pairs = fxPairsForFeed(currentNodeId());
    startPairsAsync(std::move(pairs));
}

void MarketSimulatorWindow::startPairsAsync(
    std::vector<synthetic::domain::fx_spot_generation_config> pairs) {
    if (pairs.empty())
        return;

    // Build a start request for each selected FX pair (skip any without components).
    using Req = ores::marketdata::messaging::start_market_feed_config_request;
    std::vector<Req> reqs;
    for (const auto& fx : pairs) {
        const auto fxId = boost::uuids::to_string(fx.id);
        std::vector<const synthetic::domain::gmm_component*> comps;
        for (const auto& [compId, comp] : components_) {
            if (boost::uuids::to_string(comp.fx_spot_config_id) == fxId)
                comps.push_back(&comp);
        }
        if (comps.empty()) {
            BOOST_LOG_SEV(lg(), warn)
                << "Skipping " << fx.ore_key << " — no price model components.";
            continue;
        }
        std::sort(comps.begin(), comps.end(), [](const auto* a, const auto* b) {
            return a->component_index < b->component_index;
        });
        Req req;
        req.ore_key = fx.ore_key;
        req.source_name = fx.source_name;
        for (const auto* c : comps) {
            req.gmm_means.push_back(c->mean);
            req.gmm_stdevs.push_back(c->stdev);
            req.gmm_weights.push_back(c->weight);
        }
        req.gmm_initial_price = fx.gmm_initial_price;
        req.ticks_per_hour = static_cast<double>(fx.ticks_per_hour);
        req.process_type = fx.process_type;
        reqs.push_back(std::move(req));
    }

    if (reqs.empty()) {
        QMessageBox::warning(this,
                             tr("No price model"),
                             tr("Add at least one price-behaviour component before starting."));
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Starting " << reqs.size() << " feed(s).";
    QPointer<MarketSimulatorWindow> self = this;
    auto* cm = clientManager_;
    const std::string username = username_.toStdString();
    // Capture party_id on the main thread; stamp() uses it when JWT has no party claim.
    const boost::uuids::uuid partyId = clientManager_->currentPartyId();
    using Results = std::vector<std::pair<std::string, QString>>; // source_name -> error (empty=ok)
    auto task = [cm, reqs, username, partyId]() -> Results {
        Results results;
        boost::uuids::random_generator uuid_gen;

        // Load existing bindings once so we can skip creating duplicates.
        std::set<std::pair<std::string, std::string>> existing_bindings; // (ore_key, source_name)
        {
            namespace m = ores::marketdata::messaging;
            auto br = cm->process_authenticated_request(
                m::get_feed_bindings_request{.offset = 0, .limit = 10000});
            if (br && br->success) {
                for (const auto& b : br->feed_bindings)
                    existing_bindings.emplace(b.ore_key, b.source_name);
            }
        }

        for (const auto& req : reqs) {
            auto resp = cm->process_authenticated_request(req);
            if (!resp) {
                results.push_back({req.source_name, QString::fromStdString(resp.error())});
                continue;
            }
            if (!resp->success) {
                results.push_back({req.source_name, QString::fromStdString(resp->message)});
                continue;
            }
            results.push_back({req.source_name, {}});

            // Only create a feed binding if one doesn't already exist for this
            // (ore_key, source_name) pair — prevents duplicate ingest loops and
            // duplicate market_series records.
            if (existing_bindings.count({req.ore_key, req.source_name})) {
                BOOST_LOG_SEV(lg(), debug)
                    << "Feed binding already exists for " << req.source_name << " — skipping";
                continue;
            }

            ores::marketdata::domain::feed_binding b;
            b.id = uuid_gen();
            b.ore_key = req.ore_key;
            b.source_name = req.source_name;
            b.party_id = partyId;
            b.enabled = true;
            b.performed_by = username;
            b.change_reason_code = "system.new_record";
            b.change_commentary = "Auto-created by Market Simulator on feed start";
            auto bind_req =
                ores::marketdata::messaging::save_feed_binding_request::from(std::move(b));
            auto bind_resp = cm->process_authenticated_request(bind_req);
            if (!bind_resp || !bind_resp->success) {
                const std::string err = bind_resp ? bind_resp->message : bind_resp.error();
                BOOST_LOG_SEV(lg(), warn)
                    << "Feed binding save failed for " << req.source_name << ": " << err;
            } else {
                existing_bindings.emplace(req.ore_key, req.source_name);
            }
        }
        return results;
    };

    auto* watcher = new QFutureWatcher<Results>(self);
    connect(watcher, &QFutureWatcher<Results>::finished, self, [self, watcher]() {
        auto results = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;
        int ok = 0;
        QStringList failed;
        std::vector<std::string> started;
        for (const auto& [key, err] : results) {
            if (err.isEmpty()) {
                BOOST_LOG_SEV(lg(), info) << "Started feed " << key << ".";
                started.push_back(key);
                ++ok;
            } else {
                BOOST_LOG_SEV(lg(), error)
                    << "Start failed for " << key << ": " << err.toStdString();
                failed << QString::fromStdString(key) + ": " + err;
            }
        }
        // Also mark feeds that were already running (service returns success=true for those).
        if (!started.empty())
            self->markRunning(started);
        const QString msg = self->tr("Started %1 feed(s).").arg(ok);
        self->statusLabel_->setText(msg);
        emit self->statusChanged(msg);
        if (!failed.isEmpty())
            QMessageBox::critical(
                self, self->tr("Start failed"), self->tr("Failed to start:\n") + failed.join("\n"));
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void MarketSimulatorWindow::onStopFeedClicked() {
    auto pairs = selectedFxPairs();
    if (pairs.empty() && feedsTree_->currentIndex().isValid() &&
        currentNodeType() == NodeType::Feed)
        pairs = fxPairsForFeed(currentNodeId());
    stopPairsAsync(std::move(pairs));
}

void MarketSimulatorWindow::stopPairsAsync(
    std::vector<synthetic::domain::fx_spot_generation_config> pairs) {
    if (pairs.empty())
        return;

    using Req = ores::marketdata::messaging::stop_market_feed_config_request;
    std::vector<Req> reqs;
    for (const auto& fx : pairs) {
        Req req;
        req.source_name = fx.source_name;
        reqs.push_back(std::move(req));
    }

    BOOST_LOG_SEV(lg(), info) << "Stopping " << reqs.size() << " feed(s).";
    QPointer<MarketSimulatorWindow> self = this;
    auto* cm = clientManager_;

    using Results = std::vector<std::pair<std::string, QString>>;
    auto task = [cm, reqs]() -> Results {
        Results results;
        for (const auto& req : reqs) {
            auto resp = cm->process_authenticated_request(req);
            if (!resp)
                results.push_back({req.source_name, QString::fromStdString(resp.error())});
            else if (!resp->success)
                results.push_back({req.source_name, QString::fromStdString(resp->message)});
            else
                results.push_back({req.source_name, {}});
        }
        return results;
    };

    auto* watcher = new QFutureWatcher<Results>(self);
    connect(watcher, &QFutureWatcher<Results>::finished, self, [self, watcher]() {
        auto results = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;
        int ok = 0;
        QStringList failed;
        std::vector<std::string> stopped;
        for (const auto& [key, err] : results) {
            if (err.isEmpty()) {
                BOOST_LOG_SEV(lg(), info) << "Stopped feed " << key << ".";
                stopped.push_back(key);
                ++ok;
            } else {
                BOOST_LOG_SEV(lg(), error)
                    << "Stop failed for " << key << ": " << err.toStdString();
                failed << QString::fromStdString(key) + ": " + err;
            }
        }
        if (!stopped.empty())
            self->markStopped(stopped);
        const QString msg = self->tr("Stopped %1 feed(s).").arg(ok);
        self->statusLabel_->setText(msg);
        emit self->statusChanged(msg);
        if (!failed.isEmpty())
            QMessageBox::critical(
                self, self->tr("Stop failed"), self->tr("Failed to stop:\n") + failed.join("\n"));
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void MarketSimulatorWindow::onStartAllClicked() {
    // Collect all FX pairs directly — do NOT mutate tree selection.
    // The old pattern (clearSelection + select all + delegate) left the selection
    // containing every FX pair, so subsequent single-pair Start/Stop actions
    // operated on all of them instead of just the current node.
    std::vector<synthetic::domain::fx_spot_generation_config> all;
    for (const auto& [id, fx] : fxPairs_)
        all.push_back(fx);
    startPairsAsync(std::move(all));
}

void MarketSimulatorWindow::onStopAllClicked() {
    std::vector<synthetic::domain::fx_spot_generation_config> all;
    for (const auto& [id, fx] : fxPairs_)
        all.push_back(fx);
    stopPairsAsync(std::move(all));
}

// ---- Tick chart ---------------------------------------------------------

std::string MarketSimulatorWindow::synthetic_subject(const std::string& source_name) {
    std::string token;
    for (unsigned char c : source_name) {
        const bool safe = std::isalnum(c) || c == '.' || c == '_' || c == '-';
        token += safe ? static_cast<char>(c) : '_';
    }
    return "synthetic.v1.tick." + token;
}

void MarketSimulatorWindow::subscribeTickChart(const std::string& source_name) {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    // Pre-populate the chart from the buffered subscription snapshot.
    tickSamples_.clear();
    auto cit = cacheSubscriptions_.find(source_name);
    if (cit != cacheSubscriptions_.end()) {
        for (const auto& msg : cit->second.snapshot()) {
            const auto result = rfl::json::read<marketdata::domain::fx_spot_tick>(
                ores::nats::as_string_view(msg.data));
            if (result)
                tickSamples_.push_back(result->mid);
        }
    }
    refreshTickChart();

    const std::string subject = synthetic_subject(source_name);
    BOOST_LOG_SEV(lg(), debug) << "Tick chart subscribing to: " << subject;

    // Create a new alive flag for this subscription. The lambda holds a copy
    // of the shared_ptr; setting it to false makes the callback exit before
    // touching any member of this window, preventing use-after-free when the
    // subscription is destroyed concurrently with an in-flight on_msg call.
    tickAlive_ = std::make_shared<std::atomic<bool>>(true);
    auto alive = tickAlive_;
    QPointer<MarketSimulatorWindow> self = this;

    try {
        tickSubscription_ = clientManager_->nats_client().subscribe(
            subject, [self, alive](ores::nats::message msg) {
                if (!alive->load(std::memory_order_acquire))
                    return;
                const std::string_view payload = ores::nats::as_string_view(msg.data);
                auto result = rfl::json::read<marketdata::domain::fx_spot_tick>(payload);
                if (!result)
                    return;
                const double mid = result->mid;
                QMetaObject::invokeMethod(
                    self,
                    [self, mid]() {
                        if (self)
                            self->appendTickSample(mid);
                    },
                    Qt::QueuedConnection);
            });
        if (tickFlashTimer_)
            tickFlashTimer_->start();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Tick chart subscribe failed: " << e.what();
    }
}

void MarketSimulatorWindow::unsubscribeTickChart() {
    if (tickFlashTimer_)
        tickFlashTimer_->stop();
    if (tickPosMarker_)
        tickPosMarker_->clear();
    // Gate the lambda first so no queued invokeMethod posts arrive after we
    // return, then drain the subscription. The drain is async; the closure is
    // freed only after the last on_msg returns, so there is no UAF.
    if (tickAlive_)
        tickAlive_->store(false, std::memory_order_release);
    tickSubscription_.reset();
    tickAlive_.reset();
}

void MarketSimulatorWindow::refreshTickChart() {
    if (!tickSeries_ || !tickAxisX_ || !tickAxisY_)
        return;
    const int n = static_cast<int>(tickSamples_.size());
    if (n == 0) {
        tickSeries_->clear();
        if (tickPosMarker_)
            tickPosMarker_->clear();
        return;
    }
    QList<QPointF> pts;
    pts.reserve(n);
    double minY = tickSamples_.front(), maxY = minY;
    for (int i = 0; i < n; ++i) {
        const double v = tickSamples_[i];
        pts.append(QPointF(static_cast<double>(i), v));
        minY = std::min(minY, v);
        maxY = std::max(maxY, v);
    }
    tickSeries_->replace(pts);
    if (tickPosMarker_) {
        tickPosMarker_->clear();
        tickPosMarker_->append(pts.back());
    }
    const double range = maxY - minY;
    const double pad = (range > 0.0) ? range * 0.01 : std::max(maxY * 0.0005, 1e-6);
    tickAxisY_->setRange(minY - pad, maxY + pad);
    tickAxisX_->setRange(0, n == 1 ? 1.0 : static_cast<double>(n - 1));
}

void MarketSimulatorWindow::appendTickSample(double mid) {
    constexpr int kMax = 120;
    tickSamples_.push_back(mid);
    while (static_cast<int>(tickSamples_.size()) > kMax)
        tickSamples_.pop_front();
    refreshTickChart();
}

void MarketSimulatorWindow::startCacheSubscription(const std::string& source_name) {
    if (!clientManager_ || !clientManager_->isConnected())
        return;
    if (cacheSubscriptions_.contains(source_name))
        return;
    const std::string subject = synthetic_subject(source_name);
    BOOST_LOG_SEV(lg(), debug) << "Cache subscription starting for: " << source_name;
    try {
        cacheSubscriptions_.emplace(
            source_name, clientManager_->nats_client().subscribe_buffered(subject, 1000));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "Cache subscribe failed for " << source_name << ": " << e.what();
    }
}

void MarketSimulatorWindow::stopCacheSubscription(const std::string& source_name) {
    auto it = cacheSubscriptions_.find(source_name);
    if (it == cacheSubscriptions_.end())
        return;
    // ~buffered_subscription → ~subscription locks sub_closure::mu, so this
    // safely waits for any in-flight on_msg to finish before returning.
    cacheSubscriptions_.erase(it);
}

void MarketSimulatorWindow::onTickChartFlash() {
    if (!tickPosMarker_ || tickPosMarker_->count() == 0)
        return;
    tickFlashBig_ = !tickFlashBig_;
    tickPosMarker_->setMarkerSize(tickFlashBig_ ? 15.0 : 9.0);
}

}
