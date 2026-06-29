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
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/FeedDialog.hpp"
#include "ores.qt/FxSpotRateEditor.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.marketdata.api/messaging/market_feed_config_protocol.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
#include "ores.synthetic.api/messaging/market_data_generation_config_protocol.hpp"
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QIcon>
#include <QMdiArea>
#include <QColor>
#include <QFont>
#include <QPainter>
#include <QPalette>
#include <QPainterPath>
#include <QPixmap>
#include <QPointF>
#include <QMessageBox>
#include <QPointer>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <algorithm>
#include <cmath>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>

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

    QPixmap scaled = src.scaled(QSize(diameter, diameter) * dpr,
        Qt::KeepAspectRatioByExpanding, Qt::SmoothTransformation);
    scaled.setDevicePixelRatio(dpr);
    const qreal x = (diameter - scaled.width() / dpr) / 2.0;
    const qreal y = (diameter - scaled.height() / dpr) / 2.0;
    p.drawPixmap(QPointF(x, y), scaled);
    return canvas;
}

// Compose a TradingView-style overlapping pair badge: the base-currency flag
// sits front/lower-left over the quote-currency flag behind/upper-right, with
// a separator ring around the front flag.
QPixmap pair_flags_badge(const QPixmap& base, const QPixmap& quote,
                         const QColor& ringColour, qreal dpr) {
    constexpr int d = 56;     // flag diameter
    constexpr int off = 30;   // overlap offset
    constexpr int m = 3;      // separator ring width
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
    p.drawEllipse(QPointF(m + d / 2.0, m + off + d / 2.0),
                  d / 2.0 + m, d / 2.0 + m);

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
        IconUtils::createRecoloredIcon(Icon::ArrowSync, IconUtils::DefaultIconColor),
        tr("Reload"));
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
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor),
        tr("Start feed"));
    startFeedAction_->setToolTip(
        tr("Start generating live ticks for the selected FX rate."));

    stopFeedAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::DeleteDismiss, IconUtils::DefaultIconColor),
        tr("Stop feed"));
    stopFeedAction_->setToolTip(tr("Stop generating ticks for the selected FX rate."));
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
    feedsTree_->setFrameShape(QFrame::StyledPanel);
    feedsTree_->setFrameShadow(QFrame::Sunken);
    leftLayout->addWidget(feedsTree_, 1);

    emptyHintLabel_->setWordWrap(true);
    emptyHintLabel_->setText(
        tr("No feeds yet. Click 'New Feed' to define a market-data feed, then "
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
    auto* emptyLabel = new QLabel(tr("Select a feed or FX rate to see its details."),
                                  emptyPage);
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
    heroTextLayout->addWidget(heroTitle_);
    heroTextLayout->addWidget(heroSubtitle_);
    heroTextLayout->addStretch(1);
    heroLayout->addWidget(heroText, 1);
    summaryLayout->addWidget(summaryHero_);
    summaryHero_->setVisible(false);

    summaryTitle_ = new QLabel(summaryPage_);
    summaryLayout->addWidget(summaryTitle_);
    summaryForm_ = new QFormLayout();
    summaryLayout->addLayout(summaryForm_);
    summaryLayout->addStretch(1);

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
    connect(feedsTree_->selectionModel(), &QItemSelectionModel::currentChanged, this,
            &MarketSimulatorWindow::onTreeSelectionChanged);
    connect(feedsTree_, &QTreeView::doubleClicked, this,
            &MarketSimulatorWindow::onTreeDoubleClicked);

    connect(reloadAction_, &QAction::triggered, this,
            &MarketSimulatorWindow::onReloadClicked);
    connect(newFeedAction_, &QAction::triggered, this,
            &MarketSimulatorWindow::onNewFeedClicked);
    connect(newFxRateAction_, &QAction::triggered, this,
            &MarketSimulatorWindow::onNewFxRateClicked);
    connect(editAction_, &QAction::triggered, this, &MarketSimulatorWindow::onEditClicked);
    connect(deleteAction_, &QAction::triggered, this, &MarketSimulatorWindow::onDeleteClicked);
    connect(startFeedAction_, &QAction::triggered, this,
            &MarketSimulatorWindow::onStartFeedClicked);
    connect(stopFeedAction_, &QAction::triggered, this,
            &MarketSimulatorWindow::onStopFeedClicked);
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
        r.feeds = std::move(feedsResp->configs);

        auto fxResp = cm->process_authenticated_request(
            m::get_fx_spot_generation_configs_request{.offset = 0, .limit = 1000});
        if (!fxResp) {
            r.error = QString::fromStdString(fxResp.error());
            return r;
        }
        r.fxPairs = std::move(fxResp->configs);

        auto compResp = cm->process_authenticated_request(
            m::get_gmm_components_request{.offset = 0, .limit = 1000});
        if (!compResp) {
            r.error = QString::fromStdString(compResp.error());
            return r;
        }
        r.components = std::move(compResp->components);

        // Currency display names (best-effort; an empty map just falls back to
        // ISO codes in the hero title).
        r.currencyNames = fetch_currency_names(cm);

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

        for (auto& f : result.feeds)
            self->feeds_[boost::uuids::to_string(f.id)] = std::move(f);
        for (auto& fx : result.fxPairs)
            self->fxPairs_[boost::uuids::to_string(fx.id)] = std::move(fx);
        for (auto& c : result.components)
            self->components_[boost::uuids::to_string(c.id)] = std::move(c);

        BOOST_LOG_SEV(lg(), info)
            << "Loaded " << self->feeds_.size() << " feeds, " << self->fxPairs_.size()
            << " fx pairs, " << self->components_.size() << " components.";

        self->buildTree();
        self->updateStatusCounts();
        self->updateEmptyState();
        self->clearSummary();
        self->updateToolbarState();
    });

    watcher->setFuture(QtConcurrent::run(task));
}

void MarketSimulatorWindow::buildTree() {
    treeModel_->clear();
    auto* root = treeModel_->invisibleRootItem();

    for (const auto& [feedId, feed] : feeds_) {
        QString feedText = QString::fromStdString(feed.name);
        if (!feed.enabled)
            feedText += tr(" (disabled)");

        auto* feedItem = new QStandardItem(feedText);
        feedItem->setData(static_cast<int>(NodeType::Feed), NodeTypeRole);
        feedItem->setData(QString::fromStdString(feedId), NodeIdRole);
        feedItem->setIcon(
            IconUtils::createRecoloredIcon(Icon::Folder, IconUtils::DefaultIconColor));

        for (const auto& [fxId, fx] : fxPairs_) {
            if (boost::uuids::to_string(fx.config_id) != feedId)
                continue;

            QString fxText = QString::fromStdString("FX/RATE/" + fx.base_currency_code + "/" +
                                                    fx.quote_currency_code);
            auto* fxItem = new QStandardItem(fxText);
            fxItem->setData(static_cast<int>(NodeType::FxPair), NodeTypeRole);
            fxItem->setData(QString::fromStdString(fxId), NodeIdRole);
            if (imageCache_) {
                // Compose both currency flags (base then quote) into one icon.
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

            feedItem->appendRow(fxItem);
        }

        root->appendRow(feedItem);
    }

    feedsTree_->expandAll();
}

void MarketSimulatorWindow::updateEmptyState() {
    emptyHintLabel_->setVisible(feeds_.empty());
}

void MarketSimulatorWindow::updateStatusCounts() {
    statusLabel_->setText(tr("%1 feeds, %2 FX rates")
                              .arg(feeds_.size())
                              .arg(fxPairs_.size()));
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
    clear_form(summaryForm_);
    summaryHero_->setVisible(false);
    summaryTitle_->setVisible(true);
    summaryTitle_->setText(tr("<b>Feed</b>"));
    summaryForm_->addRow(tr("Name"), new QLabel(QString::fromStdString(feed.name), summaryPage_));
    summaryForm_->addRow(
        tr("Description"), new QLabel(QString::fromStdString(feed.description), summaryPage_));
    summaryForm_->addRow(
        tr("Enabled"), new QLabel(feed.enabled ? tr("Yes") : tr("No"), summaryPage_));
    summaryStack_->setCurrentWidget(summaryPage_);
}

void MarketSimulatorWindow::showFxPairSummary(
    const synthetic::domain::fx_spot_generation_config& fx) {
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
        return (it != currencyNames_.end() && !it->second.empty())
            ? QString::fromStdString(it->second)
            : fallback;
    };
    heroTitle_->setText(displayName(fx.base_currency_code, baseCode) + " / "
        + displayName(fx.quote_currency_code, quoteCode));
    heroSubtitle_->setText(baseCode + quoteCode
        + (fx.source_name.empty()
               ? QString()
               : QString::fromUtf8(" • ") + QString::fromStdString(fx.source_name)));
    summaryHero_->setVisible(true);

    summaryForm_->addRow(
        tr("ORE key"), new QLabel(QString::fromStdString(fx.ore_key), summaryPage_));
    summaryForm_->addRow(
        tr("Source name"), new QLabel(QString::fromStdString(fx.source_name), summaryPage_));
    summaryForm_->addRow(
        tr("Initial price"), new QLabel(QString::number(fx.gmm_initial_price), summaryPage_));
    {
        const int seconds = fx.ticks_per_hour > 0
            ? std::max(1, static_cast<int>(std::lround(3600.0 / fx.ticks_per_hour)))
            : 1;
        summaryForm_->addRow(
            tr("Update frequency"),
            new QLabel(tr("every %1 s").arg(seconds), summaryPage_));
    }
    summaryForm_->addRow(
        tr("Enabled"), new QLabel(fx.enabled ? tr("Yes") : tr("No"), summaryPage_));
    summaryForm_->addRow(
        tr("Components"), new QLabel(QString::number(componentCount), summaryPage_));
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
        QMessageBox::information(this, tr("Select a feed"),
                                 tr("Select a feed (or one of its FX rates) before adding an "
                                    "FX rate."));
        return;
    }
    openFxEditorForNew(feedId);
}

void MarketSimulatorWindow::openFxEditorForNew(const std::string& feedId) {
    BOOST_LOG_SEV(lg(), info) << "Opening FX spot rate editor (new) under feed " << feedId << ".";

    auto* editor = new FxSpotRateEditor(clientManager_, imageCache_, changeReasonCache_,
                                        username_, uuid_from_string(feedId),
                                        feedNameFor(feedId), this);

    auto* sub = new DetachableMdiSubWindow(window());
    sub->setWidget(editor);
    sub->setWindowTitle(tr("New FX Rate"));
    sub->setAttribute(Qt::WA_DeleteOnClose);

    connect(editor, &FxSpotRateEditor::savedOk, this, &MarketSimulatorWindow::reload);
    connect(editor, &FxSpotRateEditor::statusChanged, this,
            &MarketSimulatorWindow::statusChanged);
    connect(editor, &FxSpotRateEditor::errorOccurred, this,
            &MarketSimulatorWindow::errorOccurred);
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
    auto* editor = new FxSpotRateEditor(clientManager_, imageCache_, changeReasonCache_,
                                        username_, fx, feedNameFor(feedId), comps, this);

    auto* sub = new DetachableMdiSubWindow(window());
    sub->setWidget(editor);
    sub->setWindowTitle(QString::fromStdString(fx.ore_key));
    sub->setAttribute(Qt::WA_DeleteOnClose);

    connect(editor, &FxSpotRateEditor::savedOk, this, &MarketSimulatorWindow::reload);
    connect(editor, &FxSpotRateEditor::statusChanged, this,
            &MarketSimulatorWindow::statusChanged);
    connect(editor, &FxSpotRateEditor::errorOccurred, this,
            &MarketSimulatorWindow::errorOccurred);
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
    const QString prompt = type == NodeType::Feed
        ? tr("Delete this feed and all of its FX rates? This cannot be undone.")
        : tr("Delete this FX rate? This cannot be undone.");
    if (QMessageBox::question(this, tr("Confirm delete"), prompt,
                              QMessageBox::Yes | QMessageBox::No, QMessageBox::No)
        != QMessageBox::Yes)
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
    connect(watcher, &QFutureWatcher<std::pair<bool, QString>>::finished, self,
            [self, watcher, deletedId]() {
                auto [ok, err] = watcher->result();
                watcher->deleteLater();
                if (!self)
                    return;
                if (!ok) {
                    BOOST_LOG_SEV(lg(), error) << "Delete failed for " << deletedId << ": "
                                               << err.toStdString();
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

    newFxRateAction_->setEnabled(hasFeedContext);
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    startFeedAction_->setEnabled(isFxNode);
    stopFeedAction_->setEnabled(isFxNode);
}

void MarketSimulatorWindow::onStartFeedClicked() {
    if (currentNodeType() != NodeType::FxPair)
        return;
    auto it = fxPairs_.find(currentNodeId());
    if (it == fxPairs_.end())
        return;
    const auto fx = it->second;
    const auto fxId = boost::uuids::to_string(fx.id);

    // Collect this fx's components, ordered by component_index.
    std::vector<const synthetic::domain::gmm_component*> comps;
    for (const auto& [compId, comp] : components_) {
        if (boost::uuids::to_string(comp.fx_spot_config_id) == fxId)
            comps.push_back(&comp);
    }
    std::sort(comps.begin(), comps.end(), [](const auto* a, const auto* b) {
        return a->component_index < b->component_index;
    });

    if (comps.empty()) {
        QMessageBox::warning(
            this, tr("No price model"),
            tr("Add at least one price-behaviour component before starting the feed."));
        return;
    }

    ores::marketdata::messaging::start_market_feed_config_request req;
    req.ore_key = fx.ore_key;
    req.gmm_means.clear();
    req.gmm_stdevs.clear();
    req.gmm_weights.clear();
    for (const auto* c : comps) {
        req.gmm_means.push_back(c->mean);
        req.gmm_stdevs.push_back(c->stdev);
        req.gmm_weights.push_back(c->weight);
    }
    req.gmm_initial_price = fx.gmm_initial_price;
    req.ticks_per_hour = static_cast<double>(fx.ticks_per_hour);
    req.process_type = fx.process_type;

    const QString pair = QString::fromStdString(fx.base_currency_code) + "/" +
        QString::fromStdString(fx.quote_currency_code);
    const std::string oreKey = fx.ore_key;
    BOOST_LOG_SEV(lg(), info) << "Starting feed " << oreKey << ".";

    QPointer<MarketSimulatorWindow> self = this;
    auto* cm = clientManager_;

    auto task = [cm, req]() -> std::pair<bool, QString> {
        auto resp = cm->process_authenticated_request(req);
        if (!resp)
            return {false, QString::fromStdString(resp.error())};
        if (!resp->success)
            return {false, QString::fromStdString(resp->message)};
        return {true, {}};
    };

    auto* watcher = new QFutureWatcher<std::pair<bool, QString>>(self);
    connect(watcher, &QFutureWatcher<std::pair<bool, QString>>::finished, self,
            [self, watcher, oreKey, pair]() {
                auto [ok, err] = watcher->result();
                watcher->deleteLater();
                if (!self)
                    return;
                if (!ok) {
                    BOOST_LOG_SEV(lg(), error)
                        << "Start failed for feed " << oreKey << ": " << err.toStdString();
                    emit self->errorOccurred(err);
                    QMessageBox::critical(self, self->tr("Start failed"), err);
                    return;
                }
                BOOST_LOG_SEV(lg(), info) << "Started feed " << oreKey << ".";
                const QString msg =
                    self->tr("Feed started for %1. Watch it live in Market Data ▸ "
                             "Market Series.")
                        .arg(pair);
                self->statusLabel_->setText(msg);
                emit self->statusChanged(msg);
                QMessageBox::information(self, self->tr("Feed started"), msg);
            });
    watcher->setFuture(QtConcurrent::run(task));
}

void MarketSimulatorWindow::onStopFeedClicked() {
    if (currentNodeType() != NodeType::FxPair)
        return;
    auto it = fxPairs_.find(currentNodeId());
    if (it == fxPairs_.end())
        return;
    const auto fx = it->second;

    ores::marketdata::messaging::stop_market_feed_config_request req;
    req.ore_key = fx.ore_key;

    const std::string oreKey = fx.ore_key;
    BOOST_LOG_SEV(lg(), info) << "Stopping feed " << oreKey << ".";

    QPointer<MarketSimulatorWindow> self = this;
    auto* cm = clientManager_;

    auto task = [cm, req]() -> std::pair<bool, QString> {
        auto resp = cm->process_authenticated_request(req);
        if (!resp)
            return {false, QString::fromStdString(resp.error())};
        if (!resp->success)
            return {false, QString::fromStdString(resp->message)};
        return {true, {}};
    };

    auto* watcher = new QFutureWatcher<std::pair<bool, QString>>(self);
    connect(watcher, &QFutureWatcher<std::pair<bool, QString>>::finished, self,
            [self, watcher, oreKey]() {
                auto [ok, err] = watcher->result();
                watcher->deleteLater();
                if (!self)
                    return;
                if (!ok) {
                    BOOST_LOG_SEV(lg(), error)
                        << "Stop failed for feed " << oreKey << ": " << err.toStdString();
                    emit self->errorOccurred(err);
                    QMessageBox::critical(self, self->tr("Stop failed"), err);
                    return;
                }
                BOOST_LOG_SEV(lg(), info) << "Stopped feed " << oreKey << ".";
                self->statusLabel_->setText(self->tr("Feed stopped."));
                emit self->statusChanged(self->tr("Feed stopped."));
            });
    watcher->setFuture(QtConcurrent::run(task));
}

}
