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
#ifndef ORES_QT_MARKET_SIMULATOR_WINDOW_HPP
#define ORES_QT_MARKET_SIMULATOR_WINDOW_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/buffered_subscription.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/WatermarkChartView.hpp"
#include "ores.synthetic.api/domain/fx_spot_generation_config.hpp"
#include "ores.synthetic.api/domain/gmm_component.hpp"
#include "ores.synthetic.api/domain/market_data_generation_config.hpp"
#include <QFormLayout>
#include <QLabel>
#include <QPushButton>
#include <QSplitter>
#include <QStackedWidget>
#include <QStandardItemModel>
#include <QToolBar>
#include <QTreeView>
#include <QWidget>
#include <deque>
#include <map>
#include <optional>
#include <set>

// Forward declarations for chart members (full headers only needed in .cpp).
class QChartView; // used for non-watermark chart views if needed
class QLineSeries;
class QScatterSeries;
class QTimer;
class QValueAxis;
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace ores::qt {

class ImageCache;
class ChangeReasonCache;

/**
 * @brief Composite MDI window for authoring synthetic market data feeds.
 *
 * The Market Simulator presents a tree, on the left, mirroring the feed
 * namespace path (synthetic.<collection>.<asset class>.<instrument type>.
 * <pair>): Synthetic (root) > Collection (market_data_generation_config,
 * e.g. Basic/Realistic) > asset class > instrument type > Feed (the leaf
 * currency pair, fx_spot_generation_config, e.g. GBP/USD). The tree is the
 * browser; the right panel is a read-only summary of the selected node.
 * Collections are created/edited in a modal FeedDialog; Feeds (with their GMM
 * price model) are created/edited in the FxSpotRateEditor, shown as a
 * non-modal MDI sub-window.
 *
 * Modelled on DataLibrarianWindow's composition pattern.
 */
class MarketSimulatorWindow final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.market_simulator_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit MarketSimulatorWindow(ClientManager* clientManager,
                                   const QString& username,
                                   ImageCache* imageCache,
                                   ChangeReasonCache* changeReasonCache,
                                   QWidget* parent = nullptr);
    ~MarketSimulatorWindow() override = default;

    QSize sizeHint() const override {
        return QSize(1500, 900);
    }

protected:
    // Keeps tickChartPlaceholder_ (a manually-positioned overlay, not layout-
    // managed) sized to match tickChartView_ whenever it resizes — otherwise
    // it's stuck at whatever size the view had at construction time (before
    // the MDI window laid it out), showing clipped/misplaced text.
    bool eventFilter(QObject* watched, QEvent* event) override;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

private slots:
    void onTreeSelectionChanged(const QModelIndex& current, const QModelIndex& previous);
    void onTreeDoubleClicked(const QModelIndex& index);
    void onReloadClicked();
    void onNewFeedClicked();
    void onNewFxRateClicked();
    void onEditClicked();
    void onDeleteClicked();
    void onStartFeedClicked();
    void onStopFeedClicked();
    void onValidateVintageClicked();
    void appendTickSample(double mid);
    void onTickChartFlash();

private:
    // Node levels stored in the tree items via Qt::UserRole markers. The tree
    // mirrors the feed's own namespace path (synthetic.<collection>.<asset
    // class>.<instrument type>.<pair>, e.g. synthetic.basic.fx.fxrates.gbpusd):
    // Root ("Synthetic") > Collection (Basic/Realistic) > Group (asset class,
    // then instrument type -- both use the same structural node type) > Feed
    // (the leaf currency pair, e.g. GBP/USD). Selecting/starting/stopping any
    // non-leaf node cascades to every Feed beneath it.
    enum class NodeType { Root, Collection, Group, Feed };

    static std::string synthetic_subject(const std::string& source_name);
    void subscribeTickChart(const std::string& source_name);
    void unsubscribeTickChart();
    void refreshTickChart();
    void startCacheSubscription(const std::string& source_name);
    void stopCacheSubscription(const std::string& source_name);

    void setupUi();
    void setupToolbar();
    void setupLeftPanel();
    void setupRightPanel();
    void setupConnections();

    void reload();
    void buildTree();
    void updateToolbarState();
    void updateStatusCounts();
    void updateEmptyState();

    void showSummaryForCurrent();
    void showFeedSummary(const synthetic::domain::market_data_generation_config& feed);
    void showFolderSummary(const QModelIndex& idx, const QString& title, const QString& name);
    void showFxPairSummary(const synthetic::domain::fx_spot_generation_config& fx);
    void clearSummary();

    void editEntity(NodeType type, const std::string& id);
    void openFxEditorForNew(const std::string& feedId);
    void openFxEditorForEdit(const synthetic::domain::fx_spot_generation_config& fx);

    // Resolve the owning Collection id implied by the current selection (the
    // node itself if it's a Collection, its nearest Collection ancestor if
    // it's a Group or Feed, or empty if it's Root or unresolvable).
    [[nodiscard]] std::string resolveFeedId() const;
    [[nodiscard]] QString feedNameFor(const std::string& feedId) const;

    [[nodiscard]] NodeType currentNodeType() const;
    [[nodiscard]] std::string currentNodeId() const;
    // All Feed-leaf pairs reachable from the current tree selection, cascading
    // through Root/Collection/Group nodes to their descendant Feeds.
    [[nodiscard]] std::vector<synthetic::domain::fx_spot_generation_config> selectedFxPairs() const;
    // All Feed-leaf pairs nested under the given index (itself if it's a Feed).
    [[nodiscard]] std::vector<synthetic::domain::fx_spot_generation_config>
    pairsUnderIndex(const QModelIndex& idx) const;

    void startPairsAsync(std::vector<synthetic::domain::fx_spot_generation_config> pairs);
    void stopPairsAsync(std::vector<synthetic::domain::fx_spot_generation_config> pairs);

    void markRunning(const std::vector<std::string>& sourceNames);
    void markStopped(const std::vector<std::string>& sourceNames);
    void refreshFeedSummaryIfCurrent(const std::string& feedId);
    void refreshFxSummaryIfCurrent();
    void refreshFeedTreeItems();
    // Recomputes status icons bottom-up starting at item; returns its
    // (running, total) leaf counts so a caller can fold them upward.
    std::pair<int, int> refreshTreeItemStatus(QStandardItem* item);
    // Recursively collects the ids of every Feed-leaf descendant of item
    // (including item itself, if it's already a Feed).
    static void collectFeedIdsUnder(QStandardItem* item, std::vector<std::string>& ids);

    ClientManager* clientManager_;
    QString username_;
    ImageCache* imageCache_;
    ChangeReasonCache* changeReasonCache_;

    // Layout
    QSplitter* mainSplitter_;

    // Toolbar
    QToolBar* toolbar_;
    QAction* reloadAction_;
    QAction* newFeedAction_;
    QAction* newFxRateAction_;
    QAction* editAction_;
    QAction* deleteAction_;
    QAction* startFeedAction_;
    QAction* stopFeedAction_;
    QAction* validateVintageAction_;

    // Left panel
    QTreeView* feedsTree_;
    QStandardItemModel* treeModel_;
    QLabel* emptyHintLabel_;

    // Status bar
    QLabel* statusLabel_;

    // Right panel: read-only summary.
    QStackedWidget* summaryStack_;
    QWidget* summaryPage_;
    QFormLayout* summaryForm_;
    QLabel* summaryTitle_;

    // TradingView-style hero header for FX pairs: big overlapping circular
    // flags and a bold pair title.
    QWidget* summaryHero_;
    QLabel* heroFlags_;
    QLabel* heroTitle_;
    QLabel* heroSubtitle_;
    QLabel* heroStatus_; // running/stopped indicator beneath the subtitle

    // Feed detail panel start/stop buttons (only visible when a Feed node is selected).
    QPushButton* feedStartButton_;
    QPushButton* feedStopButton_;
    QLabel* feedStatsLabel_;
    std::string feedSummaryId_; // id of the feed currently shown in the right panel
    std::string fxSummaryId_;   // id of the fx pair currently shown in the right panel

    // In-memory copies keyed by id (uuid string).
    std::map<std::string, synthetic::domain::market_data_generation_config> feeds_;
    std::map<std::string, synthetic::domain::fx_spot_generation_config> fxPairs_;
    std::map<std::string, synthetic::domain::gmm_component> components_;

    // source_names of feeds the client has successfully started this session.
    std::set<std::string> runningSourceNames_;

    // Currency ISO code -> display name, sourced from refdata for hero titles.
    std::unordered_map<std::string, std::string> currencyNames_;

    // Synthetic tick chart embedded in the FX pair detail panel.
    QWidget* tickChartContainer_{nullptr};
    WatermarkChartView* tickChartView_{nullptr};
    QLineSeries* tickSeries_{nullptr};
    QScatterSeries* tickPosMarker_{nullptr};
    QValueAxis* tickAxisX_{nullptr};
    QValueAxis* tickAxisY_{nullptr};
    QLabel* tickChartPlaceholder_{nullptr};
    QTimer* tickFlashTimer_{nullptr};
    std::deque<double> tickSamples_;
    std::optional<nats::service::subscription> tickSubscription_;
    // Alive flag shared with the NATS callback lambda. Set to false before
    // destroying the subscription so any in-flight callback exits early,
    // preventing use-after-free of the subscription closure.
    std::shared_ptr<std::atomic<bool>> tickAlive_;
    bool tickFlashBig_{false};

    // Per-source tick cache: populated while a feed is running so switching
    // back to a pair restores the chart immediately (no warm-up wait).
    // Key = source_name, value = rolling deque of up to 1000 mid prices.
    // Per-source buffered subscriptions: active while a feed is running,
    // keeping the last 1000 raw messages so the chart warms up instantly.
    std::map<std::string, nats::service::buffered_subscription> cacheSubscriptions_;

    bool loading_{false};
};

}

#endif
