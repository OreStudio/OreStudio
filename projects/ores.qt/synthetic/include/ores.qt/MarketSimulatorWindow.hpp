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
#include "ores.qt/ClientManager.hpp"
#include "ores.synthetic.api/domain/fx_spot_generation_config.hpp"
#include "ores.synthetic.api/domain/gmm_component.hpp"
#include "ores.synthetic.api/domain/market_data_generation_config.hpp"
#include <QFormLayout>
#include <QLabel>
#include <QSplitter>
#include <QStackedWidget>
#include <QStandardItemModel>
#include <QToolBar>
#include <QTreeView>
#include <QWidget>
#include <map>
#include <string>
#include <vector>

namespace ores::qt {

/**
 * @brief Composite MDI window for authoring synthetic market data feeds.
 *
 * The Market Simulator presents a three-level tree of feeds
 * (market_data_generation_config) > FX pairs (fx_spot_generation_config) >
 * GMM components (gmm_component) on the left. The tree is the browser; the
 * right panel is a read-only summary of the selected node. Creating and
 * editing happen in focused modal child dialogs (FeedDialog, FxPairDialog,
 * ComponentDialog) parented to this window.
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
                                   QWidget* parent = nullptr);
    ~MarketSimulatorWindow() override = default;

    QSize sizeHint() const override {
        return QSize(1500, 900);
    }

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

private slots:
    void onTreeSelectionChanged(const QModelIndex& current, const QModelIndex& previous);
    void onTreeDoubleClicked(const QModelIndex& index);
    void onReloadClicked();
    void onNewFeedClicked();
    void onNewFxPairClicked();
    void onNewComponentClicked();
    void onEditClicked();
    void onDeleteClicked();

private:
    // Node levels stored in the tree items via Qt::UserRole markers.
    enum class NodeType { Feed, FxPair, Component };

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
    void showFxPairSummary(const synthetic::domain::fx_spot_generation_config& fx);
    void showComponentSummary(const synthetic::domain::gmm_component& comp);
    void clearSummary();

    void editEntity(NodeType type, const std::string& id);

    // Resolve the feed / fx-pair id implied by the current selection (node or
    // any descendant). Returns empty string when none applies.
    [[nodiscard]] std::string resolveFeedId() const;
    [[nodiscard]] std::string resolveFxPairId() const;
    [[nodiscard]] int nextComponentIndex(const std::string& fxId) const;

    [[nodiscard]] NodeType currentNodeType() const;
    [[nodiscard]] std::string currentNodeId() const;

    ClientManager* clientManager_;
    QString username_;

    // Layout
    QSplitter* mainSplitter_;

    // Toolbar
    QToolBar* toolbar_;
    QAction* reloadAction_;
    QAction* newFeedAction_;
    QAction* newFxPairAction_;
    QAction* newComponentAction_;
    QAction* editAction_;
    QAction* deleteAction_;

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

    // In-memory copies keyed by id (uuid string).
    std::map<std::string, synthetic::domain::market_data_generation_config> feeds_;
    std::map<std::string, synthetic::domain::fx_spot_generation_config> fxPairs_;
    std::map<std::string, synthetic::domain::gmm_component> components_;

    bool loading_{false};
};

}

#endif
