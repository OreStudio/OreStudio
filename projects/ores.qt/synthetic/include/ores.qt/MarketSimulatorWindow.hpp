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
#include <QCheckBox>
#include <QDoubleSpinBox>
#include <QLabel>
#include <QLineEdit>
#include <QPlainTextEdit>
#include <QSpinBox>
#include <QSplitter>
#include <QStackedWidget>
#include <QStandardItemModel>
#include <QTabWidget>
#include <QTableWidget>
#include <QToolBar>
#include <QTreeView>
#include <QWidget>
#include <map>
#include <string>
#include <vector>

class QComboBox;

namespace ores::qt {

class ImageCache;

/**
 * @brief Composite MDI window for authoring synthetic market data feeds.
 *
 * The Market Simulator presents a three-level tree of feeds
 * (market_data_generation_config) > FX pairs (fx_spot_generation_config) >
 * GMM components (gmm_component) on the left, and a context-sensitive editor
 * on the right. It joins the three entity lists client-side and persists edits
 * back via the synthetic save protocols.
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
    void onReloadClicked();
    void onNewFeedClicked();
    void onNewFxPairClicked();
    void onNewComponentClicked();
    void onSaveClicked();
    void onDeleteClicked();
    void onCurrencyChanged();
    void onAddComponentRow();
    void onRemoveComponentRow();
    void applyPreset(const QString& preset);
    void recomputeWeightSum();

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

    void showFeedPage(const synthetic::domain::market_data_generation_config& feed);
    void showFxPairPage(const synthetic::domain::fx_spot_generation_config& fx);
    void populateGmmTable(const std::string& fx_id);
    void recomputeDerivedLabels();

    void populateCurrencyCombo(QComboBox* combo);

    [[nodiscard]] NodeType currentNodeType() const;
    [[nodiscard]] std::string currentNodeId() const;

    void saveFeed();
    void saveFxPair();

    ClientManager* clientManager_;
    QString username_;
    ImageCache* imageCache_;

    // Layout
    QSplitter* mainSplitter_;

    // Toolbar
    QToolBar* toolbar_;
    QAction* reloadAction_;
    QAction* newFeedAction_;
    QAction* newFxPairAction_;
    QAction* newComponentAction_;
    QAction* saveAction_;
    QAction* deleteAction_;

    // Left panel
    QTreeView* feedsTree_;
    QStandardItemModel* treeModel_;

    // Status bar
    QLabel* statusLabel_;

    // Right panel
    QStackedWidget* editorStack_;

    // Feed page
    QWidget* feedPage_;
    QLineEdit* feedNameEdit_;
    QPlainTextEdit* feedDescEdit_;
    QCheckBox* feedEnabledCheck_;

    // FX pair page
    QWidget* fxPage_;
    QTabWidget* fxTabs_;
    QComboBox* baseCurrencyCombo_;
    QComboBox* quoteCurrencyCombo_;
    QLabel* oreKeyLabel_;
    QLabel* sourceNameLabel_;
    QDoubleSpinBox* initialPriceSpin_;
    QSpinBox* ticksSpin_;
    QCheckBox* fxEnabledCheck_;
    QTableWidget* gmmTable_;
    QLabel* weightSumLabel_;

    // In-memory copies keyed by id (uuid string).
    std::map<std::string, synthetic::domain::market_data_generation_config> feeds_;
    std::map<std::string, synthetic::domain::fx_spot_generation_config> fxPairs_;
    std::map<std::string, synthetic::domain::gmm_component> components_;

    // The entity currently being edited.
    NodeType editingType_{NodeType::Feed};
    std::string editingId_;
    bool editingIsNew_{false};

    bool loading_{false};
};

}

#endif
