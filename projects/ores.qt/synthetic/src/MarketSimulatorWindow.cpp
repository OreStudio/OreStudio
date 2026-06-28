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
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
#include "ores.synthetic.api/messaging/market_data_generation_config_protocol.hpp"
#include <QComboBox>
#include <QFormLayout>
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QMessageBox>
#include <QPointer>
#include <QPushButton>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <algorithm>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cctype>

namespace ores::qt {

using namespace ores::logging;

namespace {

// Tree item roles.
constexpr int NodeTypeRole = Qt::UserRole + 1;
constexpr int NodeIdRole = Qt::UserRole + 2;

std::string to_lower(const std::string& s) {
    std::string r = s;
    std::transform(r.begin(), r.end(), r.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
    });
    return r;
}

std::string derive_ore_key(const std::string& base, const std::string& quote) {
    if (base.empty() || quote.empty())
        return {};
    return "FX/RATE/" + base + "/" + quote;
}

std::string derive_source_name(const std::string& base, const std::string& quote) {
    if (base.empty() || quote.empty())
        return {};
    return "synthetic." + to_lower(base) + to_lower(quote);
}

std::string new_uuid_string() {
    static boost::uuids::random_generator gen;
    return boost::uuids::to_string(gen());
}

boost::uuids::uuid uuid_from_string(const std::string& s) {
    try {
        return boost::lexical_cast<boost::uuids::uuid>(s);
    } catch (...) {
        return boost::uuids::uuid{};
    }
}

}

MarketSimulatorWindow::MarketSimulatorWindow(ClientManager* clientManager,
                                             const QString& username,
                                             QWidget* parent)
    : QWidget(parent)
    , clientManager_(clientManager)
    , username_(username)
    , imageCache_(new ImageCache(clientManager, this))
    , mainSplitter_(new QSplitter(Qt::Horizontal, this))
    , toolbar_(new QToolBar(this))
    , feedsTree_(new QTreeView(this))
    , treeModel_(new QStandardItemModel(this))
    , statusLabel_(new QLabel(this))
    , editorStack_(new QStackedWidget(this)) {

    BOOST_LOG_SEV(lg(), debug) << "Creating Market Simulator window";

    setupUi();
    setupToolbar();
    setupLeftPanel();
    setupRightPanel();
    setupConnections();

    imageCache_->loadAll();

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
    reloadAction_->setToolTip(tr("Reload all feeds, FX pairs and components"));

    toolbar_->addSeparator();

    newFeedAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor), tr("New Feed"));
    newFeedAction_->setToolTip(tr("Create a new market data feed"));

    newFxPairAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor), tr("New FX Pair"));
    newFxPairAction_->setToolTip(tr("Add an FX pair to the selected feed"));

    newComponentAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor),
        tr("New Component"));
    newComponentAction_->setToolTip(tr("Add a GMM component to the selected FX pair"));

    toolbar_->addSeparator();

    saveAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor), tr("Save"));
    saveAction_->setToolTip(tr("Save the currently edited entity"));

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor), tr("Delete"));
    deleteAction_->setToolTip(tr("Delete the selected entity"));
}

void MarketSimulatorWindow::setupLeftPanel() {
    auto* leftPanel = new QWidget(this);
    auto* leftLayout = new QVBoxLayout(leftPanel);
    leftLayout->setContentsMargins(0, 0, 0, 0);
    leftLayout->setSpacing(8);

    auto* label = new QLabel(tr("<b>Feeds</b>"), leftPanel);
    leftLayout->addWidget(label);

    feedsTree_->setModel(treeModel_);
    feedsTree_->setHeaderHidden(true);
    feedsTree_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    feedsTree_->setFrameShape(QFrame::StyledPanel);
    feedsTree_->setFrameShadow(QFrame::Sunken);
    leftLayout->addWidget(feedsTree_, 1);

    leftPanel->setMinimumWidth(280);
    leftPanel->setMaximumWidth(450);

    mainSplitter_->addWidget(leftPanel);
}

void MarketSimulatorWindow::setupRightPanel() {
    // --- Feed page ---
    feedPage_ = new QWidget(this);
    auto* feedForm = new QFormLayout(feedPage_);
    feedNameEdit_ = new QLineEdit(feedPage_);
    feedDescEdit_ = new QPlainTextEdit(feedPage_);
    feedEnabledCheck_ = new QCheckBox(tr("Enabled"), feedPage_);
    feedForm->addRow(tr("Name"), feedNameEdit_);
    feedForm->addRow(tr("Description"), feedDescEdit_);
    feedForm->addRow(QString(), feedEnabledCheck_);

    // --- FX pair page ---
    fxPage_ = new QWidget(this);
    auto* fxLayout = new QVBoxLayout(fxPage_);
    fxTabs_ = new QTabWidget(fxPage_);
    fxLayout->addWidget(fxTabs_);

    // Instrument tab.
    auto* instrumentTab = new QWidget(fxTabs_);
    auto* instrumentForm = new QFormLayout(instrumentTab);
    baseCurrencyCombo_ = new QComboBox(instrumentTab);
    quoteCurrencyCombo_ = new QComboBox(instrumentTab);
    oreKeyLabel_ = new QLabel(instrumentTab);
    oreKeyLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    sourceNameLabel_ = new QLabel(instrumentTab);
    sourceNameLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    initialPriceSpin_ = new QDoubleSpinBox(instrumentTab);
    initialPriceSpin_->setRange(0.0001, 1e9);
    initialPriceSpin_->setDecimals(4);
    initialPriceSpin_->setValue(1.0);
    ticksSpin_ = new QSpinBox(instrumentTab);
    ticksSpin_->setRange(1, 100000);
    ticksSpin_->setValue(60);
    fxEnabledCheck_ = new QCheckBox(tr("Enabled"), instrumentTab);
    instrumentForm->addRow(tr("Base currency"), baseCurrencyCombo_);
    instrumentForm->addRow(tr("Quote currency"), quoteCurrencyCombo_);
    instrumentForm->addRow(tr("ORE key"), oreKeyLabel_);
    instrumentForm->addRow(tr("Source name"), sourceNameLabel_);
    instrumentForm->addRow(tr("Initial price"), initialPriceSpin_);
    instrumentForm->addRow(tr("Ticks / hr"), ticksSpin_);
    instrumentForm->addRow(QString(), fxEnabledCheck_);
    fxTabs_->addTab(instrumentTab, tr("Instrument"));

    // Price model (GMM) tab.
    auto* gmmTab = new QWidget(fxTabs_);
    auto* gmmLayout = new QVBoxLayout(gmmTab);
    gmmTable_ = new QTableWidget(gmmTab);
    gmmTable_->setColumnCount(4);
    gmmTable_->setHorizontalHeaderLabels(
        {tr("Component"), tr("Mean"), tr("Stdev"), tr("Weight")});
    gmmTable_->horizontalHeader()->setStretchLastSection(true);
    gmmTable_->verticalHeader()->setVisible(false);
    gmmLayout->addWidget(gmmTable_, 1);

    auto* gmmButtons = new QHBoxLayout();
    auto* addCompBtn = new QPushButton(tr("Add component"), gmmTab);
    auto* removeCompBtn = new QPushButton(tr("Remove component"), gmmTab);
    auto* calmBtn = new QPushButton(tr("Calm"), gmmTab);
    auto* normalBtn = new QPushButton(tr("Normal"), gmmTab);
    auto* volatileBtn = new QPushButton(tr("Volatile"), gmmTab);
    gmmButtons->addWidget(addCompBtn);
    gmmButtons->addWidget(removeCompBtn);
    gmmButtons->addSpacing(16);
    gmmButtons->addWidget(calmBtn);
    gmmButtons->addWidget(normalBtn);
    gmmButtons->addWidget(volatileBtn);
    gmmButtons->addStretch(1);
    gmmLayout->addLayout(gmmButtons);

    weightSumLabel_ = new QLabel(gmmTab);
    gmmLayout->addWidget(weightSumLabel_);

    fxTabs_->addTab(gmmTab, tr("Price model (GMM)"));

    connect(addCompBtn, &QPushButton::clicked, this,
            &MarketSimulatorWindow::onAddComponentRow);
    connect(removeCompBtn, &QPushButton::clicked, this,
            &MarketSimulatorWindow::onRemoveComponentRow);
    connect(calmBtn, &QPushButton::clicked, this, [this]() { applyPreset("Calm"); });
    connect(normalBtn, &QPushButton::clicked, this, [this]() { applyPreset("Normal"); });
    connect(volatileBtn, &QPushButton::clicked, this, [this]() { applyPreset("Volatile"); });
    connect(gmmTable_, &QTableWidget::itemChanged, this,
            [this](QTableWidgetItem*) { recomputeWeightSum(); });

    // --- empty/default page ---
    auto* emptyPage = new QWidget(this);
    auto* emptyLayout = new QVBoxLayout(emptyPage);
    auto* emptyLabel = new QLabel(tr("Select a feed, FX pair or component."), emptyPage);
    emptyLabel->setAlignment(Qt::AlignCenter);
    emptyLayout->addWidget(emptyLabel);

    editorStack_->addWidget(emptyPage); // index 0
    editorStack_->addWidget(feedPage_); // index 1
    editorStack_->addWidget(fxPage_);   // index 2

    auto* rightPanel = new QWidget(this);
    auto* rightLayout = new QVBoxLayout(rightPanel);
    rightLayout->setContentsMargins(8, 8, 8, 8);
    auto* editorLabel = new QLabel(tr("<b>Editor</b>"), rightPanel);
    rightLayout->addWidget(editorLabel);
    rightLayout->addWidget(editorStack_, 1);

    mainSplitter_->addWidget(rightPanel);
    mainSplitter_->setSizes({350, 1150});

    populateCurrencyCombo(baseCurrencyCombo_);
    populateCurrencyCombo(quoteCurrencyCombo_);
}

void MarketSimulatorWindow::setupConnections() {
    connect(feedsTree_->selectionModel(), &QItemSelectionModel::currentChanged, this,
            &MarketSimulatorWindow::onTreeSelectionChanged);

    connect(reloadAction_, &QAction::triggered, this,
            &MarketSimulatorWindow::onReloadClicked);
    connect(newFeedAction_, &QAction::triggered, this,
            &MarketSimulatorWindow::onNewFeedClicked);
    connect(newFxPairAction_, &QAction::triggered, this,
            &MarketSimulatorWindow::onNewFxPairClicked);
    connect(newComponentAction_, &QAction::triggered, this,
            &MarketSimulatorWindow::onNewComponentClicked);
    connect(saveAction_, &QAction::triggered, this, &MarketSimulatorWindow::onSaveClicked);
    connect(deleteAction_, &QAction::triggered, this, &MarketSimulatorWindow::onDeleteClicked);

    connect(baseCurrencyCombo_, &QComboBox::currentTextChanged, this,
            &MarketSimulatorWindow::onCurrencyChanged);
    connect(quoteCurrencyCombo_, &QComboBox::currentTextChanged, this,
            &MarketSimulatorWindow::onCurrencyChanged);
}

void MarketSimulatorWindow::populateCurrencyCombo(QComboBox* combo) {
    if (!clientManager_ || !clientManager_->isConnected())
        return;

    QPointer<MarketSimulatorWindow> self = this;
    QPointer<QComboBox> target = combo;
    auto* cm = clientManager_;

    auto task = [cm]() -> std::vector<std::string> {
        return fetch_currency_codes(cm);
    };

    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    connect(watcher, &QFutureWatcher<std::vector<std::string>>::finished, self,
            [self, target, watcher]() {
                auto codes = watcher->result();
                watcher->deleteLater();
                if (!self || !target)
                    return;

                const QSignalBlocker blocker(target);
                target->clear();
                target->addItem(QString()); // "(select)" sentinel
                for (const auto& code : codes) {
                    target->addItem(QString::fromStdString(code));
                }

                apply_flag_icons(target, self->imageCache_, FlagSource::Currency);
            });

    watcher->setFuture(QtConcurrent::run(task));
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

    BOOST_LOG_SEV(lg(), debug) << "Reloading feeds, fx pairs and components.";

    loading_ = true;
    statusLabel_->setText(tr("Loading..."));

    struct FetchResult {
        bool success = false;
        std::vector<synthetic::domain::market_data_generation_config> feeds;
        std::vector<synthetic::domain::fx_spot_generation_config> fxPairs;
        std::vector<synthetic::domain::gmm_component> components;
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
        self->editorStack_->setCurrentIndex(0);
        self->editingId_.clear();
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
            IconUtils::createRecoloredIcon(Icon::Database, IconUtils::DefaultIconColor));

        for (const auto& [fxId, fx] : fxPairs_) {
            if (boost::uuids::to_string(fx.config_id) != feedId)
                continue;

            QString fxText = QString::fromStdString(fx.base_currency_code) + "/" +
                QString::fromStdString(fx.quote_currency_code);
            auto* fxItem = new QStandardItem(fxText);
            fxItem->setData(static_cast<int>(NodeType::FxPair), NodeTypeRole);
            fxItem->setData(QString::fromStdString(fxId), NodeIdRole);
            fxItem->setIcon(
                IconUtils::createRecoloredIcon(Icon::Currency, IconUtils::DefaultIconColor));

            // Collect components for this fx, ordered by component_index.
            std::vector<const synthetic::domain::gmm_component*> comps;
            for (const auto& [compId, comp] : components_) {
                if (boost::uuids::to_string(comp.fx_spot_config_id) == fxId)
                    comps.push_back(&comp);
            }
            std::sort(comps.begin(), comps.end(), [](const auto* a, const auto* b) {
                return a->component_index < b->component_index;
            });

            for (const auto* comp : comps) {
                QString compText = tr("Component %1  (μ=%2, σ=%3, w=%4)")
                                       .arg(comp->component_index)
                                       .arg(comp->mean)
                                       .arg(comp->stdev)
                                       .arg(comp->weight);
                auto* compItem = new QStandardItem(compText);
                compItem->setData(static_cast<int>(NodeType::Component), NodeTypeRole);
                compItem->setData(
                    QString::fromStdString(boost::uuids::to_string(comp->id)), NodeIdRole);
                compItem->setIcon(
                    IconUtils::createRecoloredIcon(Icon::Record, IconUtils::DefaultIconColor));
                fxItem->appendRow(compItem);
            }

            feedItem->appendRow(fxItem);
        }

        root->appendRow(feedItem);
    }

    feedsTree_->expandAll();
}

void MarketSimulatorWindow::updateStatusCounts() {
    statusLabel_->setText(tr("%1 feeds, %2 FX pairs, %3 components")
                              .arg(feeds_.size())
                              .arg(fxPairs_.size())
                              .arg(components_.size()));
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

void MarketSimulatorWindow::onTreeSelectionChanged(const QModelIndex& current,
                                                   const QModelIndex& /*previous*/) {
    if (!current.isValid()) {
        editorStack_->setCurrentIndex(0);
        editingId_.clear();
        updateToolbarState();
        return;
    }

    const auto type = static_cast<NodeType>(current.data(NodeTypeRole).toInt());
    const auto id = current.data(NodeIdRole).toString().toStdString();

    switch (type) {
        case NodeType::Feed: {
            auto it = feeds_.find(id);
            if (it != feeds_.end())
                showFeedPage(it->second);
            break;
        }
        case NodeType::FxPair: {
            auto it = fxPairs_.find(id);
            if (it != fxPairs_.end())
                showFxPairPage(it->second);
            break;
        }
        case NodeType::Component: {
            // Select parent FX pair, show GMM tab, highlight the component row.
            auto compIt = components_.find(id);
            if (compIt == components_.end())
                break;
            const auto fxId = boost::uuids::to_string(compIt->second.fx_spot_config_id);
            auto fxIt = fxPairs_.find(fxId);
            if (fxIt != fxPairs_.end()) {
                showFxPairPage(fxIt->second);
                fxTabs_->setCurrentIndex(1); // GMM tab
                for (int r = 0; r < gmmTable_->rowCount(); ++r) {
                    auto* item = gmmTable_->item(r, 0);
                    if (item &&
                        item->data(Qt::UserRole).toString().toStdString() == id) {
                        gmmTable_->selectRow(r);
                        break;
                    }
                }
            }
            break;
        }
    }

    updateToolbarState();
}

void MarketSimulatorWindow::showFeedPage(
    const synthetic::domain::market_data_generation_config& feed) {
    editingType_ = NodeType::Feed;
    editingId_ = boost::uuids::to_string(feed.id);
    editingIsNew_ = false;

    feedNameEdit_->setText(QString::fromStdString(feed.name));
    feedDescEdit_->setPlainText(QString::fromStdString(feed.description));
    feedEnabledCheck_->setChecked(feed.enabled);

    editorStack_->setCurrentWidget(feedPage_);
}

void MarketSimulatorWindow::showFxPairPage(
    const synthetic::domain::fx_spot_generation_config& fx) {
    editingType_ = NodeType::FxPair;
    editingId_ = boost::uuids::to_string(fx.id);
    editingIsNew_ = false;

    {
        const QSignalBlocker b1(baseCurrencyCombo_);
        const QSignalBlocker b2(quoteCurrencyCombo_);
        baseCurrencyCombo_->setCurrentText(QString::fromStdString(fx.base_currency_code));
        quoteCurrencyCombo_->setCurrentText(QString::fromStdString(fx.quote_currency_code));
    }
    initialPriceSpin_->setValue(fx.gmm_initial_price > 0 ? fx.gmm_initial_price : 1.0);
    ticksSpin_->setValue(fx.ticks_per_hour > 0 ? fx.ticks_per_hour : 60);
    fxEnabledCheck_->setChecked(fx.enabled);

    recomputeDerivedLabels();
    populateGmmTable(editingId_);

    editorStack_->setCurrentWidget(fxPage_);
    fxTabs_->setCurrentIndex(0);
}

void MarketSimulatorWindow::populateGmmTable(const std::string& fx_id) {
    const QSignalBlocker blocker(gmmTable_);
    gmmTable_->setRowCount(0);

    std::vector<const synthetic::domain::gmm_component*> comps;
    for (const auto& [compId, comp] : components_) {
        if (boost::uuids::to_string(comp.fx_spot_config_id) == fx_id)
            comps.push_back(&comp);
    }
    std::sort(comps.begin(), comps.end(), [](const auto* a, const auto* b) {
        return a->component_index < b->component_index;
    });

    for (const auto* comp : comps) {
        const int row = gmmTable_->rowCount();
        gmmTable_->insertRow(row);

        auto* idxItem = new QTableWidgetItem(QString::number(comp->component_index));
        idxItem->setFlags(idxItem->flags() & ~Qt::ItemIsEditable);
        idxItem->setData(Qt::UserRole,
                         QString::fromStdString(boost::uuids::to_string(comp->id)));
        gmmTable_->setItem(row, 0, idxItem);
        gmmTable_->setItem(row, 1, new QTableWidgetItem(QString::number(comp->mean)));
        gmmTable_->setItem(row, 2, new QTableWidgetItem(QString::number(comp->stdev)));
        gmmTable_->setItem(row, 3, new QTableWidgetItem(QString::number(comp->weight)));
    }

    recomputeWeightSum();
}

void MarketSimulatorWindow::recomputeDerivedLabels() {
    const auto base = baseCurrencyCombo_->currentText().toStdString();
    const auto quote = quoteCurrencyCombo_->currentText().toStdString();
    oreKeyLabel_->setText(QString::fromStdString(derive_ore_key(base, quote)));
    sourceNameLabel_->setText(QString::fromStdString(derive_source_name(base, quote)));
}

void MarketSimulatorWindow::onCurrencyChanged() {
    recomputeDerivedLabels();
}

void MarketSimulatorWindow::recomputeWeightSum() {
    double sum = 0.0;
    for (int r = 0; r < gmmTable_->rowCount(); ++r) {
        if (auto* item = gmmTable_->item(r, 3))
            sum += item->text().toDouble();
    }
    const bool ok = std::abs(sum - 1.0) < 0.001;
    weightSumLabel_->setText(tr("Weight sum: %1 %2")
                                 .arg(sum, 0, 'f', 4)
                                 .arg(ok ? tr("(≈ 1.0)") : tr("(not normalised)")));
}

void MarketSimulatorWindow::onAddComponentRow() {
    const int row = gmmTable_->rowCount();
    const QSignalBlocker blocker(gmmTable_);
    gmmTable_->insertRow(row);

    auto* idxItem = new QTableWidgetItem(QString::number(row));
    idxItem->setFlags(idxItem->flags() & ~Qt::ItemIsEditable);
    idxItem->setData(Qt::UserRole, QString::fromStdString(new_uuid_string()));
    gmmTable_->setItem(row, 0, idxItem);
    gmmTable_->setItem(row, 1, new QTableWidgetItem(QStringLiteral("0")));
    gmmTable_->setItem(row, 2, new QTableWidgetItem(QStringLiteral("0.001")));
    gmmTable_->setItem(row, 3, new QTableWidgetItem(QStringLiteral("0")));
    recomputeWeightSum();
}

void MarketSimulatorWindow::onRemoveComponentRow() {
    const int row = gmmTable_->currentRow();
    if (row < 0)
        return;
    gmmTable_->removeRow(row);
    recomputeWeightSum();
}

void MarketSimulatorWindow::applyPreset(const QString& preset) {
    std::vector<std::tuple<double, double, double>> rows; // mean, stdev, weight
    if (preset == "Calm") {
        rows = {{0.0, 0.0005, 1.0}};
    } else if (preset == "Normal") {
        rows = {{0.0, 0.001, 0.8}, {0.0, 0.003, 0.2}};
    } else if (preset == "Volatile") {
        rows = {{0.0, 0.002, 0.6}, {0.0, 0.008, 0.4}};
    }

    const QSignalBlocker blocker(gmmTable_);
    gmmTable_->setRowCount(0);
    int index = 0;
    for (const auto& [mean, stdev, weight] : rows) {
        const int row = gmmTable_->rowCount();
        gmmTable_->insertRow(row);
        auto* idxItem = new QTableWidgetItem(QString::number(index));
        idxItem->setFlags(idxItem->flags() & ~Qt::ItemIsEditable);
        idxItem->setData(Qt::UserRole, QString::fromStdString(new_uuid_string()));
        gmmTable_->setItem(row, 0, idxItem);
        gmmTable_->setItem(row, 1, new QTableWidgetItem(QString::number(mean)));
        gmmTable_->setItem(row, 2, new QTableWidgetItem(QString::number(stdev)));
        gmmTable_->setItem(row, 3, new QTableWidgetItem(QString::number(weight)));
        ++index;
    }
    recomputeWeightSum();
}

void MarketSimulatorWindow::onNewFeedClicked() {
    synthetic::domain::market_data_generation_config feed;
    feed.id = boost::uuids::random_generator()();
    feed.party_id = boost::uuids::uuid{};
    feed.enabled = true;

    editingType_ = NodeType::Feed;
    editingId_ = boost::uuids::to_string(feed.id);
    editingIsNew_ = true;
    feeds_[editingId_] = feed;

    BOOST_LOG_SEV(lg(), info) << "Creating new feed " << editingId_ << ".";

    feedNameEdit_->clear();
    feedDescEdit_->clear();
    feedEnabledCheck_->setChecked(true);
    editorStack_->setCurrentWidget(feedPage_);
    feedsTree_->clearSelection();
    updateToolbarState();
}

void MarketSimulatorWindow::onNewFxPairClicked() {
    // Resolve the selected feed (feed node, or ancestor of selection).
    std::string feedId;
    const auto type = currentNodeType();
    const auto id = currentNodeId();
    if (type == NodeType::Feed) {
        feedId = id;
    } else if (type == NodeType::FxPair) {
        auto it = fxPairs_.find(id);
        if (it != fxPairs_.end())
            feedId = boost::uuids::to_string(it->second.config_id);
    } else if (type == NodeType::Component) {
        auto it = components_.find(id);
        if (it != components_.end()) {
            auto fxIt = fxPairs_.find(boost::uuids::to_string(it->second.fx_spot_config_id));
            if (fxIt != fxPairs_.end())
                feedId = boost::uuids::to_string(fxIt->second.config_id);
        }
    }
    if (feedId.empty()) {
        emit errorOccurred(tr("Select a feed first."));
        return;
    }

    synthetic::domain::fx_spot_generation_config fx;
    fx.id = boost::uuids::random_generator()();
    fx.config_id = uuid_from_string(feedId);
    fx.gmm_initial_price = 1.0;
    fx.ticks_per_hour = 60;
    fx.enabled = true;

    editingType_ = NodeType::FxPair;
    editingId_ = boost::uuids::to_string(fx.id);
    editingIsNew_ = true;
    fxPairs_[editingId_] = fx;

    BOOST_LOG_SEV(lg(), info)
        << "Creating new fx pair " << editingId_ << " under feed " << feedId << ".";

    showFxPairPage(fx);
    editingIsNew_ = true; // showFxPairPage resets it
    feedsTree_->clearSelection();
    updateToolbarState();
}

void MarketSimulatorWindow::onNewComponentClicked() {
    // Resolve the selected fx pair.
    std::string fxId;
    const auto type = currentNodeType();
    const auto id = currentNodeId();
    if (editingType_ == NodeType::FxPair && !editingId_.empty()) {
        fxId = editingId_;
    } else if (type == NodeType::FxPair) {
        fxId = id;
    } else if (type == NodeType::Component) {
        auto it = components_.find(id);
        if (it != components_.end())
            fxId = boost::uuids::to_string(it->second.fx_spot_config_id);
    }
    if (fxId.empty()) {
        emit errorOccurred(tr("Select an FX pair first."));
        return;
    }

    BOOST_LOG_SEV(lg(), info)
        << "Creating new component under fx pair " << fxId << ".";

    // Ensure the FX page is shown and table is current for that fx.
    if (editingId_ != fxId) {
        auto fxIt = fxPairs_.find(fxId);
        if (fxIt != fxPairs_.end())
            showFxPairPage(fxIt->second);
    }
    fxTabs_->setCurrentIndex(1);
    onAddComponentRow();
    updateToolbarState();
}

void MarketSimulatorWindow::onSaveClicked() {
    if (editingId_.empty()) {
        emit errorOccurred(tr("Nothing to save."));
        return;
    }
    BOOST_LOG_SEV(lg(), info) << "Save requested for "
                              << (editingType_ == NodeType::Feed ? "feed" : "fx pair") << " "
                              << editingId_ << " (new=" << editingIsNew_ << ").";
    if (editingType_ == NodeType::Feed)
        saveFeed();
    else
        saveFxPair();
}

void MarketSimulatorWindow::saveFeed() {
    auto it = feeds_.find(editingId_);
    if (it == feeds_.end())
        return;

    auto feed = it->second;
    feed.name = feedNameEdit_->text().toStdString();
    feed.description = feedDescEdit_->toPlainText().toStdString();
    feed.enabled = feedEnabledCheck_->isChecked();
    feed.modified_by = username_.toStdString();
    feed.change_reason_code =
        editingIsNew_ ? "system.new_record" : "common.non_material_update";
    feed.change_commentary = "Authored via Market Simulator";
    feed.version = 0;

    const std::string savedId = editingId_;
    const bool isNew = editingIsNew_;
    BOOST_LOG_SEV(lg(), info) << "Saving feed " << savedId << " (new=" << isNew << ").";

    QPointer<MarketSimulatorWindow> self = this;
    auto* cm = clientManager_;

    auto task = [cm, feed]() -> std::pair<bool, QString> {
        auto resp = cm->process_authenticated_request(
            synthetic::messaging::save_market_data_generation_config_request::from(feed));
        if (!resp)
            return {false, QString::fromStdString(resp.error())};
        if (!resp->success)
            return {false, QString::fromStdString(resp->message)};
        return {true, {}};
    };

    auto* watcher = new QFutureWatcher<std::pair<bool, QString>>(self);
    connect(watcher, &QFutureWatcher<std::pair<bool, QString>>::finished, self,
            [self, watcher, savedId]() {
                auto [ok, err] = watcher->result();
                watcher->deleteLater();
                if (!self)
                    return;
                if (!ok) {
                    BOOST_LOG_SEV(lg(), error) << "Save failed for feed " << savedId << ": "
                                               << err.toStdString();
                    emit self->errorOccurred(err);
                    QMessageBox::critical(self, self->tr("Save failed"), err);
                    return;
                }
                BOOST_LOG_SEV(lg(), info) << "Saved feed " << savedId << ".";
                self->statusLabel_->setText(self->tr("Feed saved."));
                emit self->statusChanged(self->tr("Feed saved."));
                self->reload();
            });
    watcher->setFuture(QtConcurrent::run(task));
}

void MarketSimulatorWindow::saveFxPair() {
    auto it = fxPairs_.find(editingId_);
    if (it == fxPairs_.end())
        return;

    auto fx = it->second;
    const auto base = baseCurrencyCombo_->currentText().toStdString();
    const auto quote = quoteCurrencyCombo_->currentText().toStdString();
    if (base.empty() || quote.empty()) {
        emit errorOccurred(tr("Both base and quote currencies must be set."));
        return;
    }
    fx.base_currency_code = base;
    fx.quote_currency_code = quote;
    fx.ore_key = derive_ore_key(base, quote);
    fx.source_name = derive_source_name(base, quote);
    fx.gmm_initial_price = initialPriceSpin_->value();
    fx.ticks_per_hour = ticksSpin_->value();
    fx.enabled = fxEnabledCheck_->isChecked();
    fx.modified_by = username_.toStdString();
    fx.change_reason_code =
        editingIsNew_ ? "system.new_record" : "common.non_material_update";
    fx.change_commentary = "Authored via Market Simulator";
    fx.version = 0;

    // Gather GMM rows from the table and normalise weights to sum to 1.0.
    struct GmmRow {
        std::string id;
        int index;
        double mean;
        double stdev;
        double weight;
    };
    std::vector<GmmRow> rows;
    double total = 0.0;
    for (int r = 0; r < gmmTable_->rowCount(); ++r) {
        GmmRow gr;
        auto* idItem = gmmTable_->item(r, 0);
        gr.id = idItem ? idItem->data(Qt::UserRole).toString().toStdString() : new_uuid_string();
        if (gr.id.empty())
            gr.id = new_uuid_string();
        gr.index = r;
        gr.mean = gmmTable_->item(r, 1) ? gmmTable_->item(r, 1)->text().toDouble() : 0.0;
        gr.stdev = gmmTable_->item(r, 2) ? gmmTable_->item(r, 2)->text().toDouble() : 0.0;
        gr.weight = gmmTable_->item(r, 3) ? gmmTable_->item(r, 3)->text().toDouble() : 0.0;
        total += gr.weight;
        rows.push_back(gr);
    }
    if (total > 0.0) {
        for (auto& gr : rows)
            gr.weight /= total;
    }

    // Build gmm_component domain objects.
    const auto fxIdStr = editingId_;
    std::vector<synthetic::domain::gmm_component> comps;
    for (const auto& gr : rows) {
        synthetic::domain::gmm_component c;
        const auto existing = components_.find(gr.id);
        const bool isNew = (existing == components_.end());
        c.id = uuid_from_string(gr.id);
        c.fx_spot_config_id = fx.id;
        c.component_index = gr.index;
        c.mean = gr.mean;
        c.stdev = gr.stdev;
        c.weight = gr.weight;
        c.modified_by = username_.toStdString();
        c.change_reason_code = isNew ? "system.new_record" : "common.non_material_update";
        c.change_commentary = "Authored via Market Simulator";
        c.version = 0;
        comps.push_back(c);
    }

    const std::string savedId = editingId_;
    const bool isNewFx = editingIsNew_;
    BOOST_LOG_SEV(lg(), info) << "Saving fx pair " << savedId << " (new=" << isNewFx << ") with "
                              << comps.size() << " gmm components.";

    QPointer<MarketSimulatorWindow> self = this;
    auto* cm = clientManager_;

    auto task = [cm, fx, comps]() -> std::pair<bool, QString> {
        namespace m = synthetic::messaging;
        auto fxResp = cm->process_authenticated_request(
            m::save_fx_spot_generation_config_request::from(fx));
        if (!fxResp)
            return {false, QString::fromStdString(fxResp.error())};
        if (!fxResp->success)
            return {false, QString::fromStdString(fxResp->message)};

        for (const auto& c : comps) {
            auto cResp = cm->process_authenticated_request(
                m::save_gmm_component_request::from(c));
            if (!cResp)
                return {false, QString::fromStdString(cResp.error())};
            if (!cResp->success)
                return {false, QString::fromStdString(cResp->message)};
        }
        return {true, {}};
    };

    auto* watcher = new QFutureWatcher<std::pair<bool, QString>>(self);
    connect(watcher, &QFutureWatcher<std::pair<bool, QString>>::finished, self,
            [self, watcher, savedId]() {
                auto [ok, err] = watcher->result();
                watcher->deleteLater();
                if (!self)
                    return;
                if (!ok) {
                    BOOST_LOG_SEV(lg(), error) << "Save failed for fx pair " << savedId << ": "
                                               << err.toStdString();
                    emit self->errorOccurred(err);
                    QMessageBox::critical(self, self->tr("Save failed"), err);
                    return;
                }
                BOOST_LOG_SEV(lg(), info) << "Saved fx pair " << savedId << ".";
                self->statusLabel_->setText(self->tr("FX pair saved."));
                emit self->statusChanged(self->tr("FX pair saved."));
                self->reload();
            });
    watcher->setFuture(QtConcurrent::run(task));
}

void MarketSimulatorWindow::onDeleteClicked() {
    const auto type = currentNodeType();
    const auto id = currentNodeId();
    if (id.empty()) {
        emit errorOccurred(tr("Nothing to delete."));
        return;
    }

    const char* typeName =
        type == NodeType::Feed ? "feed" : (type == NodeType::FxPair ? "fx pair" : "component");
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
        } else if (type == NodeType::FxPair) {
            auto resp = cm->process_authenticated_request(
                m::delete_fx_spot_generation_config_request{.ids = {id}});
            if (!resp)
                return {false, QString::fromStdString(resp.error())};
            if (!resp->success)
                return {false, QString::fromStdString(resp->message)};
        } else {
            auto resp =
                cm->process_authenticated_request(m::delete_gmm_component_request{.ids = {id}});
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
    const bool hasEditing = !editingId_.empty();
    const auto type = currentNodeType();

    const bool feedContext = hasSelection &&
        (type == NodeType::Feed || type == NodeType::FxPair || type == NodeType::Component);
    const bool fxContext = hasSelection &&
        (type == NodeType::FxPair || type == NodeType::Component);

    newFxPairAction_->setEnabled(feedContext || editingType_ == NodeType::Feed);
    newComponentAction_->setEnabled(fxContext || editingType_ == NodeType::FxPair);
    saveAction_->setEnabled(hasEditing);
    deleteAction_->setEnabled(hasSelection);
}

}
