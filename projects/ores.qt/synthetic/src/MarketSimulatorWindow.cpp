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
#include "ores.qt/ComponentDialog.hpp"
#include "ores.qt/FeedDialog.hpp"
#include "ores.qt/FxPairDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
#include "ores.synthetic.api/messaging/market_data_generation_config_protocol.hpp"
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QMessageBox>
#include <QPointer>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <algorithm>
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

}

MarketSimulatorWindow::MarketSimulatorWindow(ClientManager* clientManager,
                                             const QString& username,
                                             QWidget* parent)
    : QWidget(parent)
    , clientManager_(clientManager)
    , username_(username)
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
    reloadAction_->setToolTip(tr("Reload all feeds, FX pairs and components"));

    toolbar_->addSeparator();

    newFeedAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor), tr("New Feed"));
    newFeedAction_->setToolTip(
        tr("New feed — a named market-data simulation you can enable and run."));

    newFxPairAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor), tr("New FX Pair"));
    newFxPairAction_->setToolTip(
        tr("New FX pair — the currency pair to simulate (e.g. EUR/USD); "
           "belongs to the selected feed."));

    newComponentAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor),
        tr("New Component"));
    newComponentAction_->setToolTip(
        tr("New GMM component — one normal distribution in the price model of "
           "the selected FX pair."));

    toolbar_->addSeparator();

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Edit, IconUtils::DefaultIconColor), tr("Edit"));
    editAction_->setToolTip(tr("Edit the selected item."));

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor), tr("Delete"));
    deleteAction_->setToolTip(tr("Delete the selected entity"));
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
    feedsTree_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    feedsTree_->setFrameShape(QFrame::StyledPanel);
    feedsTree_->setFrameShadow(QFrame::Sunken);
    leftLayout->addWidget(feedsTree_, 1);

    emptyHintLabel_->setWordWrap(true);
    emptyHintLabel_->setText(
        tr("No feeds yet. Click 'New Feed' to define a market-data feed, then "
           "add an FX pair (currency pair) and its price-model components."));
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
    auto* emptyLabel = new QLabel(tr("Select a feed, FX pair or component to see its details."),
                                  emptyPage);
    emptyLabel->setAlignment(Qt::AlignCenter);
    emptyLabel->setStyleSheet("color: gray;");
    emptyLayout->addWidget(emptyLabel);

    // Read-only summary page.
    summaryPage_ = new QWidget(this);
    auto* summaryLayout = new QVBoxLayout(summaryPage_);
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
    connect(newFxPairAction_, &QAction::triggered, this,
            &MarketSimulatorWindow::onNewFxPairClicked);
    connect(newComponentAction_, &QAction::triggered, this,
            &MarketSimulatorWindow::onNewComponentClicked);
    connect(editAction_, &QAction::triggered, this, &MarketSimulatorWindow::onEditClicked);
    connect(deleteAction_, &QAction::triggered, this, &MarketSimulatorWindow::onDeleteClicked);
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

void MarketSimulatorWindow::updateEmptyState() {
    emptyHintLabel_->setVisible(feeds_.empty());
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

std::string MarketSimulatorWindow::resolveFeedId() const {
    const auto type = currentNodeType();
    const auto id = currentNodeId();
    if (id.empty())
        return {};
    if (type == NodeType::Feed)
        return id;
    if (type == NodeType::FxPair) {
        auto it = fxPairs_.find(id);
        if (it != fxPairs_.end())
            return boost::uuids::to_string(it->second.config_id);
    } else if (type == NodeType::Component) {
        auto it = components_.find(id);
        if (it != components_.end()) {
            auto fxIt = fxPairs_.find(boost::uuids::to_string(it->second.fx_spot_config_id));
            if (fxIt != fxPairs_.end())
                return boost::uuids::to_string(fxIt->second.config_id);
        }
    }
    return {};
}

std::string MarketSimulatorWindow::resolveFxPairId() const {
    const auto type = currentNodeType();
    const auto id = currentNodeId();
    if (id.empty())
        return {};
    if (type == NodeType::FxPair)
        return id;
    if (type == NodeType::Component) {
        auto it = components_.find(id);
        if (it != components_.end())
            return boost::uuids::to_string(it->second.fx_spot_config_id);
    }
    return {};
}

int MarketSimulatorWindow::nextComponentIndex(const std::string& fxId) const {
    int next = 0;
    for (const auto& [compId, comp] : components_) {
        if (boost::uuids::to_string(comp.fx_spot_config_id) == fxId)
            next = std::max(next, comp.component_index + 1);
    }
    return next;
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
        case NodeType::Component: {
            auto it = components_.find(id);
            if (it != components_.end())
                showComponentSummary(it->second);
            break;
        }
    }
}

void MarketSimulatorWindow::showFeedSummary(
    const synthetic::domain::market_data_generation_config& feed) {
    clear_form(summaryForm_);
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

    summaryTitle_->setText(tr("<b>FX Pair</b>"));
    summaryForm_->addRow(
        tr("Pair"),
        new QLabel(QString::fromStdString(fx.base_currency_code) + "/" +
                       QString::fromStdString(fx.quote_currency_code),
                   summaryPage_));
    summaryForm_->addRow(
        tr("ORE key"), new QLabel(QString::fromStdString(fx.ore_key), summaryPage_));
    summaryForm_->addRow(
        tr("Source name"), new QLabel(QString::fromStdString(fx.source_name), summaryPage_));
    summaryForm_->addRow(
        tr("Initial price"), new QLabel(QString::number(fx.gmm_initial_price), summaryPage_));
    summaryForm_->addRow(
        tr("Ticks / hr"), new QLabel(QString::number(fx.ticks_per_hour), summaryPage_));
    summaryForm_->addRow(
        tr("Enabled"), new QLabel(fx.enabled ? tr("Yes") : tr("No"), summaryPage_));
    summaryForm_->addRow(
        tr("Components"), new QLabel(QString::number(componentCount), summaryPage_));
    summaryStack_->setCurrentWidget(summaryPage_);
}

void MarketSimulatorWindow::showComponentSummary(
    const synthetic::domain::gmm_component& comp) {
    clear_form(summaryForm_);
    summaryTitle_->setText(tr("<b>GMM Component</b>"));
    summaryForm_->addRow(
        tr("Index"), new QLabel(QString::number(comp.component_index), summaryPage_));
    summaryForm_->addRow(tr("Mean"), new QLabel(QString::number(comp.mean), summaryPage_));
    summaryForm_->addRow(tr("Stdev"), new QLabel(QString::number(comp.stdev), summaryPage_));
    summaryForm_->addRow(tr("Weight"), new QLabel(QString::number(comp.weight), summaryPage_));
    summaryStack_->setCurrentWidget(summaryPage_);
}

void MarketSimulatorWindow::onNewFeedClicked() {
    BOOST_LOG_SEV(lg(), info) << "Opening New Feed dialog.";
    FeedDialog dlg(clientManager_, username_, this);
    if (dlg.exec() == QDialog::Accepted)
        reload();
}

void MarketSimulatorWindow::onNewFxPairClicked() {
    const auto feedId = resolveFeedId();
    if (feedId.empty()) {
        emit errorOccurred(tr("Select a feed first."));
        QMessageBox::information(this, tr("Select a feed"),
                                 tr("Select a feed (or one of its items) before adding an FX "
                                    "pair."));
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Opening New FX Pair dialog under feed " << feedId << ".";
    FxPairDialog dlg(clientManager_, username_, uuid_from_string(feedId), this);
    if (dlg.exec() == QDialog::Accepted)
        reload();
}

void MarketSimulatorWindow::onNewComponentClicked() {
    const auto fxId = resolveFxPairId();
    if (fxId.empty()) {
        emit errorOccurred(tr("Select an FX pair first."));
        QMessageBox::information(this, tr("Select an FX pair"),
                                 tr("Select an FX pair (or one of its components) before adding "
                                    "a component."));
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Opening New Component dialog under fx pair " << fxId << ".";
    ComponentDialog dlg(
        clientManager_, username_, uuid_from_string(fxId), nextComponentIndex(fxId), this);
    if (dlg.exec() == QDialog::Accepted)
        reload();
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
            BOOST_LOG_SEV(lg(), info) << "Opening Edit FX Pair dialog for " << id << ".";
            FxPairDialog dlg(clientManager_, username_, it->second, this);
            if (dlg.exec() == QDialog::Accepted)
                reload();
            break;
        }
        case NodeType::Component: {
            auto it = components_.find(id);
            if (it == components_.end())
                return;
            BOOST_LOG_SEV(lg(), info) << "Opening Edit Component dialog for " << id << ".";
            ComponentDialog dlg(clientManager_, username_, it->second, this);
            if (dlg.exec() == QDialog::Accepted)
                reload();
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
    const bool hasFeedContext = !resolveFeedId().empty();
    const bool hasFxContext = !resolveFxPairId().empty();

    newFxPairAction_->setEnabled(hasFeedContext);
    newComponentAction_->setEnabled(hasFxContext);
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
}

}
