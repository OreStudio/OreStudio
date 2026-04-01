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
#include "ores.qt/MarketFixingsMdiWindow.hpp"

#include <QHeaderView>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/EntityItemDelegate.hpp"

namespace ores::qt {

using namespace ores::logging;

MarketFixingsMdiWindow::MarketFixingsMdiWindow(
    ClientManager* clientManager,
    const QString& username,
    QWidget* parent)
    : EntityListMdiWindow(parent),
      verticalLayout_(new QVBoxLayout(this)),
      tableView_(new QTableView(this)),
      toolBar_(new QToolBar(this)),
      paginationWidget_(new PaginationWidget(this)),
      reloadAction_(new QAction("Reload", this)),
      viewFixingsAction_(new QAction("View Fixings", this)),
      model_(std::make_unique<ClientMarketSeriesModel>(clientManager)),
      proxyModel_(new QSortFilterProxyModel(this)),
      clientManager_(clientManager),
      username_(username) {

    BOOST_LOG_SEV(lg(), debug) << "Creating market fixings MDI window";
    model_->set_series_type_filter("FIXING");
    setupUi();
}

void MarketFixingsMdiWindow::setupUi() {
    verticalLayout_->setContentsMargins(0, 0, 0, 0);
    verticalLayout_->setSpacing(0);

    setupToolbar();
    verticalLayout_->addWidget(toolBar_);
    verticalLayout_->addWidget(loadingBar());

    // Table setup
    proxyModel_->setSourceModel(model_.get());
    tableView_->setModel(proxyModel_);
    tableView_->setAlternatingRowColors(true);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    tableView_->setWordWrap(false);
    tableView_->setSortingEnabled(true);
    tableView_->verticalHeader()->setVisible(false);
    tableView_->setItemDelegate(new EntityItemDelegate(this));

    initializeTableSettings(tableView_, model_.get(),
        "MarketFixingsListWindow", {}, {1100, 600}, 1);

    verticalLayout_->addWidget(tableView_);
    verticalLayout_->addWidget(paginationWidget_);

    // Connections
    connect(model_.get(), &ClientMarketSeriesModel::dataLoaded,
            this, &MarketFixingsMdiWindow::onDataLoaded);
    connect(model_.get(), &ClientMarketSeriesModel::loadError,
            this, &MarketFixingsMdiWindow::onLoadError);
    connect(tableView_, &QTableView::doubleClicked,
            this, &MarketFixingsMdiWindow::onRowDoubleClicked);
    connect(tableView_->selectionModel(),
            &QItemSelectionModel::selectionChanged,
            this, &MarketFixingsMdiWindow::onSelectionChanged);

    connect(paginationWidget_, &PaginationWidget::page_size_changed,
            this, [this](std::uint32_t size) {
        model_->set_page_size(size);
        model_->refresh();
    });
    connect(paginationWidget_, &PaginationWidget::page_requested,
            this, [this](std::uint32_t offset, std::uint32_t limit) {
        model_->load_page(offset, limit);
    });
    connect(paginationWidget_, &PaginationWidget::load_all_requested,
            this, [this]() {
        const auto total = model_->total_available_count();
        if (total > 0 && total <= 5000) {
            model_->set_page_size(total);
            model_->refresh();
        }
    });

    if (clientManager_) {
        connect(clientManager_, &ClientManager::connectionStateChanged,
                this, &MarketFixingsMdiWindow::onConnectionStateChanged);
        if (clientManager_->isConnected())
            model_->refresh();
    }

    updateActionStates();
}

void MarketFixingsMdiWindow::setupToolbar() {
    toolBar_->setMovable(false);
    toolBar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);

    const QColor iconColor = color_constants::icon_color;

    reloadAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowSync, iconColor));
    reloadAction_->setToolTip(tr("Refresh fixing series"));
    connect(reloadAction_, &QAction::triggered,
            this, &MarketFixingsMdiWindow::doReload);
    toolBar_->addAction(reloadAction_);
    initializeStaleIndicator(reloadAction_, Icon::ArrowSync);

    toolBar_->addSeparator();

    viewFixingsAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::Chart, iconColor));
    viewFixingsAction_->setToolTip(tr("View fixing history for selected index"));
    viewFixingsAction_->setEnabled(false);
    connect(viewFixingsAction_, &QAction::triggered,
            this, &MarketFixingsMdiWindow::viewFixings);
    toolBar_->addAction(viewFixingsAction_);
}

void MarketFixingsMdiWindow::updateActionStates() {
    const bool hasSelection =
        tableView_->selectionModel() &&
        tableView_->selectionModel()->hasSelection();
    viewFixingsAction_->setEnabled(hasSelection);
}

void MarketFixingsMdiWindow::doReload() {
    clearStaleIndicator();
    model_->refresh();
}

void MarketFixingsMdiWindow::onDataLoaded() {
    const auto loaded = model_->rowCount();
    const auto total  = static_cast<int>(model_->total_available_count());
    paginationWidget_->update_state(loaded, total);
    paginationWidget_->set_load_all_enabled(
        loaded < total && total > 0 && total <= 5000);
    emit statusChanged(tr("Loaded %1 of %2 fixing series")
        .arg(loaded).arg(total));
}

void MarketFixingsMdiWindow::onLoadError(
    const QString& error_message, const QString& /*details*/) {
    emit errorOccurred(error_message);
}

void MarketFixingsMdiWindow::onRowDoubleClicked(const QModelIndex& index) {
    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* s = model_->getSeries(sourceIndex.row()))
        emit showMarketFixings(*s);
}

void MarketFixingsMdiWindow::viewFixings() {
    const auto selection = tableView_->selectionModel()->selectedRows();
    if (selection.isEmpty()) return;
    auto sourceIndex = proxyModel_->mapToSource(selection.first());
    if (auto* s = model_->getSeries(sourceIndex.row()))
        emit showMarketFixings(*s);
}

void MarketFixingsMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void MarketFixingsMdiWindow::onConnectionStateChanged() {
    if (clientManager_ && clientManager_->isConnected())
        model_->refresh();
}

}
