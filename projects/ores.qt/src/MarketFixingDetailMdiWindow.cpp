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
#include "ores.qt/MarketFixingDetailMdiWindow.hpp"

#include <QHeaderView>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/EntityItemDelegate.hpp"

namespace ores::qt {

using namespace ores::logging;

MarketFixingDetailMdiWindow::MarketFixingDetailMdiWindow(
    ClientManager* clientManager,
    const marketdata::domain::market_series& series,
    QWidget* parent)
    : EntityListMdiWindow(parent),
      series_(series),
      verticalLayout_(new QVBoxLayout(this)),
      seriesInfoLabel_(new QLabel(this)),
      tableView_(new QTableView(this)),
      toolBar_(new QToolBar(this)),
      reloadAction_(new QAction("Reload", this)),
      model_(std::make_unique<ClientMarketFixingModel>(
          clientManager, series.id)),
      proxyModel_(new QSortFilterProxyModel(this)),
      clientManager_(clientManager) {

    BOOST_LOG_SEV(lg(), debug) << "Creating market fixing detail MDI window";
    setupUi();
}

const boost::uuids::uuid& MarketFixingDetailMdiWindow::seriesId() const {
    return series_.id;
}

void MarketFixingDetailMdiWindow::setupUi() {
    verticalLayout_->setContentsMargins(4, 4, 4, 4);
    verticalLayout_->setSpacing(2);

    setupToolbar();
    verticalLayout_->addWidget(toolBar_);
    verticalLayout_->addWidget(loadingBar());

    // Series info banner
    const QString info = tr("%1 / %2 / %3")
        .arg(QString::fromStdString(series_.series_type))
        .arg(QString::fromStdString(series_.metric))
        .arg(QString::fromStdString(series_.qualifier));
    seriesInfoLabel_->setText(info);
    seriesInfoLabel_->setStyleSheet("padding: 4px; font-weight: bold;");
    verticalLayout_->addWidget(seriesInfoLabel_);

    // Table
    proxyModel_->setSourceModel(model_.get());
    tableView_->setModel(proxyModel_);
    tableView_->setAlternatingRowColors(true);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    tableView_->setWordWrap(false);
    tableView_->setSortingEnabled(true);
    tableView_->verticalHeader()->setVisible(false);
    tableView_->setItemDelegate(new EntityItemDelegate(tableView_));

    initializeTableSettings(tableView_, model_.get(),
        "MarketFixingDetailWindow", {}, {700, 500}, 1);

    verticalLayout_->addWidget(tableView_);

    connect(model_.get(), &ClientMarketFixingModel::dataLoaded,
            this, &MarketFixingDetailMdiWindow::onDataLoaded);
    connect(model_.get(), &ClientMarketFixingModel::loadError,
            this, &MarketFixingDetailMdiWindow::onLoadError);

    model_->refresh();
}

void MarketFixingDetailMdiWindow::setupToolbar() {
    toolBar_->setMovable(false);
    toolBar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);

    reloadAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowSync, color_constants::icon_color));
    reloadAction_->setToolTip(tr("Refresh fixings"));
    connect(reloadAction_, &QAction::triggered,
            this, &MarketFixingDetailMdiWindow::doReload);
    toolBar_->addAction(reloadAction_);
    initializeStaleIndicator(reloadAction_, Icon::ArrowSync);
}

void MarketFixingDetailMdiWindow::doReload() {
    clearStaleIndicator();
    model_->refresh();
}

void MarketFixingDetailMdiWindow::onDataLoaded() {
    const auto count = model_->rowCount();
    emit statusChanged(tr("Loaded %1 fixing(s)").arg(count));
}

void MarketFixingDetailMdiWindow::onLoadError(
    const QString& error_message, const QString& /*details*/) {
    emit errorOccurred(error_message);
}

}
