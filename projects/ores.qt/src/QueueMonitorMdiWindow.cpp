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
#include "ores.qt/QueueMonitorMdiWindow.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/EntityItemDelegate.hpp"
#include "ores.qt/MessageBoxHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

QueueMonitorMdiWindow::QueueMonitorMdiWindow(
    ClientManager* clientManager, QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      toolbar_(nullptr),
      tableView_(nullptr),
      model_(nullptr),
      proxyModel_(nullptr),
      reloadAction_(nullptr) {

    setupUi();
    setupConnections();
    reload();
}

void QueueMonitorMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    setupToolbar();
    layout->addWidget(toolbar_);

    setupTable();
    layout->addWidget(tableView_);
}

void QueueMonitorMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    connect(reloadAction_, &QAction::triggered,
            this, &QueueMonitorMdiWindow::reload);

    initializeStaleIndicator(reloadAction_,
                             IconUtils::iconPath(Icon::ArrowClockwise));

    toolbar_->addSeparator();

    chartAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor),
        tr("View Chart"));
    chartAction_->setToolTip(tr("Show time-series chart for selected queue"));
    chartAction_->setEnabled(false);
    connect(chartAction_, &QAction::triggered,
            this, &QueueMonitorMdiWindow::onViewChart);
}

void QueueMonitorMdiWindow::setupTable() {
    model_ = new ClientQueueModel(clientManager_, this);
    proxyModel_ = new QSortFilterProxyModel(this);
    proxyModel_->setSourceModel(model_);
    proxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    tableView_ = new QTableView(this);
    tableView_->setModel(proxyModel_);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    tableView_->setSortingEnabled(true);
    tableView_->setAlternatingRowColors(true);
    tableView_->verticalHeader()->setVisible(false);
    tableView_->setItemDelegate(
        new EntityItemDelegate(ClientQueueModel::columnStyles(), tableView_));

    initializeTableSettings(tableView_, model_,
        ClientQueueModel::kSettingsGroup,
        ClientQueueModel::defaultHiddenColumns(),
        ClientQueueModel::kDefaultWindowSize, 1);
}

void QueueMonitorMdiWindow::updateActionStates() {
    const bool has_selection =
        tableView_->selectionModel() &&
        tableView_->selectionModel()->hasSelection();
    chartAction_->setEnabled(has_selection);
}

void QueueMonitorMdiWindow::setupConnections() {
    connect(model_, &ClientQueueModel::dataLoaded,
            this, &QueueMonitorMdiWindow::onDataLoaded);
    connect(model_, &ClientQueueModel::loadError,
            this, &QueueMonitorMdiWindow::onLoadError);
    connect(tableView_, &QTableView::doubleClicked,
            this, &QueueMonitorMdiWindow::onRowDoubleClicked);
}

void QueueMonitorMdiWindow::reload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading queue metrics";
    clearStaleIndicator();
    emit statusChanged(tr("Loading queues..."));
    model_->refresh();
}

void QueueMonitorMdiWindow::onDataLoaded() {
    emit statusChanged(tr("Loaded %1 queues").arg(model_->rowCount()));

    // Wire selection model after model data is loaded (proxy model may
    // not have a valid selection model before first data arrives).
    disconnect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
               this, &QueueMonitorMdiWindow::onSelectionChanged);
    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &QueueMonitorMdiWindow::onSelectionChanged);
    updateActionStates();
}

void QueueMonitorMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void QueueMonitorMdiWindow::onRowDoubleClicked(const QModelIndex& index) {
    const auto sourceIndex = proxyModel_->mapToSource(index);
    if (const auto* row = model_->getRow(sourceIndex.row())) {
        emit viewChartRequested(QString::fromStdString(row->queue_name));
    }
}

void QueueMonitorMdiWindow::onViewChart() {
    const auto selection = tableView_->selectionModel()->selectedRows();
    if (selection.isEmpty()) return;

    const auto sourceIndex = proxyModel_->mapToSource(selection.first());
    if (const auto* row = model_->getRow(sourceIndex.row())) {
        emit viewChartRequested(QString::fromStdString(row->queue_name));
    }
}

void QueueMonitorMdiWindow::onLoadError(const QString& error_message,
                                        const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

}
