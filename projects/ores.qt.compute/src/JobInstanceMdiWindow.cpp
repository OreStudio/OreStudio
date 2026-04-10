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
#include "ores.qt/JobInstanceMdiWindow.hpp"

#include <QLabel>
#include <QVBoxLayout>
#include <QHeaderView>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

JobInstanceMdiWindow::JobInstanceMdiWindow(
    ClientManager* clientManager,
    QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      toolbar_(nullptr),
      tableView_(nullptr),
      model_(nullptr),
      proxyModel_(nullptr),
      reloadAction_(nullptr),
      autoRefreshAction_(nullptr),
      intervalSpin_(nullptr),
      autoRefreshTimer_(new QTimer(this)) {

    autoRefreshTimer_->setInterval(15000); // default 15 s
    connect(autoRefreshTimer_, &QTimer::timeout,
            this, &EntityListMdiWindow::reload);

    setupUi();
    setupConnections();
    reload();
}

void JobInstanceMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);
    setupToolbar();
    layout->addWidget(toolbar_);
    layout->addWidget(loadingBar());
    setupTable();
    layout->addWidget(tableView_);
}

void JobInstanceMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    connect(reloadAction_, &QAction::triggered,
            this, &EntityListMdiWindow::reload);

    initializeStaleIndicator(reloadAction_, IconUtils::iconPath(Icon::ArrowClockwise));

    toolbar_->addSeparator();

    autoRefreshAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Clock, IconUtils::DefaultIconColor),
        tr("Auto Refresh"));
    autoRefreshAction_->setCheckable(true);
    autoRefreshAction_->setChecked(false);
    connect(autoRefreshAction_, &QAction::toggled,
            this, &JobInstanceMdiWindow::onAutoRefreshToggled);

    toolbar_->addWidget(new QLabel(tr(" Every "), this));

    intervalSpin_ = new QSpinBox(this);
    intervalSpin_->setRange(5, 3600);
    intervalSpin_->setValue(15);
    intervalSpin_->setSuffix(tr(" s"));
    intervalSpin_->setToolTip(tr("Auto-refresh interval in seconds"));
    connect(intervalSpin_, &QSpinBox::valueChanged,
            this, &JobInstanceMdiWindow::onAutoRefreshIntervalChanged);
    toolbar_->addWidget(intervalSpin_);
}

void JobInstanceMdiWindow::setupTable() {
    model_ = new ClientJobInstanceModel(clientManager_, this);
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

    initializeTableSettings(tableView_, model_,
        "JobInstanceListWindow",
        {ClientJobInstanceModel::ErrorMessage},
        {960, 420}, 1);
}

void JobInstanceMdiWindow::setupConnections() {
    connect(model_, &ClientJobInstanceModel::dataLoaded,
            this, &JobInstanceMdiWindow::onDataLoaded);
    connect(model_, &ClientJobInstanceModel::loadError,
            this, &JobInstanceMdiWindow::onLoadError);
    connectModel(model_);

    connect(tableView_, &QTableView::doubleClicked,
            this, &JobInstanceMdiWindow::onDoubleClicked);
}

void JobInstanceMdiWindow::doReload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading job instances";
    emit statusChanged(tr("Loading job instances..."));
    model_->refresh();
}

void JobInstanceMdiWindow::onDataLoaded() {
    const auto loaded = model_->rowCount();
    emit statusChanged(tr("Loaded %1 job instances").arg(loaded));
}

void JobInstanceMdiWindow::onLoadError(const QString& error_message,
                                        const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void JobInstanceMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid()) return;

    const auto sourceIndex = proxyModel_->mapToSource(index);
    if (const auto* inst = model_->getInstance(sourceIndex.row()))
        emit showInstanceDetails(*inst);
}

void JobInstanceMdiWindow::onAutoRefreshToggled(bool enabled) {
    if (enabled)
        autoRefreshTimer_->start();
    else
        autoRefreshTimer_->stop();
}

void JobInstanceMdiWindow::onAutoRefreshIntervalChanged(int seconds) {
    autoRefreshTimer_->setInterval(seconds * 1000);
}

}
