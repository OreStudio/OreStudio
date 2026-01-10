/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/ChangeReasonCategoryMdiWindow.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include "ores.qt/IconUtils.hpp"

namespace ores::qt {

using namespace ores::logging;

ChangeReasonCategoryMdiWindow::ChangeReasonCategoryMdiWindow(
    ClientManager* clientManager,
    const QString& username,
    QWidget* parent)
    : QWidget(parent),
      clientManager_(clientManager),
      username_(username),
      toolbar_(nullptr),
      tableView_(nullptr),
      model_(nullptr),
      proxyModel_(nullptr),
      reloadAction_(nullptr),
      viewDetailsAction_(nullptr),
      pulseTimer_(new QTimer(this)) {

    setupUi();
    setupConnections();

    // Initial load
    reload();
}

QSize ChangeReasonCategoryMdiWindow::sizeHint() const {
    return {800, 400};
}

void ChangeReasonCategoryMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    setupToolbar();
    layout->addWidget(toolbar_);

    setupTable();
    layout->addWidget(tableView_);
}

void ChangeReasonCategoryMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    const QColor iconColor(220, 220, 220);
    const QColor staleColor(255, 165, 0);  // Orange for stale

    normalReloadIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_clockwise_16_regular.svg", iconColor);
    staleReloadIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_clockwise_16_regular.svg", staleColor);

    reloadAction_ = toolbar_->addAction(normalReloadIcon_, tr("Reload"));
    reloadAction_->setToolTip(tr("Reload categories from server"));
    connect(reloadAction_, &QAction::triggered, this,
            &ChangeReasonCategoryMdiWindow::reload);

    toolbar_->addSeparator();

    viewDetailsAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_info_20_regular.svg", iconColor),
        tr("View Details"));
    viewDetailsAction_->setToolTip(tr("View category details"));
    viewDetailsAction_->setEnabled(false);
    connect(viewDetailsAction_, &QAction::triggered, this, [this]() {
        auto selected = tableView_->selectionModel()->selectedRows();
        if (selected.isEmpty())
            return;
        auto sourceIndex = proxyModel_->mapToSource(selected.first());
        if (auto* category = model_->getCategory(sourceIndex.row())) {
            emit showCategoryDetails(*category);
        }
    });

    // Pulse timer for stale indicator
    pulseTimer_->setInterval(pulse_interval_ms_);
    connect(pulseTimer_, &QTimer::timeout, this, [this]() {
        pulseState_ = !pulseState_;
        reloadAction_->setIcon(pulseState_ ? staleReloadIcon_ : normalReloadIcon_);
        pulseCount_++;
        if (pulseCount_ >= max_pulse_cycles_ * 2) {
            pulseTimer_->stop();
            reloadAction_->setIcon(staleReloadIcon_);
        }
    });
}

void ChangeReasonCategoryMdiWindow::setupTable() {
    model_ = new ClientChangeReasonCategoryModel(clientManager_, this);
    proxyModel_ = new QSortFilterProxyModel(this);
    proxyModel_->setSourceModel(model_);
    proxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    tableView_ = new QTableView(this);
    tableView_->setModel(proxyModel_);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    tableView_->setSortingEnabled(true);
    tableView_->setAlternatingRowColors(true);
    tableView_->horizontalHeader()->setStretchLastSection(true);
    tableView_->verticalHeader()->setVisible(false);

    // Set column widths
    tableView_->setColumnWidth(ClientChangeReasonCategoryModel::Code, 120);
    tableView_->setColumnWidth(ClientChangeReasonCategoryModel::Description, 250);
    tableView_->setColumnWidth(ClientChangeReasonCategoryModel::Version, 80);
    tableView_->setColumnWidth(ClientChangeReasonCategoryModel::RecordedBy, 120);
}

void ChangeReasonCategoryMdiWindow::setupConnections() {
    connect(model_, &ClientChangeReasonCategoryModel::dataLoaded,
            this, &ChangeReasonCategoryMdiWindow::onDataLoaded);
    connect(model_, &ClientChangeReasonCategoryModel::loadError,
            this, &ChangeReasonCategoryMdiWindow::onLoadError);

    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &ChangeReasonCategoryMdiWindow::onSelectionChanged);
    connect(tableView_, &QTableView::doubleClicked,
            this, &ChangeReasonCategoryMdiWindow::onDoubleClicked);
}

void ChangeReasonCategoryMdiWindow::reload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading categories";
    clearStaleIndicator();
    emit statusChanged(tr("Loading categories..."));
    model_->refresh();
}

void ChangeReasonCategoryMdiWindow::onDataLoaded() {
    emit statusChanged(tr("Loaded %1 categories").arg(model_->rowCount()));
}

void ChangeReasonCategoryMdiWindow::onLoadError(const QString& error_message) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
}

void ChangeReasonCategoryMdiWindow::onSelectionChanged() {
    bool hasSelection = tableView_->selectionModel()->hasSelection();
    viewDetailsAction_->setEnabled(hasSelection);
}

void ChangeReasonCategoryMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* category = model_->getCategory(sourceIndex.row())) {
        emit showCategoryDetails(*category);
    }
}

void ChangeReasonCategoryMdiWindow::markAsStale() {
    BOOST_LOG_SEV(lg(), debug) << "Marking as stale";
    reloadAction_->setToolTip(tr("Data changed on server - click to reload"));
    startPulseAnimation();
}

void ChangeReasonCategoryMdiWindow::clearStaleIndicator() {
    pulseTimer_->stop();
    pulseState_ = false;
    pulseCount_ = 0;
    reloadAction_->setIcon(normalReloadIcon_);
    reloadAction_->setToolTip(tr("Reload categories from server"));
}

void ChangeReasonCategoryMdiWindow::startPulseAnimation() {
    pulseCount_ = 0;
    pulseState_ = false;
    pulseTimer_->start();
}

}
