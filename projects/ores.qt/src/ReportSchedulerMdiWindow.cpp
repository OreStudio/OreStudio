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
#include "ores.qt/ReportSchedulerMdiWindow.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

ReportSchedulerMdiWindow::ReportSchedulerMdiWindow(
    ClientManager* clientManager,
    const QString& username,
    QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      username_(username),
      toolbar_(nullptr),
      tableView_(nullptr),
      model_(nullptr),
      proxyModel_(nullptr),
      paginationWidget_(nullptr),
      reloadAction_(nullptr),
      playAction_(nullptr),
      stopAction_(nullptr) {

    setupUi();
    setupConnections();

    // Initial load
    reload();
}

void ReportSchedulerMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);

    setupToolbar();
    layout->addWidget(toolbar_);

    setupTable();
    layout->addWidget(tableView_);

    paginationWidget_ = new PaginationWidget(this);
    layout->addWidget(paginationWidget_);
}

void ReportSchedulerMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    connect(reloadAction_, &QAction::triggered, this,
            &ReportSchedulerMdiWindow::reload);

    initializeStaleIndicator(reloadAction_, IconUtils::iconPath(Icon::ArrowClockwise));

    toolbar_->addSeparator();

    playAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::CalendarAdd, IconUtils::DefaultIconColor),
        tr("Schedule"));
    playAction_->setToolTip(tr("Schedule selected report definitions"));
    playAction_->setEnabled(false);
    connect(playAction_, &QAction::triggered, this,
            &ReportSchedulerMdiWindow::onPlayClicked);

    stopAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::CalendarCancel, IconUtils::DefaultIconColor),
        tr("Unschedule"));
    stopAction_->setToolTip(tr("Unschedule selected report definitions"));
    stopAction_->setEnabled(false);
    connect(stopAction_, &QAction::triggered, this,
            &ReportSchedulerMdiWindow::onStopClicked);
}

void ReportSchedulerMdiWindow::setupTable() {
    model_ = new ClientReportDefinitionModel(clientManager_, this);
    proxyModel_ = new QSortFilterProxyModel(this);
    proxyModel_->setSourceModel(model_);
    proxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    tableView_ = new QTableView(this);
    tableView_->setModel(proxyModel_);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::ExtendedSelection);
    tableView_->setSortingEnabled(true);
    tableView_->setAlternatingRowColors(true);
    tableView_->verticalHeader()->setVisible(false);

    initializeTableSettings(tableView_, model_,
        "ReportSchedulerListWindow",
        {},
        {900, 400}, 1);
}

void ReportSchedulerMdiWindow::setupConnections() {
    connect(model_, &ClientReportDefinitionModel::dataLoaded,
            this, &ReportSchedulerMdiWindow::onDataLoaded);
    connect(model_, &ClientReportDefinitionModel::loadError,
            this, &ReportSchedulerMdiWindow::onLoadError);

    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &ReportSchedulerMdiWindow::onSelectionChanged);

    connect(paginationWidget_, &PaginationWidget::page_size_changed,
            this, [this](std::uint32_t size) {
        model_->set_page_size(size);
        model_->refresh();
    });

    connect(paginationWidget_, &PaginationWidget::load_all_requested,
            this, [this]() {
        const auto total = model_->total_available_count();
        if (total > 0 && total <= 1000) {
            model_->set_page_size(total);
            model_->refresh();
        }
    });

    connect(paginationWidget_, &PaginationWidget::page_requested,
            this, [this](std::uint32_t offset, std::uint32_t limit) {
        model_->load_page(offset, limit);
    });
}

void ReportSchedulerMdiWindow::reload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading report definitions for scheduler";
    clearStaleIndicator();
    emit statusChanged(tr("Loading report definitions..."));
    model_->refresh();
}

void ReportSchedulerMdiWindow::onDataLoaded() {
    const auto loaded = model_->rowCount();
    const auto total = model_->total_available_count();
    emit statusChanged(tr("Loaded %1 of %2 report definitions").arg(loaded).arg(total));

    paginationWidget_->update_state(loaded, total);
    paginationWidget_->set_load_all_enabled(
        loaded < static_cast<int>(total) && total > 0 && total <= 1000);
}

void ReportSchedulerMdiWindow::onLoadError(const QString& error_message,
                                           const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void ReportSchedulerMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void ReportSchedulerMdiWindow::updateActionStates() {
    const auto selected = tableView_->selectionModel()->selectedRows();

    bool hasInactive = false;
    bool hasActive = false;

    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (const auto* def = model_->getDefinition(sourceIndex.row())) {
            if (def->scheduler_job_id.has_value())
                hasActive = true;
            else
                hasInactive = true;
        }
    }

    playAction_->setEnabled(hasInactive);
    stopAction_->setEnabled(hasActive);
}

void ReportSchedulerMdiWindow::onPlayClicked() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    std::vector<boost::uuids::uuid> ids;

    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (const auto* def = model_->getDefinition(sourceIndex.row())) {
            if (!def->scheduler_job_id.has_value()) {
                ids.push_back(def->id);
            }
        }
    }

    if (!ids.empty()) {
        BOOST_LOG_SEV(lg(), debug) << "Scheduling " << ids.size() << " report definitions";
        emit scheduleRequested(ids);
    }
}

void ReportSchedulerMdiWindow::onStopClicked() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    std::vector<boost::uuids::uuid> ids;

    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (const auto* def = model_->getDefinition(sourceIndex.row())) {
            if (def->scheduler_job_id.has_value()) {
                ids.push_back(def->id);
            }
        }
    }

    if (!ids.empty()) {
        BOOST_LOG_SEV(lg(), debug) << "Unscheduling " << ids.size() << " report definitions";
        emit unscheduleRequested(ids);
    }
}

}
