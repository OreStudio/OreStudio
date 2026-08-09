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
#include "ores.qt/YieldCurveProcessParameterDefinitionMdiWindow.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.synthetic.api/messaging/yield_curve_process_parameter_definition_protocol.hpp"
#include <QFutureWatcher>
#include <QHeaderView>
#include <QMessageBox>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

YieldCurveProcessParameterDefinitionMdiWindow::YieldCurveProcessParameterDefinitionMdiWindow(
    ClientManager* clientManager, const QString& username, QWidget* parent)
    : EntityListMdiWindow(parent)
    , clientManager_(clientManager)
    , username_(username)
    , toolbar_(nullptr)
    , tableView_(nullptr)
    , model_(nullptr)
    , proxyModel_(nullptr)
    , paginationWidget_(nullptr)
    , reloadAction_(nullptr) {

    setupUi();
    setupConnections();
    reload();
}

void YieldCurveProcessParameterDefinitionMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);

    setupToolbar();
    layout->addWidget(toolbar_);
    layout->addWidget(loadingBar());

    setupTable();
    layout->addWidget(tableView_);

    paginationWidget_ = new PaginationWidget(this);
    layout->addWidget(paginationWidget_);
}

void YieldCurveProcessParameterDefinitionMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    connect(reloadAction_, &QAction::triggered, this, &EntityListMdiWindow::reload);

    initializeStaleIndicator(reloadAction_, IconUtils::iconPath(Icon::ArrowClockwise));
}

void YieldCurveProcessParameterDefinitionMdiWindow::setupTable() {
    model_ = new ClientYieldCurveProcessParameterDefinitionModel(clientManager_, this);
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


    initializeTableSettings(tableView_,
                            model_,
                            "YieldCurveProcessParameterDefinitionListWindow",
                            {
                                ClientYieldCurveProcessParameterDefinitionModel::Description,
                            },
                            {900, 400},
                            1);
}

void YieldCurveProcessParameterDefinitionMdiWindow::setupConnections() {
    connect(model_,
            &ClientYieldCurveProcessParameterDefinitionModel::dataLoaded,
            this,
            &YieldCurveProcessParameterDefinitionMdiWindow::onDataLoaded);
    connect(model_,
            &ClientYieldCurveProcessParameterDefinitionModel::loadError,
            this,
            &YieldCurveProcessParameterDefinitionMdiWindow::onLoadError);

    connect(tableView_->selectionModel(),
            &QItemSelectionModel::selectionChanged,
            this,
            &YieldCurveProcessParameterDefinitionMdiWindow::onSelectionChanged);
    connect(tableView_,
            &QTableView::doubleClicked,
            this,
            &YieldCurveProcessParameterDefinitionMdiWindow::onDoubleClicked);

    connect(
        paginationWidget_, &PaginationWidget::page_size_changed, this, [this](std::uint32_t size) {
            model_->set_page_size(size);
            model_->refresh();
        });

    connect(paginationWidget_, &PaginationWidget::load_all_requested, this, [this]() {
        const auto total = model_->total_available_count();
        if (total > 0 && total <= 1000) {
            model_->set_page_size(total);
            paginationWidget_->reset_page();
            model_->refresh();
        }
    });

    connect(
        paginationWidget_,
        &PaginationWidget::page_requested,
        this,
        [this](std::uint32_t offset, std::uint32_t limit) { model_->load_page(offset, limit); });

    connectModel(model_);
}

void YieldCurveProcessParameterDefinitionMdiWindow::doReload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading yield curve process parameter definitions";
    clearStaleIndicator();
    emit statusChanged(tr("Loading yield curve process parameter definitions..."));
    model_->load_page(paginationWidget_->current_offset(), paginationWidget_->page_size());
}

void YieldCurveProcessParameterDefinitionMdiWindow::onDataLoaded() {
    const auto loaded = model_->rowCount();
    const auto total = model_->total_available_count();
    emit statusChanged(
        tr("Loaded %1 of %2 yield curve process parameter definitions").arg(loaded).arg(total));

    paginationWidget_->update_state(loaded, total);
    paginationWidget_->set_load_all_enabled(loaded < static_cast<int>(total) && total > 0 &&
                                            total <= 1000);
}

void YieldCurveProcessParameterDefinitionMdiWindow::onLoadError(const QString& error_message,
                                                                const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void YieldCurveProcessParameterDefinitionMdiWindow::onSelectionChanged() {}

void YieldCurveProcessParameterDefinitionMdiWindow::onDoubleClicked(const QModelIndex&) {}


}
