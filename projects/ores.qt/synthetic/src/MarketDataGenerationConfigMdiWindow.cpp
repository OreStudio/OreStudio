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
#include "ores.qt/MarketDataGenerationConfigMdiWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.synthetic.api/messaging/market_data_generation_config_protocol.hpp"
#include <QFutureWatcher>
#include <QHeaderView>
#include <QMessageBox>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

MarketDataGenerationConfigMdiWindow::MarketDataGenerationConfigMdiWindow(
    ClientManager* clientManager, const QString& username, QWidget* parent)
    : EntityListMdiWindow(parent)
    , clientManager_(clientManager)
    , username_(username)
    , toolbar_(nullptr)
    , tableView_(nullptr)
    , model_(nullptr)
    , proxyModel_(nullptr)
    , paginationWidget_(nullptr)
    , reloadAction_(nullptr)
    , addAction_(nullptr)
    , editAction_(nullptr)
    , deleteAction_(nullptr) {

    setupUi();
    setupConnections();
    reload();
}

void MarketDataGenerationConfigMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);

    setupToolbar();
    layout->addWidget(toolbar_);
    layout->addWidget(loadingBar());

    setupTable();
    layout->addWidget(tableView_);

    paginationWidget_ = new PaginationWidget(this);
    layout->addWidget(paginationWidget_);
}

void MarketDataGenerationConfigMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    connect(reloadAction_, &QAction::triggered, this, &EntityListMdiWindow::reload);

    initializeStaleIndicator(reloadAction_, IconUtils::iconPath(Icon::ArrowClockwise));

    toolbar_->addSeparator();

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor), tr("Add"));
    addAction_->setToolTip(tr("Add new market data generation config"));
    connect(addAction_, &QAction::triggered, this, &MarketDataGenerationConfigMdiWindow::addNew);

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Edit, IconUtils::DefaultIconColor), tr("Edit"));
    editAction_->setToolTip(tr("Edit selected market data generation config"));
    editAction_->setEnabled(false);
    connect(
        editAction_, &QAction::triggered, this, &MarketDataGenerationConfigMdiWindow::editSelected);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor), tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected market data generation config"));
    deleteAction_->setEnabled(false);
    connect(deleteAction_,
            &QAction::triggered,
            this,
            &MarketDataGenerationConfigMdiWindow::deleteSelected);
}

void MarketDataGenerationConfigMdiWindow::setupTable() {
    model_ = new ClientMarketDataGenerationConfigModel(clientManager_, this);
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
                            "MarketDataGenerationConfigListWindow",
                            {ClientMarketDataGenerationConfigModel::Description},
                            {900, 400},
                            1);
}

void MarketDataGenerationConfigMdiWindow::setupConnections() {
    connect(model_,
            &ClientMarketDataGenerationConfigModel::dataLoaded,
            this,
            &MarketDataGenerationConfigMdiWindow::onDataLoaded);
    connect(model_,
            &ClientMarketDataGenerationConfigModel::loadError,
            this,
            &MarketDataGenerationConfigMdiWindow::onLoadError);

    connect(tableView_->selectionModel(),
            &QItemSelectionModel::selectionChanged,
            this,
            &MarketDataGenerationConfigMdiWindow::onSelectionChanged);
    connect(tableView_,
            &QTableView::doubleClicked,
            this,
            &MarketDataGenerationConfigMdiWindow::onDoubleClicked);

    connect(
        paginationWidget_, &PaginationWidget::page_size_changed, this, [this](std::uint32_t size) {
            model_->set_page_size(size);
            model_->refresh();
        });

    connect(paginationWidget_, &PaginationWidget::load_all_requested, this, [this]() {
        const auto total = model_->total_available_count();
        if (total > 0 && total <= 1000) {
            model_->set_page_size(total);
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

void MarketDataGenerationConfigMdiWindow::doReload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading market data generation configs";
    clearStaleIndicator();
    emit statusChanged(tr("Loading market data generation configs..."));
    model_->refresh();
}

void MarketDataGenerationConfigMdiWindow::onDataLoaded() {
    const auto loaded = model_->rowCount();
    const auto total = model_->total_available_count();
    emit statusChanged(tr("Loaded %1 of %2 market data generation configs").arg(loaded).arg(total));

    paginationWidget_->update_state(loaded, total);
    paginationWidget_->set_load_all_enabled(loaded < static_cast<int>(total) && total > 0 &&
                                            total <= 1000);
}

void MarketDataGenerationConfigMdiWindow::onLoadError(const QString& error_message,
                                                      const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void MarketDataGenerationConfigMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void MarketDataGenerationConfigMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* config = model_->getConfig(sourceIndex.row())) {
        emit showConfigDetails(*config);
    }
}

void MarketDataGenerationConfigMdiWindow::updateActionStates() {
    const bool hasSelection = tableView_->selectionModel()->hasSelection();
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
}

void MarketDataGenerationConfigMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new market data generation config requested";
    emit addNewRequested();
}

void MarketDataGenerationConfigMdiWindow::editSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* config = model_->getConfig(sourceIndex.row())) {
        emit showConfigDetails(*config);
    }
}

void MarketDataGenerationConfigMdiWindow::deleteSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete market data generation config while disconnected.");
        return;
    }

    std::vector<std::string> ids;
    std::vector<std::string> names;
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* config = model_->getConfig(sourceIndex.row())) {
            ids.push_back(boost::uuids::to_string(config->id));
            names.push_back(config->name);
        }
    }

    if (ids.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid market data generation configs to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for " << ids.size()
                               << " market data generation configs";

    QString confirmMessage;
    if (ids.size() == 1) {
        confirmMessage =
            QString("Are you sure you want to delete market data generation config '%1'?")
                .arg(QString::fromStdString(names.front()));
    } else {
        confirmMessage =
            QString("Are you sure you want to delete %1 market data generation configs?")
                .arg(ids.size());
    }

    auto reply = MessageBoxHelper::question(this,
                                            "Delete Market Data Generation Config",
                                            confirmMessage,
                                            QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<MarketDataGenerationConfigMdiWindow> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
        std::vector<std::string> ids;
    };

    auto task = [self, ids]() -> DeleteResult {
        if (!self || !self->clientManager_)
            return {false, "Window closed", {}};

        BOOST_LOG_SEV(lg(), debug)
            << "Making delete request for " << ids.size() << " market data generation configs";

        synthetic::messaging::delete_market_data_generation_config_request request{.ids = ids};
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));
        if (!response_result) {
            return {false, "Failed to communicate with server", {}};
        }
        return {response_result->success, response_result->message, ids};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            for (const auto& id : result.ids) {
                BOOST_LOG_SEV(lg(), debug) << "Market Data Generation Config deleted: " << id;
                emit self->configDeleted(QString::fromStdString(id));
            }
            self->model_->refresh();
            QString msg = result.ids.size() == 1 ?
                              "Successfully deleted 1 market data generation config" :
                              QString("Successfully deleted %1 market data generation configs")
                                  .arg(result.ids.size());
            emit self->statusChanged(msg);
        } else {
            BOOST_LOG_SEV(lg(), error)
                << "Market Data Generation Config deletion failed: " << result.message;
            QString msg = QString::fromStdString(result.message);
            emit self->errorOccurred(msg);
            MessageBoxHelper::critical(self, "Delete Failed", msg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
