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
#include "ores.qt/CatalogMdiWindow.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/EntityItemDelegate.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.dq/messaging/data_organization_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

CatalogMdiWindow::CatalogMdiWindow(ClientManager* clientManager,
                                   const QString& username,
                                   QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      username_(username),
      tableView_(nullptr),
      model_(nullptr),
      toolbar_(nullptr) {

    setupUi();
    setupToolbar();
    setupConnections();
    model_->loadData();
}

void CatalogMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    tableView_ = new QTableView(this);
    tableView_->setAlternatingRowColors(true);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    tableView_->setSortingEnabled(true);
    tableView_->setItemDelegate(new EntityItemDelegate(
        ClientCatalogModel::columnStyles(), tableView_));
    tableView_->verticalHeader()->setVisible(false);

    model_ = new ClientCatalogModel(clientManager_, this);
    tableView_->setModel(model_);

    initializeTableSettings(tableView_, model_,
        ClientCatalogModel::kSettingsGroup,
        ClientCatalogModel::defaultHiddenColumns(), ClientCatalogModel::kDefaultWindowSize, 1);

    layout->addWidget(tableView_);
}

void CatalogMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Add, IconUtils::DefaultIconColor),
        tr("Add"));
    addAction_->setToolTip(tr("Add a new catalog"));

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Edit, IconUtils::DefaultIconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit the selected catalog"));
    editAction_->setEnabled(false);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Delete, IconUtils::DefaultIconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete the selected catalog"));
    deleteAction_->setEnabled(false);

    toolbar_->addSeparator();

    historyAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::History, IconUtils::DefaultIconColor),
        tr("History"));
    historyAction_->setToolTip(tr("View version history"));
    historyAction_->setEnabled(false);

    toolbar_->addSeparator();

    refreshAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowSync, IconUtils::DefaultIconColor),
        tr("Refresh"));

    initializeStaleIndicator(refreshAction_, IconUtils::iconPath(Icon::ArrowSync));

    if (auto* layout = qobject_cast<QVBoxLayout*>(this->layout())) {
        layout->insertWidget(0, toolbar_);
    }
}

void CatalogMdiWindow::setupConnections() {
    connect(model_, &ClientCatalogModel::loadStarted, this, [this]() {
        emit statusChanged(tr("Loading catalogs..."));
    });
    connect(model_, &ClientCatalogModel::loadFinished, this, [this]() {
        emit statusChanged(tr("Catalogs loaded"));
        updateActionStates();
    });
    connect(model_, &ClientCatalogModel::errorOccurred,
            this, &CatalogMdiWindow::errorOccurred);

    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, [this]() { updateActionStates(); });
    connect(tableView_, &QTableView::doubleClicked,
            this, &CatalogMdiWindow::onDoubleClicked);

    connect(addAction_, &QAction::triggered,
            this, &CatalogMdiWindow::onAddClicked);
    connect(editAction_, &QAction::triggered,
            this, &CatalogMdiWindow::onEditClicked);
    connect(deleteAction_, &QAction::triggered,
            this, &CatalogMdiWindow::onDeleteClicked);
    connect(historyAction_, &QAction::triggered,
            this, &CatalogMdiWindow::onHistoryClicked);
    connect(refreshAction_, &QAction::triggered,
            this, &CatalogMdiWindow::onRefreshClicked);
}

void CatalogMdiWindow::updateActionStates() {
    bool hasSelection = tableView_->selectionModel()->hasSelection();
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(hasSelection);
}

void CatalogMdiWindow::reload() {
    clearStaleIndicator();
    model_->loadData();
}

void CatalogMdiWindow::onAddClicked() {
    emit addNewRequested();
}

void CatalogMdiWindow::onEditClicked() {
    auto indexes = tableView_->selectionModel()->selectedRows();
    if (indexes.isEmpty())
        return;

    int row = indexes.first().row();
    const auto& catalog = model_->catalogAt(row);
    emit showCatalogDetails(catalog);
}

void CatalogMdiWindow::onDeleteClicked() {
    auto indexes = tableView_->selectionModel()->selectedRows();
    if (indexes.isEmpty())
        return;

    int row = indexes.first().row();
    const auto& catalog = model_->catalogAt(row);
    QString name = QString::fromStdString(catalog.name);

    auto result = QMessageBox::question(
        this, tr("Confirm Delete"),
        tr("Are you sure you want to delete catalog '%1'?").arg(name),
        QMessageBox::Yes | QMessageBox::No);

    if (result != QMessageBox::Yes)
        return;

    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorOccurred(tr("Not connected to server"));
        return;
    }

    QPointer<CatalogMdiWindow> self = this;
    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, name = catalog.name]() -> DeleteResult {
        if (!self || !self->clientManager_)
            return {false, "Window closed"};

        dq::messaging::delete_catalog_request request;
        request.names.push_back(name);
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_catalog_request,
            0, std::move(payload));

        auto response_result =
            self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result)
            return {false, "Failed to communicate with server"};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result)
            return {false, "Failed to decompress response"};

        auto response =
            dq::messaging::delete_catalog_response::deserialize(*payload_result);
        if (!response)
            return {false, "Invalid server response"};

        if (response->results.empty())
            return {false, "No result returned"};

        return {response->results[0].success, response->results[0].message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(this);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished, this,
            [self, watcher, name]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (result.success) {
            emit self->statusChanged(
                tr("Catalog '%1' deleted successfully").arg(name));
            self->reload();
        } else {
            emit self->errorOccurred(QString::fromStdString(result.message));
        }
    });

    watcher->setFuture(QtConcurrent::run(task));
}

void CatalogMdiWindow::onHistoryClicked() {
    auto indexes = tableView_->selectionModel()->selectedRows();
    if (indexes.isEmpty())
        return;

    int row = indexes.first().row();
    const auto& catalog = model_->catalogAt(row);
    emit showCatalogHistory(QString::fromStdString(catalog.name));
}

void CatalogMdiWindow::onRefreshClicked() {
    reload();
}

void CatalogMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    const auto& catalog = model_->catalogAt(index.row());
    emit showCatalogDetails(catalog);
}

}
