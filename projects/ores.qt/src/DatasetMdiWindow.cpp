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
#include "ores.qt/DatasetMdiWindow.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include <QSettings>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.dq/messaging/dataset_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

DatasetMdiWindow::DatasetMdiWindow(
    ClientManager* clientManager, const QString& username, QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      username_(username),
      model_(new ClientDatasetModel(clientManager, this)),
      proxyModel_(new QSortFilterProxyModel(this)),
      tableView_(new QTableView(this)),
      toolbar_(nullptr) {

    proxyModel_->setSourceModel(model_);
    proxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    setupUi();
    setupToolbar();
    setupConnections();
    loadColumnVisibility();

    model_->refresh();
}

void DatasetMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);

    tableView_->setModel(proxyModel_);
    tableView_->setSortingEnabled(true);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::ExtendedSelection);
    tableView_->setAlternatingRowColors(true);
    tableView_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    tableView_->horizontalHeader()->setStretchLastSection(true);
    tableView_->verticalHeader()->setVisible(false);
    tableView_->sortByColumn(ClientDatasetModel::Name, Qt::AscendingOrder);

    layout->addWidget(tableView_);
}

void DatasetMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    const auto& iconColor = color_constants::icon_color;

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_add_20_regular.svg", iconColor),
        tr("Add"));
    addAction_->setToolTip(tr("Add new dataset"));

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_edit_20_regular.svg", iconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected dataset"));
    editAction_->setEnabled(false);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_delete_20_regular.svg", iconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected dataset(s)"));
    deleteAction_->setEnabled(false);

    toolbar_->addSeparator();

    historyAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_history_20_regular.svg", iconColor),
        tr("History"));
    historyAction_->setToolTip(tr("View version history"));
    historyAction_->setEnabled(false);

    toolbar_->addSeparator();

    refreshAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_arrow_sync_20_regular.svg", iconColor),
        tr("Refresh"));

    initializeStaleIndicator(refreshAction_, ":/icons/ic_fluent_arrow_sync_20_regular.svg");

    if (auto* layout = qobject_cast<QVBoxLayout*>(this->layout())) {
        layout->insertWidget(0, toolbar_);
    }
}

void DatasetMdiWindow::setupConnections() {
    connect(model_, &ClientDatasetModel::dataLoaded,
            this, &DatasetMdiWindow::onDataLoaded);
    connect(model_, &ClientDatasetModel::loadError,
            this, &DatasetMdiWindow::onLoadError);
    connect(tableView_, &QTableView::doubleClicked,
            this, &DatasetMdiWindow::onRowDoubleClicked);
    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &DatasetMdiWindow::onSelectionChanged);

    connect(addAction_, &QAction::triggered, this, &DatasetMdiWindow::onAddClicked);
    connect(editAction_, &QAction::triggered, this, &DatasetMdiWindow::onEditClicked);
    connect(deleteAction_, &QAction::triggered, this, &DatasetMdiWindow::onDeleteClicked);
    connect(refreshAction_, &QAction::triggered, this, &DatasetMdiWindow::onRefreshClicked);
    connect(historyAction_, &QAction::triggered, this, &DatasetMdiWindow::onHistoryClicked);
}

void DatasetMdiWindow::onDataLoaded() {
    emit statusChanged(tr("Loaded %1 datasets").arg(model_->rowCount()));
    updateActionStates();
}

void DatasetMdiWindow::onLoadError(const QString& error_message,
                                    const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void DatasetMdiWindow::onRowDoubleClicked(const QModelIndex& index) {
    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* dataset = model_->getDataset(sourceIndex.row())) {
        emit showDatasetDetails(*dataset);
    }
}

void DatasetMdiWindow::onAddClicked() {
    emit addNewRequested();
}

void DatasetMdiWindow::onEditClicked() {
    auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* dataset = model_->getDataset(sourceIndex.row())) {
        emit showDatasetDetails(*dataset);
    }
}

void DatasetMdiWindow::onDeleteClicked() {
    auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    std::vector<boost::uuids::uuid> ids;
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* dataset = model_->getDataset(sourceIndex.row())) {
            ids.push_back(dataset->id);
        }
    }

    QString message = ids.size() == 1
        ? tr("Delete selected dataset?")
        : tr("Delete %1 datasets?").arg(ids.size());

    auto reply = MessageBoxHelper::question(this, tr("Confirm Delete"), message,
                                            QMessageBox::Yes | QMessageBox::No);
    if (reply != QMessageBox::Yes) return;

    QPointer<DatasetMdiWindow> self = this;
    auto task = [self, ids = std::move(ids)]() -> bool {
        if (!self || !self->clientManager_) return false;

        dq::messaging::delete_dataset_request request;
        request.ids = ids;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_dataset_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return false;

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return false;

        auto response = dq::messaging::delete_dataset_response::deserialize(*payload_result);
        return response && !response->results.empty() && response->results[0].success;
    };

    auto* watcher = new QFutureWatcher<bool>(this);
    connect(watcher, &QFutureWatcher<bool>::finished, this, [self, watcher]() {
        bool success = watcher->result();
        watcher->deleteLater();

        if (self) {
            if (success) {
                emit self->statusChanged(tr("Dataset(s) deleted successfully"));
                self->reload();
            } else {
                emit self->errorOccurred(tr("Failed to delete dataset(s)"));
            }
        }
    });

    watcher->setFuture(QtConcurrent::run(task));
}

void DatasetMdiWindow::onRefreshClicked() {
    emit statusChanged(tr("Refreshing..."));
    model_->refresh();
}

void DatasetMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void DatasetMdiWindow::onHistoryClicked() {
    auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* dataset = model_->getDataset(sourceIndex.row())) {
        emit showDatasetHistory(dataset->id);
    }
}

void DatasetMdiWindow::updateActionStates() {
    auto selected = tableView_->selectionModel()->selectedRows();
    bool hasSelection = !selected.isEmpty();
    bool singleSelection = selected.size() == 1;

    editAction_->setEnabled(singleSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(singleSelection);
}

void DatasetMdiWindow::reload() {
    clearStaleIndicator();
    model_->refresh();
}

void DatasetMdiWindow::saveColumnVisibility() {
    QSettings settings;
    settings.beginGroup("DatasetMdiWindow");
    for (int i = 0; i < model_->columnCount(); ++i) {
        settings.setValue(QString("column_%1_visible").arg(i),
                          !tableView_->isColumnHidden(i));
    }
    settings.endGroup();
}

void DatasetMdiWindow::loadColumnVisibility() {
    QSettings settings;
    settings.beginGroup("DatasetMdiWindow");
    for (int i = 0; i < model_->columnCount(); ++i) {
        bool visible = settings.value(QString("column_%1_visible").arg(i), true).toBool();
        tableView_->setColumnHidden(i, !visible);
    }
    settings.endGroup();
}

}
