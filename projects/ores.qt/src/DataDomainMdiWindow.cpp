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
#include "ores.qt/DataDomainMdiWindow.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include <QSettings>
#include <QMessageBox>
#include <QtConcurrent>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.dq/messaging/data_organization_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

DataDomainMdiWindow::DataDomainMdiWindow(
    ClientManager* clientManager, const QString& username, QWidget* parent)
    : QWidget(parent),
      clientManager_(clientManager),
      username_(username),
      model_(new ClientDataDomainModel(clientManager, this)),
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

void DataDomainMdiWindow::setupUi() {
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
    tableView_->sortByColumn(ClientDataDomainModel::Name, Qt::AscendingOrder);

    layout->addWidget(tableView_);
}

void DataDomainMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    const auto& iconColor = color_constants::icon_color;

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_add_20_regular.svg", iconColor),
        tr("Add"));
    addAction_->setToolTip(tr("Add new data domain"));

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_edit_20_regular.svg", iconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected data domain"));
    editAction_->setEnabled(false);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_delete_20_regular.svg", iconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected data domain(s)"));
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
    refreshAction_->setToolTip(tr("Refresh data domains"));

    if (auto* layout = qobject_cast<QVBoxLayout*>(this->layout())) {
        layout->insertWidget(0, toolbar_);
    }
}

void DataDomainMdiWindow::setupConnections() {
    connect(model_, &ClientDataDomainModel::dataLoaded,
            this, &DataDomainMdiWindow::onDataLoaded);
    connect(model_, &ClientDataDomainModel::loadError,
            this, &DataDomainMdiWindow::onLoadError);
    connect(tableView_, &QTableView::doubleClicked,
            this, &DataDomainMdiWindow::onRowDoubleClicked);
    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &DataDomainMdiWindow::onSelectionChanged);

    connect(addAction_, &QAction::triggered, this, &DataDomainMdiWindow::onAddClicked);
    connect(editAction_, &QAction::triggered, this, &DataDomainMdiWindow::onEditClicked);
    connect(deleteAction_, &QAction::triggered, this, &DataDomainMdiWindow::onDeleteClicked);
    connect(refreshAction_, &QAction::triggered, this, &DataDomainMdiWindow::onRefreshClicked);
    connect(historyAction_, &QAction::triggered, this, &DataDomainMdiWindow::onHistoryClicked);
}

void DataDomainMdiWindow::onDataLoaded() {
    emit statusChanged(tr("Loaded %1 data domains").arg(model_->rowCount()));
    updateActionStates();
}

void DataDomainMdiWindow::onLoadError(const QString& error_message) {
    emit errorOccurred(error_message);
}

void DataDomainMdiWindow::onRowDoubleClicked(const QModelIndex& index) {
    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* domain = model_->getDomain(sourceIndex.row())) {
        emit showDomainDetails(*domain);
    }
}

void DataDomainMdiWindow::onAddClicked() {
    emit addNewRequested();
}

void DataDomainMdiWindow::onEditClicked() {
    auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* domain = model_->getDomain(sourceIndex.row())) {
        emit showDomainDetails(*domain);
    }
}

void DataDomainMdiWindow::onDeleteClicked() {
    auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    std::vector<std::string> names;
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* domain = model_->getDomain(sourceIndex.row())) {
            names.push_back(domain->name);
        }
    }

    QString message = names.size() == 1
        ? tr("Delete data domain '%1'?").arg(QString::fromStdString(names[0]))
        : tr("Delete %1 data domains?").arg(names.size());

    auto reply = MessageBoxHelper::question(this, tr("Confirm Delete"), message,
                                            QMessageBox::Yes | QMessageBox::No);
    if (reply != QMessageBox::Yes) return;

    QPointer<DataDomainMdiWindow> self = this;
    auto task = [self, names = std::move(names)]() -> bool {
        if (!self || !self->clientManager_) return false;

        dq::messaging::delete_data_domain_request request;
        request.names = names;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_data_domain_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return false;

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return false;

        auto response = dq::messaging::delete_data_domain_response::deserialize(*payload_result);
        return response && !response->results.empty() && response->results[0].success;
    };

    auto* watcher = new QFutureWatcher<bool>(this);
    connect(watcher, &QFutureWatcher<bool>::finished, this, [self, watcher]() {
        bool success = watcher->result();
        watcher->deleteLater();

        if (self) {
            if (success) {
                emit self->statusChanged(tr("Data domain(s) deleted successfully"));
                self->reload();
            } else {
                emit self->errorOccurred(tr("Failed to delete data domain(s)"));
            }
        }
    });

    watcher->setFuture(QtConcurrent::run(task));
}

void DataDomainMdiWindow::onRefreshClicked() {
    emit statusChanged(tr("Refreshing..."));
    model_->refresh();
}

void DataDomainMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void DataDomainMdiWindow::onHistoryClicked() {
    auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* domain = model_->getDomain(sourceIndex.row())) {
        emit showDomainHistory(QString::fromStdString(domain->name));
    }
}

void DataDomainMdiWindow::updateActionStates() {
    auto selected = tableView_->selectionModel()->selectedRows();
    bool hasSelection = !selected.isEmpty();
    bool singleSelection = selected.size() == 1;

    editAction_->setEnabled(singleSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(singleSelection);
}

void DataDomainMdiWindow::reload() {
    model_->refresh();
}

void DataDomainMdiWindow::saveColumnVisibility() {
    QSettings settings;
    settings.beginGroup("DataDomainMdiWindow");
    for (int i = 0; i < model_->columnCount(); ++i) {
        settings.setValue(QString("column_%1_visible").arg(i),
                          !tableView_->isColumnHidden(i));
    }
    settings.endGroup();
}

void DataDomainMdiWindow::loadColumnVisibility() {
    QSettings settings;
    settings.beginGroup("DataDomainMdiWindow");
    for (int i = 0; i < model_->columnCount(); ++i) {
        bool visible = settings.value(QString("column_%1_visible").arg(i), true).toBool();
        tableView_->setColumnHidden(i, !visible);
    }
    settings.endGroup();
}

}
