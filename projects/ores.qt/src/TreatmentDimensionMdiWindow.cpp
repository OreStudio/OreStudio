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
#include "ores.qt/TreatmentDimensionMdiWindow.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include <QSettings>
#include <QMessageBox>
#include <QtConcurrent>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.dq/messaging/dimension_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

TreatmentDimensionMdiWindow::TreatmentDimensionMdiWindow(
    ClientManager* clientManager, const QString& username, QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      username_(username),
      model_(new ClientTreatmentDimensionModel(clientManager, this)),
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

void TreatmentDimensionMdiWindow::setupUi() {
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
    tableView_->sortByColumn(ClientTreatmentDimensionModel::Code, Qt::AscendingOrder);

    layout->addWidget(tableView_);
}

void TreatmentDimensionMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    const auto& iconColor = color_constants::icon_color;

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_add_20_regular.svg", iconColor),
        tr("Add"));
    addAction_->setToolTip(tr("Add new treatment dimension"));

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_edit_20_regular.svg", iconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected treatment dimension"));
    editAction_->setEnabled(false);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_delete_20_regular.svg", iconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected treatment dimension(s)"));
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

void TreatmentDimensionMdiWindow::setupConnections() {
    connect(model_, &ClientTreatmentDimensionModel::dataLoaded,
            this, &TreatmentDimensionMdiWindow::onDataLoaded);
    connect(model_, &ClientTreatmentDimensionModel::loadError,
            this, &TreatmentDimensionMdiWindow::onLoadError);
    connect(tableView_, &QTableView::doubleClicked,
            this, &TreatmentDimensionMdiWindow::onRowDoubleClicked);
    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &TreatmentDimensionMdiWindow::onSelectionChanged);

    connect(addAction_, &QAction::triggered, this, &TreatmentDimensionMdiWindow::onAddClicked);
    connect(editAction_, &QAction::triggered, this, &TreatmentDimensionMdiWindow::onEditClicked);
    connect(deleteAction_, &QAction::triggered, this, &TreatmentDimensionMdiWindow::onDeleteClicked);
    connect(refreshAction_, &QAction::triggered, this, &TreatmentDimensionMdiWindow::onRefreshClicked);
    connect(historyAction_, &QAction::triggered, this, &TreatmentDimensionMdiWindow::onHistoryClicked);
}

void TreatmentDimensionMdiWindow::onDataLoaded() {
    emit statusChanged(tr("Loaded %1 treatment dimensions").arg(model_->rowCount()));
    updateActionStates();
}

void TreatmentDimensionMdiWindow::onLoadError(const QString& error_message,
                                               const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void TreatmentDimensionMdiWindow::onRowDoubleClicked(const QModelIndex& index) {
    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* dim = model_->getDimension(sourceIndex.row())) {
        emit showDimensionDetails(*dim);
    }
}

void TreatmentDimensionMdiWindow::onAddClicked() {
    emit addNewRequested();
}

void TreatmentDimensionMdiWindow::onEditClicked() {
    auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* dim = model_->getDimension(sourceIndex.row())) {
        emit showDimensionDetails(*dim);
    }
}

void TreatmentDimensionMdiWindow::onDeleteClicked() {
    auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    std::vector<std::string> codes;
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* dim = model_->getDimension(sourceIndex.row())) {
            codes.push_back(dim->code);
        }
    }

    QString message = codes.size() == 1
        ? tr("Delete treatment dimension '%1'?").arg(QString::fromStdString(codes[0]))
        : tr("Delete %1 treatment dimensions?").arg(codes.size());

    auto reply = MessageBoxHelper::question(this, tr("Confirm Delete"), message,
                                            QMessageBox::Yes | QMessageBox::No);
    if (reply != QMessageBox::Yes) return;

    QPointer<TreatmentDimensionMdiWindow> self = this;
    auto task = [self, codes = std::move(codes)]() -> bool {
        if (!self || !self->clientManager_) return false;

        dq::messaging::delete_treatment_dimension_request request;
        request.codes = codes;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_treatment_dimension_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return false;

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return false;

        auto response = dq::messaging::delete_treatment_dimension_response::deserialize(*payload_result);
        return response && !response->results.empty() && response->results[0].success;
    };

    auto* watcher = new QFutureWatcher<bool>(this);
    connect(watcher, &QFutureWatcher<bool>::finished, this, [self, watcher]() {
        bool success = watcher->result();
        watcher->deleteLater();

        if (self) {
            if (success) {
                emit self->statusChanged(tr("Treatment dimension(s) deleted successfully"));
                self->reload();
            } else {
                emit self->errorOccurred(tr("Failed to delete treatment dimension(s)"));
            }
        }
    });

    watcher->setFuture(QtConcurrent::run(task));
}

void TreatmentDimensionMdiWindow::onRefreshClicked() {
    emit statusChanged(tr("Refreshing..."));
    model_->refresh();
}

void TreatmentDimensionMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void TreatmentDimensionMdiWindow::onHistoryClicked() {
    auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* dim = model_->getDimension(sourceIndex.row())) {
        emit showDimensionHistory(QString::fromStdString(dim->code));
    }
}

void TreatmentDimensionMdiWindow::updateActionStates() {
    auto selected = tableView_->selectionModel()->selectedRows();
    bool hasSelection = !selected.isEmpty();
    bool singleSelection = selected.size() == 1;

    editAction_->setEnabled(singleSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(singleSelection);
}

void TreatmentDimensionMdiWindow::reload() {
    clearStaleIndicator();
    model_->refresh();
}

void TreatmentDimensionMdiWindow::saveColumnVisibility() {
    QSettings settings;
    settings.beginGroup("TreatmentDimensionMdiWindow");
    for (int i = 0; i < model_->columnCount(); ++i) {
        settings.setValue(QString("column_%1_visible").arg(i),
                          !tableView_->isColumnHidden(i));
    }
    settings.endGroup();
}

void TreatmentDimensionMdiWindow::loadColumnVisibility() {
    QSettings settings;
    settings.beginGroup("TreatmentDimensionMdiWindow");
    for (int i = 0; i < model_->columnCount(); ++i) {
        bool visible = settings.value(QString("column_%1_visible").arg(i), true).toBool();
        tableView_->setColumnHidden(i, !visible);
    }
    settings.endGroup();
}

}
