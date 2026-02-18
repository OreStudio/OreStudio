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
#include "ores.qt/MethodologyMdiWindow.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/EntityItemDelegate.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.dq/messaging/dataset_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

MethodologyMdiWindow::MethodologyMdiWindow(
    ClientManager* clientManager, const QString& username, QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      username_(username),
      model_(new ClientMethodologyModel(clientManager, this)),
      proxyModel_(new QSortFilterProxyModel(this)),
      tableView_(new QTableView(this)),
      toolbar_(nullptr) {

    proxyModel_->setSourceModel(model_);
    proxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    setupUi();
    setupToolbar();
    setupConnections();

    model_->refresh();
}

void MethodologyMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);

    tableView_->setModel(proxyModel_);
    tableView_->setSortingEnabled(true);
    using cs = column_style;
    tableView_->setItemDelegate(new EntityItemDelegate({
        cs::text_left,   // Name
        cs::text_left,   // Description
        cs::text_left,   // LogicReference
        cs::mono_center, // Version
        cs::text_left,   // ModifiedBy
        cs::mono_left    // RecordedAt
    }, tableView_));
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::ExtendedSelection);
    tableView_->setAlternatingRowColors(true);
    tableView_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    tableView_->verticalHeader()->setVisible(false);
    tableView_->sortByColumn(ClientMethodologyModel::Name, Qt::AscendingOrder);

    initializeTableSettings(tableView_, model_,
        "MethodologyMdiWindow",
        {}, {900, 400}, 1);

    layout->addWidget(tableView_);
}

void MethodologyMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor),
        tr("Add"));
    addAction_->setToolTip(tr("Add new methodology"));

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Edit, IconUtils::DefaultIconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected methodology"));
    editAction_->setEnabled(false);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected methodology(s)"));
    deleteAction_->setEnabled(false);

    toolbar_->addSeparator();

    historyAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor),
        tr("History"));
    historyAction_->setToolTip(tr("View version history"));
    historyAction_->setEnabled(false);

    toolbar_->addSeparator();

    refreshAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowSync, IconUtils::DefaultIconColor),
        tr("Refresh"));
    refreshAction_->setToolTip(tr("Refresh methodologies"));
    initializeStaleIndicator(refreshAction_, IconUtils::iconPath(Icon::ArrowSync));

    if (auto* layout = qobject_cast<QVBoxLayout*>(this->layout())) {
        layout->insertWidget(0, toolbar_);
    }
}

void MethodologyMdiWindow::setupConnections() {
    connect(model_, &ClientMethodologyModel::dataLoaded,
            this, &MethodologyMdiWindow::onDataLoaded);
    connect(model_, &ClientMethodologyModel::loadError,
            this, &MethodologyMdiWindow::onLoadError);
    connect(tableView_, &QTableView::doubleClicked,
            this, &MethodologyMdiWindow::onRowDoubleClicked);
    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &MethodologyMdiWindow::onSelectionChanged);

    connect(addAction_, &QAction::triggered, this, &MethodologyMdiWindow::onAddClicked);
    connect(editAction_, &QAction::triggered, this, &MethodologyMdiWindow::onEditClicked);
    connect(deleteAction_, &QAction::triggered, this, &MethodologyMdiWindow::onDeleteClicked);
    connect(refreshAction_, &QAction::triggered, this, &MethodologyMdiWindow::onRefreshClicked);
    connect(historyAction_, &QAction::triggered, this, &MethodologyMdiWindow::onHistoryClicked);
}

void MethodologyMdiWindow::onDataLoaded() {
    emit statusChanged(tr("Loaded %1 methodologies").arg(model_->rowCount()));
    updateActionStates();
}

void MethodologyMdiWindow::onLoadError(const QString& error_message,
                                        const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void MethodologyMdiWindow::onRowDoubleClicked(const QModelIndex& index) {
    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* methodology = model_->getMethodology(sourceIndex.row())) {
        emit showMethodologyDetails(*methodology);
    }
}

void MethodologyMdiWindow::onAddClicked() {
    emit addNewRequested();
}

void MethodologyMdiWindow::onEditClicked() {
    auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* methodology = model_->getMethodology(sourceIndex.row())) {
        emit showMethodologyDetails(*methodology);
    }
}

void MethodologyMdiWindow::onDeleteClicked() {
    auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    std::vector<boost::uuids::uuid> ids;
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* methodology = model_->getMethodology(sourceIndex.row())) {
            ids.push_back(methodology->id);
        }
    }

    QString message = ids.size() == 1
        ? tr("Delete selected methodology?")
        : tr("Delete %1 methodologies?").arg(ids.size());

    auto reply = MessageBoxHelper::question(this, tr("Confirm Delete"), message,
                                            QMessageBox::Yes | QMessageBox::No);
    if (reply != QMessageBox::Yes) return;

    QPointer<MethodologyMdiWindow> self = this;
    auto task = [self, ids = std::move(ids)]() -> bool {
        if (!self || !self->clientManager_) return false;

        dq::messaging::delete_methodology_request request;
        request.ids = ids;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_methodology_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return false;

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return false;

        auto response = dq::messaging::delete_methodology_response::deserialize(*payload_result);
        return response && !response->results.empty() && response->results[0].success;
    };

    auto* watcher = new QFutureWatcher<bool>(this);
    connect(watcher, &QFutureWatcher<bool>::finished, this, [self, watcher]() {
        bool success = watcher->result();
        watcher->deleteLater();

        if (self) {
            if (success) {
                emit self->statusChanged(tr("Methodology(s) deleted successfully"));
                self->reload();
            } else {
                emit self->errorOccurred(tr("Failed to delete methodology(s)"));
            }
        }
    });

    watcher->setFuture(QtConcurrent::run(task));
}

void MethodologyMdiWindow::onRefreshClicked() {
    emit statusChanged(tr("Refreshing..."));
    model_->refresh();
}

void MethodologyMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void MethodologyMdiWindow::onHistoryClicked() {
    auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* methodology = model_->getMethodology(sourceIndex.row())) {
        emit showMethodologyHistory(methodology->id);
    }
}

void MethodologyMdiWindow::updateActionStates() {
    auto selected = tableView_->selectionModel()->selectedRows();
    bool hasSelection = !selected.isEmpty();
    bool singleSelection = selected.size() == 1;

    editAction_->setEnabled(singleSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(singleSelection);
}

void MethodologyMdiWindow::reload() {
    clearStaleIndicator();
    model_->refresh();
}

}
