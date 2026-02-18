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
#include "ores.qt/CodingSchemeMdiWindow.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include <QMessageBox>
#include <QtConcurrent>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/EntityItemDelegate.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.dq/messaging/coding_scheme_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

CodingSchemeMdiWindow::CodingSchemeMdiWindow(
    ClientManager* clientManager, const QString& username, QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      username_(username),
      model_(new ClientCodingSchemeModel(clientManager, this)),
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

void CodingSchemeMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);

    tableView_->setModel(proxyModel_);
    tableView_->setSortingEnabled(true);
    using cs = column_style;
    tableView_->setItemDelegate(new EntityItemDelegate({
        cs::text_left,   // Code
        cs::text_left,   // Name
        cs::text_left,   // AuthorityType
        cs::text_left,   // SubjectArea
        cs::text_left,   // Domain
        cs::text_left,   // Description
        cs::mono_center, // Version
        cs::text_left,   // ModifiedBy
        cs::mono_left    // RecordedAt
    }, tableView_));
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::ExtendedSelection);
    tableView_->setAlternatingRowColors(true);
    tableView_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    tableView_->verticalHeader()->setVisible(false);
    tableView_->sortByColumn(ClientCodingSchemeModel::Code, Qt::AscendingOrder);

    initializeTableSettings(tableView_, model_,
        "CodingSchemeMdiWindow",
        {},
        {900, 400}, 1);

    layout->addWidget(tableView_);
}

void CodingSchemeMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor),
        tr("Add"));
    addAction_->setToolTip(tr("Add new coding scheme"));

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Edit, IconUtils::DefaultIconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected coding scheme"));
    editAction_->setEnabled(false);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected coding scheme(s)"));
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
    refreshAction_->setToolTip(tr("Refresh coding schemes"));
    initializeStaleIndicator(refreshAction_, IconUtils::iconPath(Icon::ArrowSync));

    if (auto* layout = qobject_cast<QVBoxLayout*>(this->layout())) {
        layout->insertWidget(0, toolbar_);
    }
}

void CodingSchemeMdiWindow::setupConnections() {
    connect(model_, &ClientCodingSchemeModel::dataLoaded,
            this, &CodingSchemeMdiWindow::onDataLoaded);
    connect(model_, &ClientCodingSchemeModel::loadError,
            this, &CodingSchemeMdiWindow::onLoadError);
    connect(tableView_, &QTableView::doubleClicked,
            this, &CodingSchemeMdiWindow::onRowDoubleClicked);
    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &CodingSchemeMdiWindow::onSelectionChanged);

    connect(addAction_, &QAction::triggered, this, &CodingSchemeMdiWindow::onAddClicked);
    connect(editAction_, &QAction::triggered, this, &CodingSchemeMdiWindow::onEditClicked);
    connect(deleteAction_, &QAction::triggered, this, &CodingSchemeMdiWindow::onDeleteClicked);
    connect(refreshAction_, &QAction::triggered, this, &CodingSchemeMdiWindow::onRefreshClicked);
    connect(historyAction_, &QAction::triggered, this, &CodingSchemeMdiWindow::onHistoryClicked);
}

void CodingSchemeMdiWindow::onDataLoaded() {
    emit statusChanged(tr("Loaded %1 coding schemes").arg(model_->rowCount()));
    updateActionStates();
}

void CodingSchemeMdiWindow::onLoadError(const QString& error_message,
                                         const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void CodingSchemeMdiWindow::onRowDoubleClicked(const QModelIndex& index) {
    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* scheme = model_->getScheme(sourceIndex.row())) {
        emit showSchemeDetails(*scheme);
    }
}

void CodingSchemeMdiWindow::onAddClicked() {
    emit addNewRequested();
}

void CodingSchemeMdiWindow::onEditClicked() {
    auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* scheme = model_->getScheme(sourceIndex.row())) {
        emit showSchemeDetails(*scheme);
    }
}

void CodingSchemeMdiWindow::onDeleteClicked() {
    auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    std::vector<std::string> codes;
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* scheme = model_->getScheme(sourceIndex.row())) {
            codes.push_back(scheme->code);
        }
    }

    QString message = codes.size() == 1
        ? tr("Delete coding scheme '%1'?").arg(QString::fromStdString(codes[0]))
        : tr("Delete %1 coding schemes?").arg(codes.size());

    auto reply = MessageBoxHelper::question(this, tr("Confirm Delete"), message,
                                            QMessageBox::Yes | QMessageBox::No);
    if (reply != QMessageBox::Yes) return;

    QPointer<CodingSchemeMdiWindow> self = this;
    auto task = [self, codes = std::move(codes)]() -> bool {
        if (!self || !self->clientManager_) return false;

        dq::messaging::delete_coding_scheme_request request;
        request.codes = codes;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_coding_scheme_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return false;

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return false;

        auto response = dq::messaging::delete_coding_scheme_response::deserialize(*payload_result);
        return response && !response->results.empty() && response->results[0].success;
    };

    auto* watcher = new QFutureWatcher<bool>(this);
    connect(watcher, &QFutureWatcher<bool>::finished, this, [self, watcher]() {
        bool success = watcher->result();
        watcher->deleteLater();

        if (self) {
            if (success) {
                emit self->statusChanged(tr("Coding scheme(s) deleted successfully"));
                self->reload();
            } else {
                emit self->errorOccurred(tr("Failed to delete coding scheme(s)"));
            }
        }
    });

    watcher->setFuture(QtConcurrent::run(task));
}

void CodingSchemeMdiWindow::onRefreshClicked() {
    emit statusChanged(tr("Refreshing..."));
    model_->refresh();
}

void CodingSchemeMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void CodingSchemeMdiWindow::onHistoryClicked() {
    auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* scheme = model_->getScheme(sourceIndex.row())) {
        emit showSchemeHistory(QString::fromStdString(scheme->code));
    }
}

void CodingSchemeMdiWindow::updateActionStates() {
    auto selected = tableView_->selectionModel()->selectedRows();
    bool hasSelection = !selected.isEmpty();
    bool singleSelection = selected.size() == 1;

    editAction_->setEnabled(singleSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(singleSelection);
}

void CodingSchemeMdiWindow::reload() {
    clearStaleIndicator();
    model_->refresh();
}

}
