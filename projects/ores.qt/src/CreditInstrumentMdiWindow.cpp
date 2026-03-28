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
#include "ores.qt/CreditInstrumentMdiWindow.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

CreditInstrumentMdiWindow::CreditInstrumentMdiWindow(
    ClientManager* clientManager,
    QString username,
    QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      username_(std::move(username)),
      toolbar_(nullptr),
      tableView_(nullptr),
      model_(nullptr),
      proxyModel_(nullptr),
      paginationWidget_(nullptr),
      reloadAction_(nullptr),
      addAction_(nullptr),
      editAction_(nullptr),
      deleteAction_(nullptr),
      historyAction_(nullptr) {

    setupUi();
    setupConnections();
    reload();
}

void CreditInstrumentMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);
    setupToolbar();
    layout->addWidget(toolbar_);
    setupTable();
    layout->addWidget(tableView_);
    paginationWidget_ = new PaginationWidget(this);
    layout->addWidget(paginationWidget_);
}

void CreditInstrumentMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    connect(reloadAction_, &QAction::triggered, this,
            &EntityListMdiWindow::reload);
    initializeStaleIndicator(reloadAction_,
        IconUtils::iconPath(Icon::ArrowClockwise));

    toolbar_->addSeparator();

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor),
        tr("Add"));
    addAction_->setToolTip(tr("Add new credit instrument"));
    connect(addAction_, &QAction::triggered, this,
            &CreditInstrumentMdiWindow::addNew);

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Edit, IconUtils::DefaultIconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected credit instrument"));
    editAction_->setEnabled(false);
    connect(editAction_, &QAction::triggered, this,
            &CreditInstrumentMdiWindow::editSelected);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Delete, IconUtils::DefaultIconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected credit instrument"));
    deleteAction_->setEnabled(false);
    connect(deleteAction_, &QAction::triggered, this,
            &CreditInstrumentMdiWindow::deleteSelected);

    historyAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::History, IconUtils::DefaultIconColor),
        tr("History"));
    historyAction_->setToolTip(tr("View credit instrument history"));
    historyAction_->setEnabled(false);
    connect(historyAction_, &QAction::triggered, this,
            &CreditInstrumentMdiWindow::viewHistorySelected);
}

void CreditInstrumentMdiWindow::setupTable() {
    model_ = new ClientCreditInstrumentModel(clientManager_, this);
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

    initializeTableSettings(tableView_, model_,
        "CreditInstrumentListWindow",
        {ClientCreditInstrumentModel::TradeType},
        {1000, 400}, 1);
}

void CreditInstrumentMdiWindow::setupConnections() {
    connect(model_, &ClientCreditInstrumentModel::dataLoaded,
            this, &CreditInstrumentMdiWindow::onDataLoaded);
    connect(model_, &ClientCreditInstrumentModel::loadError,
            this, &CreditInstrumentMdiWindow::onLoadError);

    connect(tableView_->selectionModel(),
            &QItemSelectionModel::selectionChanged,
            this, &CreditInstrumentMdiWindow::onSelectionChanged);
    connect(tableView_, &QTableView::doubleClicked,
            this, &CreditInstrumentMdiWindow::onDoubleClicked);

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

void CreditInstrumentMdiWindow::doReload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading credit instruments";
    clearStaleIndicator();
    emit statusChanged(tr("Loading credit instruments..."));
    model_->refresh();
}

void CreditInstrumentMdiWindow::onDataLoaded() {
    const auto loaded = model_->rowCount();
    const auto total = model_->total_available_count();
    emit statusChanged(
        tr("Loaded %1 of %2 credit instruments").arg(loaded).arg(total));
    paginationWidget_->update_state(loaded, total);
    paginationWidget_->set_load_all_enabled(
        loaded < static_cast<int>(total) && total > 0 && total <= 1000);
}

void CreditInstrumentMdiWindow::onLoadError(const QString& error_message,
                                            const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void CreditInstrumentMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void CreditInstrumentMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid()) return;
    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* v = model_->getCreditInstrument(sourceIndex.row())) {
        emit showCreditInstrumentDetails(*v);
    }
}

void CreditInstrumentMdiWindow::updateActionStates() {
    const bool hasSelection = tableView_->selectionModel()->hasSelection();
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(hasSelection);
}

void CreditInstrumentMdiWindow::addNew() {
    emit addNewRequested();
}

void CreditInstrumentMdiWindow::editSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;
    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* v = model_->getCreditInstrument(sourceIndex.row())) {
        emit showCreditInstrumentDetails(*v);
    }
}

void CreditInstrumentMdiWindow::viewHistorySelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;
    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* v = model_->getCreditInstrument(sourceIndex.row())) {
        emit showCreditInstrumentHistory(*v);
    }
}

void CreditInstrumentMdiWindow::deleteSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete credit instrument while disconnected.");
        return;
    }

    std::vector<std::string> ids;
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* v = model_->getCreditInstrument(sourceIndex.row())) {
            ids.push_back(boost::uuids::to_string(v->id));
        }
    }

    if (ids.empty()) return;

    QString confirmMessage = ids.size() == 1
        ? QString("Are you sure you want to delete credit instrument '%1'?")
              .arg(QString::fromStdString(ids.front()))
        : QString("Are you sure you want to delete %1 credit instruments?")
              .arg(ids.size());

    auto reply = MessageBoxHelper::question(this, "Delete Credit Instrument",
        confirmMessage, QMessageBox::Yes | QMessageBox::No);
    if (reply != QMessageBox::Yes) return;

    QPointer<CreditInstrumentMdiWindow> self = this;
    using DeleteResult =
        std::vector<std::pair<std::string, std::pair<bool, std::string>>>;

    auto task = [self, ids]() -> DeleteResult {
        if (!self) return {};

        trading::messaging::delete_credit_instrument_request request;
        request.ids = ids;

        auto response_result = self->clientManager_->
            process_authenticated_request(std::move(request));

        if (!response_result) {
            DeleteResult results;
            for (const auto& id : ids)
                results.push_back({id, {false, "Failed to communicate with server"}});
            return results;
        }

        return response_result->results;
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, watcher]() {
        auto results = watcher->result();
        watcher->deleteLater();

        int success_count = 0;
        int failure_count = 0;
        QString first_error;

        for (const auto& [id, result] : results) {
            if (result.first) {
                success_count++;
                emit self->creditInstrumentDeleted(QString::fromStdString(id));
            } else {
                failure_count++;
                if (first_error.isEmpty())
                    first_error = QString::fromStdString(result.second);
            }
        }

        self->model_->refresh();

        if (failure_count == 0) {
            emit self->statusChanged(
                QString("Deleted %1 credit instrument(s)").arg(success_count));
        } else if (success_count == 0) {
            QString msg = QString("Failed to delete: %1").arg(first_error);
            emit self->errorOccurred(msg);
            MessageBoxHelper::critical(self, "Delete Failed", msg);
        } else {
            emit self->statusChanged(
                QString("Deleted %1, failed %2").arg(success_count).arg(failure_count));
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
