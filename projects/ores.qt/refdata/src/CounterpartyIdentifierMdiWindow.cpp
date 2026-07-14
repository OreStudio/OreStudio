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
#include "ores.qt/CounterpartyIdentifierMdiWindow.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/counterparty_identifier_protocol.hpp"
#include <QFutureWatcher>
#include <QHeaderView>
#include <QMessageBox>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

CounterpartyIdentifierMdiWindow::CounterpartyIdentifierMdiWindow(ClientManager* clientManager,
                                                                 const QString& username,
                                                                 QWidget* parent)
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
    , deleteAction_(nullptr)
    , historyAction_(nullptr) {

    setupUi();
    setupConnections();
    reload();
}

void CounterpartyIdentifierMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);

    setupToolbar();
    layout->addWidget(toolbar_);
    layout->addWidget(loadingBar());

    setupTable();
    layout->addWidget(tableView_);

    paginationWidget_ = new PaginationWidget(this);
    layout->addWidget(paginationWidget_);
}

void CounterpartyIdentifierMdiWindow::setupToolbar() {
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
    addAction_->setToolTip(tr("Add new counterparty identifier"));
    connect(addAction_, &QAction::triggered, this, &CounterpartyIdentifierMdiWindow::addNew);

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Edit, IconUtils::DefaultIconColor), tr("Edit"));
    editAction_->setToolTip(tr("Edit selected counterparty identifier"));
    editAction_->setEnabled(false);
    connect(editAction_, &QAction::triggered, this, &CounterpartyIdentifierMdiWindow::editSelected);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor), tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected counterparty identifier"));
    deleteAction_->setEnabled(false);
    connect(
        deleteAction_, &QAction::triggered, this, &CounterpartyIdentifierMdiWindow::deleteSelected);

    historyAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor), tr("History"));
    historyAction_->setToolTip(tr("View counterparty identifier history"));
    historyAction_->setEnabled(false);
    connect(historyAction_,
            &QAction::triggered,
            this,
            &CounterpartyIdentifierMdiWindow::viewHistorySelected);
}

void CounterpartyIdentifierMdiWindow::setupTable() {
    model_ = new ClientCounterpartyIdentifierModel(clientManager_, this);
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
                            "CounterpartyIdentifierListWindow",
                            {
                                ClientCounterpartyIdentifierModel::Description,
                            },
                            {900, 400},
                            1);
}

void CounterpartyIdentifierMdiWindow::setupConnections() {
    connect(model_,
            &ClientCounterpartyIdentifierModel::dataLoaded,
            this,
            &CounterpartyIdentifierMdiWindow::onDataLoaded);
    connect(model_,
            &ClientCounterpartyIdentifierModel::loadError,
            this,
            &CounterpartyIdentifierMdiWindow::onLoadError);

    connect(tableView_->selectionModel(),
            &QItemSelectionModel::selectionChanged,
            this,
            &CounterpartyIdentifierMdiWindow::onSelectionChanged);
    connect(tableView_,
            &QTableView::doubleClicked,
            this,
            &CounterpartyIdentifierMdiWindow::onDoubleClicked);

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

void CounterpartyIdentifierMdiWindow::doReload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading counterparty identifiers";
    clearStaleIndicator();
    emit statusChanged(tr("Loading counterparty identifiers..."));
    model_->refresh();
}

void CounterpartyIdentifierMdiWindow::onDataLoaded() {
    const auto loaded = model_->rowCount();
    const auto total = model_->total_available_count();
    emit statusChanged(tr("Loaded %1 of %2 counterparty identifiers").arg(loaded).arg(total));

    paginationWidget_->update_state(loaded, total);
    paginationWidget_->set_load_all_enabled(loaded < static_cast<int>(total) && total > 0 &&
                                            total <= 1000);
}

void CounterpartyIdentifierMdiWindow::onLoadError(const QString& error_message,
                                                  const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void CounterpartyIdentifierMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void CounterpartyIdentifierMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* counterpartyIdentifier = model_->getIdentifier(sourceIndex.row())) {
        emit showIdentifierDetails(*counterpartyIdentifier);
    }
}

void CounterpartyIdentifierMdiWindow::updateActionStates() {
    const bool hasSelection = tableView_->selectionModel()->hasSelection();
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(hasSelection);
}

void CounterpartyIdentifierMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new counterparty identifier requested";
    emit addNewRequested();
}

void CounterpartyIdentifierMdiWindow::editSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* counterpartyIdentifier = model_->getIdentifier(sourceIndex.row())) {
        emit showIdentifierDetails(*counterpartyIdentifier);
    }
}

void CounterpartyIdentifierMdiWindow::viewHistorySelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "View history requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* counterpartyIdentifier = model_->getIdentifier(sourceIndex.row())) {
        BOOST_LOG_SEV(lg(), debug)
            << "Emitting showIdentifierHistory for code: " << counterpartyIdentifier->id_value;
        emit showIdentifierHistory(*counterpartyIdentifier);
    }
}

void CounterpartyIdentifierMdiWindow::deleteSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete counterparty identifier while disconnected.");
        return;
    }

    std::vector<std::string> ids;
    std::vector<std::string> codes; // For display purposes
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* counterpartyIdentifier = model_->getIdentifier(sourceIndex.row())) {
            ids.push_back(boost::uuids::to_string(counterpartyIdentifier->id));
            codes.push_back(counterpartyIdentifier->id_value);
        }
    }

    if (ids.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid counterparty identifiers to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for " << ids.size()
                               << " counterparty identifiers";

    QString confirmMessage;
    if (ids.size() == 1) {
        confirmMessage = QString("Are you sure you want to delete counterparty identifier '%1'?")
                             .arg(QString::fromStdString(codes.front()));
    } else {
        confirmMessage =
            QString("Are you sure you want to delete %1 counterparty identifiers?").arg(ids.size());
    }

    auto reply = MessageBoxHelper::question(
        this, "Delete Counterparty Identifier", confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<CounterpartyIdentifierMdiWindow> self = this;
    using DeleteResult = std::vector<std::tuple<std::string, std::string, bool, std::string>>;

    auto task = [self, ids, codes]() -> DeleteResult {
        DeleteResult results;
        if (!self)
            return {};

        BOOST_LOG_SEV(lg(), debug)
            << "Making delete request for " << ids.size() << " counterparty identifiers";

        refdata::messaging::delete_counterparty_identifier_request request;
        request.ids = ids;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to send batch delete request";
            for (std::size_t i = 0; i < ids.size(); ++i) {
                results.push_back({ids[i], codes[i], false, "Failed to communicate with server"});
            }
            return results;
        }

        for (std::size_t i = 0; i < ids.size(); ++i) {
            results.push_back(
                {ids[i], codes[i], response_result->success, response_result->message});
        }

        return results;
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished, self, [self, watcher]() {
        auto results = watcher->result();
        watcher->deleteLater();

        int success_count = 0;
        int failure_count = 0;
        QString first_error;

        for (const auto& [id, code, success, message] : results) {
            if (success) {
                BOOST_LOG_SEV(lg(), debug) << "Counterparty Identifier deleted: " << code;
                success_count++;
                emit self->counterpartyIdentifierDeleted(QString::fromStdString(code));
            } else {
                BOOST_LOG_SEV(lg(), error)
                    << "Counterparty Identifier deletion failed: " << code << " - " << message;
                failure_count++;
                if (first_error.isEmpty()) {
                    first_error = QString::fromStdString(message);
                }
            }
        }

        self->model_->refresh();

        if (failure_count == 0) {
            QString msg =
                success_count == 1 ?
                    "Successfully deleted 1 counterparty identifier" :
                    QString("Successfully deleted %1 counterparty identifiers").arg(success_count);
            emit self->statusChanged(msg);
        } else if (success_count == 0) {
            QString msg = QString("Failed to delete %1 %2: %3")
                              .arg(failure_count)
                              .arg(failure_count == 1 ? "counterparty identifier" :
                                                        "counterparty identifiers")
                              .arg(first_error);
            emit self->errorOccurred(msg);
            MessageBoxHelper::critical(self, "Delete Failed", msg);
        } else {
            QString msg =
                QString("Deleted %1, failed to delete %2").arg(success_count).arg(failure_count);
            emit self->statusChanged(msg);
            MessageBoxHelper::warning(self, "Partial Success", msg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}


}
