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
#include "ores.qt/TenantMdiWindow.hpp"

#include <QtConcurrent>
#include <QFutureWatcher>
#include <QHeaderView>
#include <QMessageBox>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.iam/messaging/tenant_protocol.hpp"
#include "ores.comms/net/client_session.hpp"

namespace ores::qt {

using namespace ores::logging;

TenantMdiWindow::TenantMdiWindow(ClientManager* clientManager,
                                   const QString& username,
                                   QWidget* parent)
    : EntityListMdiWindow(parent),
      verticalLayout_(new QVBoxLayout(this)),
      tenantTableView_(new QTableView(this)),
      toolBar_(new QToolBar(this)),
      reloadAction_(new QAction("Reload", this)),
      addAction_(new QAction("Add", this)),
      editAction_(new QAction("Edit", this)),
      deleteAction_(new QAction("Delete", this)),
      historyAction_(new QAction("History", this)),
      tenantModel_(std::make_unique<ClientTenantModel>(clientManager)),
      proxyModel_(new QSortFilterProxyModel(this)),
      clientManager_(clientManager),
      username_(username) {

    BOOST_LOG_SEV(lg(), debug) << "Creating tenant MDI window";

    toolBar_->setMovable(false);
    toolBar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);

    // Setup reload action with normal and stale icons
    setupReloadAction();
    toolBar_->addAction(reloadAction_);

    toolBar_->addSeparator();

    addAction_->setIcon(IconUtils::createRecoloredIcon(
            Icon::Add, IconUtils::DefaultIconColor));
    addAction_->setToolTip("Add new tenant");
    connect(addAction_, &QAction::triggered, this, &TenantMdiWindow::addNew);
    toolBar_->addAction(addAction_);

    editAction_->setIcon(IconUtils::createRecoloredIcon(
            Icon::Edit, IconUtils::DefaultIconColor));
    editAction_->setToolTip("Edit selected tenant");
    connect(editAction_, &QAction::triggered, this,
        &TenantMdiWindow::editSelected);
    toolBar_->addAction(editAction_);

    deleteAction_->setIcon(IconUtils::createRecoloredIcon(
            Icon::Delete, IconUtils::DefaultIconColor));
    deleteAction_->setToolTip("Delete selected tenant(s)");
    connect(deleteAction_, &QAction::triggered, this,
        &TenantMdiWindow::deleteSelected);
    toolBar_->addAction(deleteAction_);

    toolBar_->addSeparator();

    historyAction_->setIcon(IconUtils::createRecoloredIcon(
            Icon::History, IconUtils::DefaultIconColor));
    historyAction_->setToolTip("View tenant history");
    connect(historyAction_, &QAction::triggered, this,
        &TenantMdiWindow::viewHistorySelected);
    toolBar_->addAction(historyAction_);

    verticalLayout_->addWidget(toolBar_);
    verticalLayout_->addWidget(tenantTableView_);

    tenantTableView_->setObjectName("tenantTableView");
    tenantTableView_->setAlternatingRowColors(true);
    tenantTableView_->setSelectionMode(QAbstractItemView::ExtendedSelection);
    tenantTableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tenantTableView_->setWordWrap(false);

    proxyModel_->setSourceModel(tenantModel_.get());
    tenantTableView_->setModel(proxyModel_);
    tenantTableView_->setSortingEnabled(true);
    tenantTableView_->sortByColumn(0, Qt::AscendingOrder);

    QHeaderView* horizontalHeader(tenantTableView_->horizontalHeader());
    tenantTableView_->verticalHeader()->setSectionResizeMode(QHeaderView::Fixed);
    horizontalHeader->setSectionResizeMode(QHeaderView::ResizeToContents);

    // Connect signals
    connect(tenantModel_.get(), &ClientTenantModel::dataLoaded,
            this, &TenantMdiWindow::onDataLoaded);
    connect(tenantModel_.get(), &ClientTenantModel::loadError,
            this, &TenantMdiWindow::onLoadError);
    connect(tenantTableView_, &QTableView::doubleClicked,
            this, &TenantMdiWindow::onRowDoubleClicked);
    connect(tenantTableView_->selectionModel(),
        &QItemSelectionModel::selectionChanged,
            this, &TenantMdiWindow::onSelectionChanged);

    // Connect connection state signals
    if (clientManager_) {
        connect(clientManager_, &ClientManager::connected,
                this, &TenantMdiWindow::onConnectionStateChanged);
        connect(clientManager_, &ClientManager::disconnected,
                this, &TenantMdiWindow::onConnectionStateChanged);
    }

    updateActionStates();

    emit statusChanged("Loading tenants...");

    // Initial load (only if logged in)
    if (clientManager_->isLoggedIn()) {
        tenantModel_->refresh();
    } else {
        emit statusChanged("Disconnected - Offline");
        toolBar_->setEnabled(false);
    }
}

TenantMdiWindow::~TenantMdiWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Destroying tenant MDI window";

    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
        watcher->waitForFinished();
    }
}

void TenantMdiWindow::onConnectionStateChanged() {
    const bool connected = clientManager_ && clientManager_->isConnected();
    toolBar_->setEnabled(connected);

    if (connected) {
        emit statusChanged("Connected");
    } else {
        emit statusChanged("Disconnected - Offline");
    }
}

void TenantMdiWindow::reload() {
    BOOST_LOG_SEV(lg(), debug) << "Reload requested";
    if (!clientManager_->isConnected()) {
        emit statusChanged("Cannot reload - Disconnected");
        return;
    }
    emit statusChanged("Reloading tenants...");
    clearStaleIndicator();
    tenantModel_->refresh();
}

void TenantMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new tenant requested";
    emit addNewRequested();
}

void TenantMdiWindow::onDataLoaded() {
    const auto loaded = tenantModel_->rowCount();

    const QString message = QString("Loaded %1 tenants").arg(loaded);
    emit statusChanged(message);
    BOOST_LOG_SEV(lg(), debug) << "Tenant data loaded successfully: "
                               << loaded << " tenants";

    if (tenantModel_->rowCount() > 0 &&
        tenantTableView_->selectionModel()->selectedRows().isEmpty()) {
        tenantTableView_->selectRow(0);
        BOOST_LOG_SEV(lg(), debug) << "Auto-selected first row";
    }
}

void TenantMdiWindow::onLoadError(const QString& error_message,
                                   const QString& details) {
    emit errorOccurred(error_message);
    BOOST_LOG_SEV(lg(), error) << "Error loading tenants: "
                              << error_message.toStdString();

    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void TenantMdiWindow::onRowDoubleClicked(const QModelIndex& index) {
    if (!index.isValid()) {
        return;
    }

    const QModelIndex sourceIndex = proxyModel_->mapToSource(index);
    const auto* tenant = tenantModel_->getTenant(sourceIndex.row());
    if (!tenant) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get tenant for row: "
                                 << sourceIndex.row();
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Emitting showTenantDetails for tenant: "
                               << tenant->code;
    emit showTenantDetails(*tenant);
}

void TenantMdiWindow::onSelectionChanged() {
    const int selection_count = tenantTableView_->selectionModel()->selectedRows().count();
    updateActionStates();
    emit selectionChanged(selection_count);
}

void TenantMdiWindow::editSelected() {
    const auto selected = tenantTableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    onRowDoubleClicked(selected.first());
}

void TenantMdiWindow::deleteSelected() {
    const auto selected = tenantTableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
         MessageBoxHelper::warning(this, "Disconnected",
             "Cannot delete tenant while disconnected.");
         return;
    }

    std::vector<boost::uuids::uuid> tenant_ids;
    for (const auto& index : selected) {
        const QModelIndex sourceIndex = proxyModel_->mapToSource(index);
        const auto* tenant = tenantModel_->getTenant(sourceIndex.row());
        if (tenant)
            tenant_ids.push_back(tenant->id);
    }

    if (tenant_ids.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid tenants to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for " << tenant_ids.size()
                               << " tenants";

    QString confirmMessage;
    if (tenant_ids.size() == 1) {
        const QModelIndex sourceIndex = proxyModel_->mapToSource(selected.first());
        const auto* tenant = tenantModel_->getTenant(sourceIndex.row());
        confirmMessage = QString("Are you sure you want to delete tenant '%1'?")
            .arg(QString::fromStdString(tenant->code));
    } else {
        confirmMessage = QString("Are you sure you want to delete %1 tenants?")
            .arg(tenant_ids.size());
    }

    auto reply = MessageBoxHelper::question(this, "Delete Tenant",
        confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user.";
        return;
    }

    QPointer<TenantMdiWindow> self = this;
    using DeleteResult = std::vector<iam::messaging::delete_tenant_result>;

    auto task = [self, tenant_ids]() -> DeleteResult {
        if (!self) {
            return {};
        }

        BOOST_LOG_SEV(lg(), debug) << "Deleting " << tenant_ids.size() << " tenants";

        iam::messaging::delete_tenant_request request{tenant_ids};
        auto response = self->clientManager_->
            process_authenticated_request(std::move(request));

        if (response) {
            return std::move(response->results);
        }

        // Handle error from process_authenticated_request
        const auto error_string = comms::net::to_string(response.error());
        BOOST_LOG_SEV(lg(), error) << "Failed to delete tenants: " << error_string;

        DeleteResult error_results;
        const auto error_message = "Failed to delete tenants: " + error_string;
        for (const auto& id : tenant_ids) {
            error_results.push_back({id, false, error_message});
        }
        return error_results;
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, watcher]() {
        auto results = watcher->result();
        watcher->deleteLater();

        int success_count = 0;
        int failure_count = 0;
        QString first_error;

        for (const auto& result : results) {
            if (result.success) {
                BOOST_LOG_SEV(lg(), debug) << "Tenant deleted successfully: "
                                           << boost::uuids::to_string(result.id);
                emit self->tenantDeleted(QString::fromStdString(
                    boost::uuids::to_string(result.id)));
                success_count++;
            } else {
                BOOST_LOG_SEV(lg(), error) << "Tenant deletion failed: "
                                           << result.message;
                failure_count++;
                if (first_error.isEmpty()) {
                    first_error = QString::fromStdString(result.message);
                }
            }
        }

        self->tenantModel_->refresh();
        if (failure_count == 0) {
            QString msg = success_count == 1
                ? "Deleted 1 tenant"
                : QString("Deleted %1 tenants").arg(success_count);
            emit self->statusChanged(msg);
        } else if (success_count == 0) {
            QString msg = QString("Failed to delete %1 %2: %3")
                .arg(failure_count)
                .arg(failure_count == 1 ? "tenant" : "tenants")
                .arg(first_error);
            emit self->errorOccurred(msg);
            MessageBoxHelper::critical(self, "Delete Failed", msg);
        } else {
            QString msg = QString("Deleted %1 %2, failed to delete %3 %4")
                .arg(success_count)
                .arg(success_count == 1 ? "tenant" : "tenants")
                .arg(failure_count)
                .arg(failure_count == 1 ? "tenant" : "tenants");
            emit self->statusChanged(msg);
            MessageBoxHelper::warning(self, "Partial Success", msg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void TenantMdiWindow::viewHistorySelected() {
    const auto selected = tenantTableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "History requested but no row selected";
        return;
    }

    const QModelIndex sourceIndex = proxyModel_->mapToSource(selected.first());
    const auto* tenant = tenantModel_->getTenant(sourceIndex.row());
    if (!tenant) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get tenant for row: "
                                 << sourceIndex.row();
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Emitting showTenantHistory for tenant: "
                               << tenant->code;
    emit showTenantHistory(QString::fromStdString(tenant->code));
}

void TenantMdiWindow::updateActionStates() {
    const int selection_count = tenantTableView_
        ->selectionModel()->selectedRows().count();
    const bool hasSingleSelection = selection_count == 1;
    const bool hasSelection = selection_count > 0;

    // Edit and history only work on single selection
    editAction_->setEnabled(hasSingleSelection);
    historyAction_->setEnabled(hasSingleSelection);

    // Delete supports multi-selection
    deleteAction_->setEnabled(hasSelection);
}

void TenantMdiWindow::setupReloadAction() {
    reloadAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowSync, IconUtils::DefaultIconColor));
    connect(reloadAction_, &QAction::triggered, this, &TenantMdiWindow::reload);

    initializeStaleIndicator(reloadAction_, IconUtils::iconPath(Icon::ArrowSync));
}

}
