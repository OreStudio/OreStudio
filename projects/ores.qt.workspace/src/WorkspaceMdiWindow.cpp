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
#include "ores.qt/WorkspaceMdiWindow.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.workspace.api/messaging/workspace_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

WorkspaceMdiWindow::WorkspaceMdiWindow(
    ClientManager* clientManager,
    const QString& username,
    QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      username_(username),
      toolbar_(nullptr),
      tableView_(nullptr),
      model_(nullptr),
      proxyModel_(nullptr),
      paginationWidget_(nullptr),
      reloadAction_(nullptr),
      addAction_(nullptr),
      editAction_(nullptr),
      deleteAction_(nullptr) {

    setupUi();
    setupConnections();
    reload();
}

void WorkspaceMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);

    setupToolbar();
    layout->addWidget(toolbar_);
    layout->addWidget(loadingBar());

    setupTable();
    layout->addWidget(tableView_);

    paginationWidget_ = new PaginationWidget(this);
    layout->addWidget(paginationWidget_);
}

void WorkspaceMdiWindow::setupToolbar() {
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

    initializeStaleIndicator(reloadAction_, IconUtils::iconPath(Icon::ArrowClockwise));

    toolbar_->addSeparator();

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Add, IconUtils::DefaultIconColor),
        tr("Add"));
    addAction_->setToolTip(tr("Add new workspace"));
    connect(addAction_, &QAction::triggered, this,
            &WorkspaceMdiWindow::addNew);

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Edit, IconUtils::DefaultIconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected workspace"));
    editAction_->setEnabled(false);
    connect(editAction_, &QAction::triggered, this,
            &WorkspaceMdiWindow::editSelected);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Delete, IconUtils::DefaultIconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected workspace"));
    deleteAction_->setEnabled(false);
    connect(deleteAction_, &QAction::triggered, this,
            &WorkspaceMdiWindow::deleteSelected);

}

void WorkspaceMdiWindow::setupTable() {
    model_ = new ClientWorkspaceModel(clientManager_, this);
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
        "WorkspaceListWindow",
        {ClientWorkspaceModel::Description},
        {900, 400}, 1);
}

void WorkspaceMdiWindow::setupConnections() {
    connect(model_, &ClientWorkspaceModel::dataLoaded,
            this, &WorkspaceMdiWindow::onDataLoaded);
    connect(model_, &ClientWorkspaceModel::loadError,
            this, &WorkspaceMdiWindow::onLoadError);

    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &WorkspaceMdiWindow::onSelectionChanged);
    connect(tableView_, &QTableView::doubleClicked,
            this, &WorkspaceMdiWindow::onDoubleClicked);

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

    connectModel(model_);
}

void WorkspaceMdiWindow::doReload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading workspaces";
    clearStaleIndicator();
    emit statusChanged(tr("Loading workspaces..."));
    model_->refresh();
}

void WorkspaceMdiWindow::onDataLoaded() {
    const auto loaded = model_->rowCount();
    const auto total = model_->total_available_count();
    emit statusChanged(tr("Loaded %1 of %2 workspaces").arg(loaded).arg(total));

    paginationWidget_->update_state(loaded, total);
    paginationWidget_->set_load_all_enabled(
        loaded < static_cast<int>(total) && total > 0 && total <= 1000);
}

void WorkspaceMdiWindow::onLoadError(const QString& error_message,
                                          const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void WorkspaceMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void WorkspaceMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* workspace = model_->getWorkspace(sourceIndex.row())) {
        emit showWorkspaceDetails(*workspace);
    }
}

void WorkspaceMdiWindow::updateActionStates() {
    const bool hasSelection = tableView_->selectionModel()->hasSelection();
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
}

void WorkspaceMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new workspace requested";
    emit addNewRequested();
}

void WorkspaceMdiWindow::editSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* workspace = model_->getWorkspace(sourceIndex.row())) {
        emit showWorkspaceDetails(*workspace);
    }
}

void WorkspaceMdiWindow::deleteSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Archive requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot archive workspace while disconnected.");
        return;
    }

    int workspace_id = 0;
    QString workspace_name;
    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* ws = model_->getWorkspace(sourceIndex.row())) {
        workspace_id = ws->id;
        workspace_name = QString::fromStdString(ws->name);
    }

    if (workspace_id == 0) {
        BOOST_LOG_SEV(lg(), warn) << "No valid workspace selected for archive";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Archive requested for workspace id: " << workspace_id;

    auto reply = MessageBoxHelper::question(this, "Archive Workspace",
        QString("Are you sure you want to archive workspace '%1'?").arg(workspace_name),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Archive cancelled by user";
        return;
    }

    QPointer<WorkspaceMdiWindow> self = this;
    using ArchiveResult = std::pair<bool, std::string>;

    auto task = [self, workspace_id]() -> ArchiveResult {
        if (!self) return {};
        BOOST_LOG_SEV(lg(), debug) << "Sending archive request for workspace id: " << workspace_id;
        workspace::messaging::archive_workspace_request request;
        request.id = workspace_id;
        auto result = self->clientManager_->process_authenticated_request(std::move(request));
        if (!result) return {false, "Failed to communicate with server"};
        return {result->success, result->message};
    };

    auto* watcher = new QFutureWatcher<ArchiveResult>(self);
    connect(watcher, &QFutureWatcher<ArchiveResult>::finished,
            self, [self, watcher, workspace_id, workspace_name]() {
        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), debug) << "Workspace archived: " << workspace_id;
            emit self->workspaceDeleted(QString::number(workspace_id));
            self->model_->refresh();
            emit self->statusChanged(
                QString("Workspace '%1' archived successfully").arg(workspace_name));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Archive failed for workspace " << workspace_id
                                       << ": " << message;
            auto msg = QString::fromStdString(message);
            emit self->errorOccurred(msg);
            MessageBoxHelper::critical(self, "Archive Failed", msg);
        }
    });

    QFuture<ArchiveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
