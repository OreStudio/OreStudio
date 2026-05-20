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

#include <map>
#include <QVBoxLayout>
#include <QHeaderView>
#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/string_generator.hpp>
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
      treeWidget_(nullptr),
      model_(nullptr),
      reloadAction_(nullptr),
      addAction_(nullptr),
      openAction_(nullptr),
      editAction_(nullptr),
      archiveAction_(nullptr),
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

    setupTree();
    layout->addWidget(treeWidget_);
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

    toolbar_->addSeparator();

    openAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::FolderOpen, IconUtils::DefaultIconColor),
        tr("Open"));
    openAction_->setToolTip(tr("Open selected workspace (make it the active context)"));
    openAction_->setEnabled(false);
    connect(openAction_, &QAction::triggered, this,
            &WorkspaceMdiWindow::openSelected);

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Edit, IconUtils::DefaultIconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected workspace"));
    editAction_->setEnabled(false);
    connect(editAction_, &QAction::triggered, this,
            &WorkspaceMdiWindow::editSelected);

    archiveAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Archive, IconUtils::DefaultIconColor),
        tr("Archive"));
    archiveAction_->setToolTip(tr("Archive selected workspace"));
    archiveAction_->setEnabled(false);
    connect(archiveAction_, &QAction::triggered, this,
            &WorkspaceMdiWindow::archiveSelected);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Delete, IconUtils::DefaultIconColor),
        tr("Delete"));
    deleteAction_->setToolTip(
        tr("Permanently soft-delete selected workspace and associated data"));
    deleteAction_->setEnabled(false);
    connect(deleteAction_, &QAction::triggered, this,
            &WorkspaceMdiWindow::deleteSelected);
}

void WorkspaceMdiWindow::setupTree() {
    model_ = new ClientWorkspaceModel(clientManager_, this);

    treeWidget_ = new QTreeWidget(this);
    treeWidget_->setColumnCount(3);
    treeWidget_->setHeaderLabels({tr("Name"), tr("Status"), tr("Modified By")});
    treeWidget_->setRootIsDecorated(true);
    treeWidget_->setAnimated(true);
    treeWidget_->setAlternatingRowColors(true);
    treeWidget_->setSelectionBehavior(QAbstractItemView::SelectRows);
    treeWidget_->setSelectionMode(QAbstractItemView::SingleSelection);
    treeWidget_->header()->setStretchLastSection(false);
    treeWidget_->header()->setSectionResizeMode(0, QHeaderView::Stretch);
}

void WorkspaceMdiWindow::setupConnections() {
    connect(model_, &ClientWorkspaceModel::dataLoaded,
            this, &WorkspaceMdiWindow::onDataLoaded);
    connect(model_, &ClientWorkspaceModel::loadError,
            this, &WorkspaceMdiWindow::onLoadError);

    connect(treeWidget_, &QTreeWidget::itemSelectionChanged,
            this, &WorkspaceMdiWindow::onSelectionChanged);
    connect(treeWidget_, &QTreeWidget::itemDoubleClicked,
            this, &WorkspaceMdiWindow::onItemDoubleClicked);

    connectModel(model_);
}

void WorkspaceMdiWindow::doReload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading workspaces";
    clearStaleIndicator();
    emit statusChanged(tr("Loading workspaces..."));
    model_->refresh();
}

namespace {

using children_map_t = std::map<
    boost::uuids::uuid,
    std::vector<const workspace::domain::workspace*>>;

void addTreeItems(QTreeWidgetItem* parent,
    const boost::uuids::uuid& parent_id,
    const children_map_t& children_map) {

    auto it = children_map.find(parent_id);
    if (it == children_map.end())
        return;

    for (const auto* ws : it->second) {
        const QString id_str =
            QString::fromStdString(boost::uuids::to_string(ws->id));
        auto* item = new QTreeWidgetItem(parent);
        item->setText(0, QString::fromStdString(ws->name));
        item->setText(1, QString::fromStdString(ws->status_code));
        item->setText(2, QString::fromStdString(ws->modified_by));
        item->setData(0, Qt::UserRole, id_str);
        addTreeItems(item, ws->id, children_map);
    }
}

} // namespace

void WorkspaceMdiWindow::buildTree() {
    treeWidget_->clear();

    const auto& all = model_->workspaces();
    if (all.empty())
        return;

    std::map<boost::uuids::uuid, const workspace::domain::workspace*> by_id;
    for (const auto& ws : all)
        by_id[ws.id] = &ws;

    children_map_t children_map;

    std::vector<const workspace::domain::workspace*> roots;
    for (const auto& ws : all) {
        if (!ws.parent_workspace_id) {
            roots.push_back(&ws);
        } else {
            if (by_id.count(*ws.parent_workspace_id)) {
                children_map[*ws.parent_workspace_id].push_back(&ws);
            } else {
                roots.push_back(&ws);
            }
        }
    }

    for (const auto* ws : roots) {
        const QString id_str =
            QString::fromStdString(boost::uuids::to_string(ws->id));
        auto* item = new QTreeWidgetItem(treeWidget_);
        item->setText(0, QString::fromStdString(ws->name));
        item->setText(1, QString::fromStdString(ws->status_code));
        item->setText(2, QString::fromStdString(ws->modified_by));
        item->setData(0, Qt::UserRole, id_str);
        addTreeItems(item, ws->id, children_map);
    }

    treeWidget_->expandAll();
}

void WorkspaceMdiWindow::onDataLoaded() {
    const auto loaded = static_cast<int>(model_->workspaces().size());
    const auto total = model_->total_available_count();
    emit statusChanged(tr("Loaded %1 of %2 workspaces").arg(loaded).arg(total));

    buildTree();
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

void WorkspaceMdiWindow::onItemDoubleClicked(QTreeWidgetItem* item, int /*column*/) {
    if (!item)
        return;

    const auto target_id = boost::uuids::string_generator()(
        item->data(0, Qt::UserRole).toString().toStdString());
    for (const auto& ws : model_->workspaces()) {
        if (ws.id == target_id) {
            emit showWorkspaceDetails(ws);
            return;
        }
    }
}

void WorkspaceMdiWindow::updateActionStates() {
    const bool has_selection = !treeWidget_->selectedItems().isEmpty();
    openAction_->setEnabled(has_selection);
    editAction_->setEnabled(has_selection);
    archiveAction_->setEnabled(has_selection);
    deleteAction_->setEnabled(has_selection);
}

void WorkspaceMdiWindow::openSelected() {
    auto selected = treeWidget_->selectedItems();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Open requested but no item selected";
        return;
    }

    const auto id_str = selected.first()->data(0, Qt::UserRole).toString();
    const auto target_id =
        boost::uuids::string_generator()(id_str.toStdString());
    for (const auto& ws : model_->workspaces()) {
        if (ws.id == target_id) {
            emit workspaceActivated(ws);
            return;
        }
    }
    BOOST_LOG_SEV(lg(), warn) << "Could not find workspace for id: "
                              << id_str.toStdString();
}

void WorkspaceMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new workspace requested";
    emit addNewRequested();
}

void WorkspaceMdiWindow::editSelected() {
    auto selected = treeWidget_->selectedItems();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no item selected";
        return;
    }

    const auto target_id = boost::uuids::string_generator()(
        selected.first()->data(0, Qt::UserRole).toString().toStdString());
    for (const auto& ws : model_->workspaces()) {
        if (ws.id == target_id) {
            emit showWorkspaceDetails(ws);
            return;
        }
    }
}

void WorkspaceMdiWindow::archiveSelected() {
    auto selected = treeWidget_->selectedItems();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Archive requested but no item selected";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, tr("Disconnected"),
            tr("Cannot archive workspace while disconnected."));
        return;
    }

    const auto id_str = selected.first()->data(0, Qt::UserRole).toString();
    const auto target_id =
        boost::uuids::string_generator()(id_str.toStdString());
    std::string workspace_id;
    QString workspace_name;
    for (const auto& ws : model_->workspaces()) {
        if (ws.id == target_id) {
            workspace_id = boost::uuids::to_string(ws.id);
            workspace_name = QString::fromStdString(ws.name);
            break;
        }
    }

    if (workspace_id.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid workspace selected for archive";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Archive requested for workspace id: "
                               << workspace_id;

    const std::string actor = username_.toStdString();

    auto reply = MessageBoxHelper::question(this, tr("Archive Workspace"),
        tr("Are you sure you want to archive workspace '%1'?").arg(workspace_name),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Archive cancelled by user";
        return;
    }

    QPointer<WorkspaceMdiWindow> self = this;
    using OpResult = std::pair<bool, std::string>;

    auto task = [self, workspace_id, actor]() -> OpResult {
        if (!self) return {};
        BOOST_LOG_SEV(lg(), debug) << "Sending archive request for workspace id: "
                                   << workspace_id;
        workspace::messaging::archive_workspace_request request;
        request.id = workspace_id;
        request.modified_by = actor;
        request.change_reason_code = "user.archive";
        request.change_commentary = "Archived by user";
        auto result = self->clientManager_->process_authenticated_request(
            std::move(request));
        if (!result) return {false, "Failed to communicate with server"};
        return {result->success, result->message};
    };

    auto* watcher = new QFutureWatcher<OpResult>(self);
    connect(watcher, &QFutureWatcher<OpResult>::finished,
            self, [self, watcher, workspace_id, workspace_name]() {
        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), debug) << "Workspace archived: " << workspace_id;
            emit self->workspaceDeleted(QString::fromStdString(workspace_id));
            self->model_->refresh();
            emit self->statusChanged(
                tr("Workspace '%1' archived successfully").arg(workspace_name));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Archive failed for workspace "
                                       << workspace_id << ": " << message;
            auto msg = QString::fromStdString(message);
            emit self->errorOccurred(msg);
            MessageBoxHelper::critical(self, tr("Archive Failed"), msg);
        }
    });

    QFuture<OpResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void WorkspaceMdiWindow::deleteSelected() {
    auto selected = treeWidget_->selectedItems();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no item selected";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, tr("Disconnected"),
            tr("Cannot delete workspace while disconnected."));
        return;
    }

    const auto id_str = selected.first()->data(0, Qt::UserRole).toString();
    const auto target_id =
        boost::uuids::string_generator()(id_str.toStdString());
    std::string workspace_id;
    QString workspace_name;
    for (const auto& ws : model_->workspaces()) {
        if (ws.id == target_id) {
            workspace_id = boost::uuids::to_string(ws.id);
            workspace_name = QString::fromStdString(ws.name);
            break;
        }
    }

    if (workspace_id.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid workspace selected for delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for workspace id: "
                               << workspace_id;

    const std::string actor = username_.toStdString();

    auto reply = MessageBoxHelper::question(this, tr("Delete Workspace"),
        tr("Permanently delete workspace '%1' and all its associated data?\n\n"
           "This action cannot be undone.").arg(workspace_name),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<WorkspaceMdiWindow> self = this;
    using OpResult = std::pair<bool, std::string>;

    auto task = [self, workspace_id, actor]() -> OpResult {
        if (!self) return {};
        BOOST_LOG_SEV(lg(), debug) << "Sending delete request for workspace id: "
                                   << workspace_id;
        workspace::messaging::remove_workspace_request request;
        request.id = workspace_id;
        request.modified_by = actor;
        request.change_reason_code = "user.delete";
        request.change_commentary = "Deleted by user";
        auto result = self->clientManager_->process_authenticated_request(
            std::move(request));
        if (!result) return {false, "Failed to communicate with server"};
        return {result->success, result->message};
    };

    auto* watcher = new QFutureWatcher<OpResult>(self);
    connect(watcher, &QFutureWatcher<OpResult>::finished,
            self, [self, watcher, workspace_id, workspace_name]() {
        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), debug) << "Workspace deleted: " << workspace_id;
            emit self->workspaceDeleted(QString::fromStdString(workspace_id));
            self->model_->refresh();
            emit self->statusChanged(
                tr("Workspace '%1' deleted successfully").arg(workspace_name));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Delete failed for workspace "
                                       << workspace_id << ": " << message;
            auto msg = QString::fromStdString(message);
            emit self->errorOccurred(msg);
            MessageBoxHelper::critical(self, tr("Delete Failed"), msg);
        }
    });

    QFuture<OpResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
