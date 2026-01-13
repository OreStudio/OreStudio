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
#include "ores.qt/ConnectionBrowserMdiWindow.hpp"
#include "ores.qt/ConnectionTreeModel.hpp"
#include "ores.qt/ConnectionDetailDialog.hpp"
#include "ores.qt/FolderDetailDialog.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.connections/service/connection_manager.hpp"
#include <QHeaderView>
#include <QMenu>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

ConnectionBrowserMdiWindow::ConnectionBrowserMdiWindow(
    connections::service::connection_manager* manager,
    QWidget* parent)
    : QWidget(parent),
      manager_(manager) {

    setupUI();
    updateActionStates();
}

ConnectionBrowserMdiWindow::~ConnectionBrowserMdiWindow() = default;

QSize ConnectionBrowserMdiWindow::sizeHint() const {
    return QSize(600, 400);
}

void ConnectionBrowserMdiWindow::setupUI() {
    using namespace ores::logging;

    layout_ = new QVBoxLayout(this);
    layout_->setContentsMargins(0, 0, 0, 0);
    layout_->setSpacing(0);

    // Create toolbar with icons following icon-guidelines
    toolBar_ = new QToolBar(this);
    toolBar_->setIconSize(QSize(20, 20));

    const QColor iconColor(220, 220, 220); // Light gray for dark theme

    createFolderAction_ = toolBar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_folder_20_regular.svg", iconColor),
        tr("New Folder"));
    createFolderAction_->setToolTip(tr("Create a new folder"));

    createConnectionAction_ = toolBar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_add_20_regular.svg", iconColor),
        tr("New Connection"));
    createConnectionAction_->setToolTip(tr("Add a new server connection"));

    toolBar_->addSeparator();

    editAction_ = toolBar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_edit_20_regular.svg", iconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected item"));

    deleteAction_ = toolBar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_delete_20_regular.svg", iconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected item"));

    toolBar_->addSeparator();

    connectAction_ = toolBar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_plug_connected_20_regular.svg", iconColor),
        tr("Connect"));
    connectAction_->setToolTip(tr("Connect to selected server"));

    toolBar_->addSeparator();

    refreshAction_ = toolBar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_arrow_clockwise_16_regular.svg", iconColor),
        tr("Refresh"));
    refreshAction_->setToolTip(tr("Refresh connection list"));

    toolBar_->addSeparator();

    changeMasterPasswordAction_ = toolBar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_key_multiple_20_regular.svg", iconColor),
        tr("Change Password"));
    changeMasterPasswordAction_->setToolTip(tr("Change master password"));

    purgeDatabaseAction_ = toolBar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_delete_dismiss_20_regular.svg", iconColor),
        tr("Purge"));
    purgeDatabaseAction_->setToolTip(tr("Delete all connections and reset database"));

    layout_->addWidget(toolBar_);

    // Create tree view
    treeView_ = new QTreeView(this);
    treeView_->setContextMenuPolicy(Qt::CustomContextMenu);
    treeView_->setSelectionMode(QAbstractItemView::SingleSelection);
    treeView_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    treeView_->setExpandsOnDoubleClick(false); // We handle double-click ourselves
    treeView_->setUniformRowHeights(true);
    treeView_->setIconSize(QSize(20, 20)); // Larger icons for better visibility

    // Create model and set on view
    model_ = std::make_unique<ConnectionTreeModel>(manager_, this);
    treeView_->setModel(model_.get());
    treeView_->expandAll();

    // Configure as a simple tree (hide detail columns)
    treeView_->setHeaderHidden(true);
    treeView_->header()->setStretchLastSection(true);
    treeView_->setColumnHidden(1, true); // Hide Host
    treeView_->setColumnHidden(2, true); // Hide Port
    treeView_->setColumnHidden(3, true); // Hide Username

    layout_->addWidget(treeView_);

    // Connect signals
    connect(createFolderAction_, &QAction::triggered,
            this, &ConnectionBrowserMdiWindow::createFolder);
    connect(createConnectionAction_, &QAction::triggered,
            this, &ConnectionBrowserMdiWindow::createConnection);
    connect(editAction_, &QAction::triggered,
            this, &ConnectionBrowserMdiWindow::editSelected);
    connect(deleteAction_, &QAction::triggered,
            this, &ConnectionBrowserMdiWindow::deleteSelected);
    connect(connectAction_, &QAction::triggered,
            this, &ConnectionBrowserMdiWindow::connectToSelected);
    connect(refreshAction_, &QAction::triggered,
            this, &ConnectionBrowserMdiWindow::reload);
    connect(changeMasterPasswordAction_, &QAction::triggered,
            this, &ConnectionBrowserMdiWindow::changeMasterPassword);
    connect(purgeDatabaseAction_, &QAction::triggered,
            this, &ConnectionBrowserMdiWindow::purgeDatabase);

    connect(treeView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &ConnectionBrowserMdiWindow::onSelectionChanged);
    connect(treeView_, &QTreeView::doubleClicked,
            this, &ConnectionBrowserMdiWindow::onDoubleClicked);
    connect(treeView_, &QTreeView::customContextMenuRequested,
            this, &ConnectionBrowserMdiWindow::showContextMenu);

    connect(model_.get(), &ConnectionTreeModel::errorOccurred,
            this, &ConnectionBrowserMdiWindow::errorOccurred);

    BOOST_LOG_SEV(lg(), info) << "Connection browser window initialized";
}

void ConnectionBrowserMdiWindow::updateActionStates() {
    QModelIndex current = treeView_->currentIndex();
    bool hasSelection = current.isValid();

    auto* node = model_->nodeFromIndex(current);
    bool isEnvironment = node && node->type == ConnectionTreeNode::Type::Environment;
    bool isFolder = node && node->type == ConnectionTreeNode::Type::Folder;

    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    connectAction_->setEnabled(isEnvironment);

    // Create folder action can specify parent if folder is selected
    createFolderAction_->setEnabled(true);
    createConnectionAction_->setEnabled(true);
}

void ConnectionBrowserMdiWindow::reload() {
    using namespace ores::logging;
    BOOST_LOG_SEV(lg(), debug) << "Reloading connection browser";

    model_->refresh();
    treeView_->expandAll();
    emit statusChanged(tr("Connections reloaded"));
}

void ConnectionBrowserMdiWindow::createFolder() {
    using namespace ores::logging;

    // Get current selection as potential parent
    std::optional<boost::uuids::uuid> parentId;
    QModelIndex current = treeView_->currentIndex();
    if (current.isValid()) {
        auto* node = model_->nodeFromIndex(current);
        if (node && node->type == ConnectionTreeNode::Type::Folder) {
            parentId = node->id;
        }
    }

    FolderDetailDialog dialog(manager_, this);
    dialog.setInitialParent(parentId);

    if (dialog.exec() == QDialog::Accepted) {
        try {
            auto folder = dialog.getFolder();
            manager_->create_folder(folder);
            model_->refresh();
            treeView_->expandAll();
            emit statusChanged(tr("Folder created: %1").arg(
                QString::fromStdString(folder.name)));
            BOOST_LOG_SEV(lg(), info) << "Created folder: " << folder.name;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to create folder: " << e.what();
            emit errorOccurred(tr("Failed to create folder: %1").arg(e.what()));
        }
    }
}

void ConnectionBrowserMdiWindow::createConnection() {
    using namespace ores::logging;

    // Get current selection as potential folder
    std::optional<boost::uuids::uuid> folderId;
    QModelIndex current = treeView_->currentIndex();
    if (current.isValid()) {
        auto* node = model_->nodeFromIndex(current);
        if (node) {
            if (node->type == ConnectionTreeNode::Type::Folder) {
                folderId = node->id;
            } else if (node->type == ConnectionTreeNode::Type::Environment && node->parent_id) {
                folderId = node->parent_id;
            }
        }
    }

    ConnectionDetailDialog dialog(manager_, this);
    dialog.setInitialFolder(folderId);

    if (dialog.exec() == QDialog::Accepted) {
        try {
            auto env = dialog.getEnvironment();
            auto password = dialog.getPassword();
            manager_->create_environment(env, password.value_or(""));
            model_->refresh();
            treeView_->expandAll();
            emit statusChanged(tr("Connection created: %1").arg(
                QString::fromStdString(env.name)));
            BOOST_LOG_SEV(lg(), info) << "Created connection: " << env.name;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to create connection: " << e.what();
            emit errorOccurred(tr("Failed to create connection: %1").arg(e.what()));
        }
    }
}

void ConnectionBrowserMdiWindow::editSelected() {
    using namespace ores::logging;

    QModelIndex current = treeView_->currentIndex();
    if (!current.isValid())
        return;

    auto* node = model_->nodeFromIndex(current);
    if (!node)
        return;

    if (node->type == ConnectionTreeNode::Type::Folder) {
        auto folder = model_->getFolderFromIndex(current);
        if (!folder) {
            emit errorOccurred(tr("Failed to load folder details"));
            return;
        }

        FolderDetailDialog dialog(manager_, this);
        dialog.setFolder(*folder);

        if (dialog.exec() == QDialog::Accepted) {
            try {
                auto updated = dialog.getFolder();
                manager_->update_folder(updated);
                model_->refresh();
                treeView_->expandAll();
                emit statusChanged(tr("Folder updated: %1").arg(
                    QString::fromStdString(updated.name)));
                BOOST_LOG_SEV(lg(), info) << "Updated folder: " << updated.name;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error) << "Failed to update folder: " << e.what();
                emit errorOccurred(tr("Failed to update folder: %1").arg(e.what()));
            }
        }
    } else if (node->type == ConnectionTreeNode::Type::Environment) {
        auto env = model_->getEnvironmentFromIndex(current);
        if (!env) {
            emit errorOccurred(tr("Failed to load connection details"));
            return;
        }

        ConnectionDetailDialog dialog(manager_, this);
        dialog.setEnvironment(*env);

        if (dialog.exec() == QDialog::Accepted) {
            try {
                auto updated = dialog.getEnvironment();
                auto password = dialog.getPassword();
                manager_->update_environment(updated, password);
                model_->refresh();
                treeView_->expandAll();
                emit statusChanged(tr("Connection updated: %1").arg(
                    QString::fromStdString(updated.name)));
                BOOST_LOG_SEV(lg(), info) << "Updated connection: " << updated.name;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error) << "Failed to update connection: " << e.what();
                emit errorOccurred(tr("Failed to update connection: %1").arg(e.what()));
            }
        }
    }
}

void ConnectionBrowserMdiWindow::deleteSelected() {
    using namespace ores::logging;

    QModelIndex current = treeView_->currentIndex();
    if (!current.isValid())
        return;

    auto* node = model_->nodeFromIndex(current);
    if (!node)
        return;

    QString itemName = node->name;
    QString itemType = (node->type == ConnectionTreeNode::Type::Folder) ? tr("folder") : tr("connection");

    QString message;
    if (node->type == ConnectionTreeNode::Type::Folder) {
        message = tr("Are you sure you want to delete the folder '%1'?\n\n"
                     "All connections in this folder will be moved to the root level.")
                      .arg(itemName);
    } else {
        message = tr("Are you sure you want to delete the connection '%1'?")
                      .arg(itemName);
    }

    auto result = MessageBoxHelper::question(this,
        tr("Confirm Delete"),
        message,
        QMessageBox::Yes | QMessageBox::No);

    if (result != QMessageBox::Yes) {
        return;
    }

    try {
        if (node->type == ConnectionTreeNode::Type::Folder) {
            manager_->delete_folder(node->id);
            BOOST_LOG_SEV(lg(), info) << "Deleted folder: " << node->name.toStdString();
        } else {
            manager_->delete_environment(node->id);
            BOOST_LOG_SEV(lg(), info) << "Deleted connection: " << node->name.toStdString();
        }

        model_->refresh();
        treeView_->expandAll();
        emit statusChanged(tr("%1 deleted: %2").arg(itemType, itemName));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to delete " << itemType.toStdString()
                                   << ": " << e.what();
        emit errorOccurred(tr("Failed to delete %1: %2").arg(itemType, e.what()));
    }
}

void ConnectionBrowserMdiWindow::connectToSelected() {
    using namespace ores::logging;

    QModelIndex current = treeView_->currentIndex();
    if (!current.isValid())
        return;

    auto* node = model_->nodeFromIndex(current);
    if (!node || node->type != ConnectionTreeNode::Type::Environment)
        return;

    BOOST_LOG_SEV(lg(), info) << "Connect requested for: " << node->name.toStdString();
    emit connectRequested(node->id, node->name);
}

void ConnectionBrowserMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void ConnectionBrowserMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    auto* node = model_->nodeFromIndex(index);
    if (!node)
        return;

    if (node->type == ConnectionTreeNode::Type::Environment) {
        // Double-click on environment triggers connect
        connectToSelected();
    } else if (node->type == ConnectionTreeNode::Type::Folder) {
        // Double-click on folder toggles expansion
        if (treeView_->isExpanded(index))
            treeView_->collapse(index);
        else
            treeView_->expand(index);
    }
}

void ConnectionBrowserMdiWindow::showContextMenu(const QPoint& pos) {
    QModelIndex index = treeView_->indexAt(pos);

    QMenu menu(this);

    // Always show create actions
    menu.addAction(createFolderAction_);
    menu.addAction(createConnectionAction_);

    if (index.isValid()) {
        menu.addSeparator();
        menu.addAction(editAction_);
        menu.addAction(deleteAction_);

        auto* node = model_->nodeFromIndex(index);
        if (node && node->type == ConnectionTreeNode::Type::Environment) {
            menu.addSeparator();
            menu.addAction(connectAction_);
        }
    }

    menu.addSeparator();
    menu.addAction(refreshAction_);

    menu.exec(treeView_->viewport()->mapToGlobal(pos));
}

void ConnectionBrowserMdiWindow::changeMasterPassword() {
    using namespace ores::logging;
    BOOST_LOG_SEV(lg(), debug) << "Change master password requested";
    emit changeMasterPasswordRequested();
}

void ConnectionBrowserMdiWindow::purgeDatabase() {
    using namespace ores::logging;

    auto result = MessageBoxHelper::question(this,
        tr("Purge Database"),
        tr("This will permanently delete ALL saved connections and folders.\n\n"
           "This action cannot be undone. Are you sure?"),
        QMessageBox::Yes | QMessageBox::No);

    if (result != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), warn) << "Purging connections database";

    try {
        manager_->purge();
        model_->refresh();
        emit statusChanged(tr("Database purged - all connections deleted"));
        emit databasePurged();

        BOOST_LOG_SEV(lg(), info) << "Database purged successfully";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to purge database: " << e.what();
        emit errorOccurred(tr("Failed to purge database: %1").arg(e.what()));
    }
}

}
