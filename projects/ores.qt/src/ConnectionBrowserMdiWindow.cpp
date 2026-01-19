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
#include "ores.qt/AddItemDialog.hpp"
#include "ores.qt/ConnectionDetailPanel.hpp"
#include "ores.qt/ConnectionItemDelegate.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ColorConstants.hpp"
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
      manager_(manager),
      mdiArea_(nullptr),
      mainWindow_(nullptr),
      allDetachableWindows_(nullptr) {

    setupUI();
    updateActionStates();
}

ConnectionBrowserMdiWindow::~ConnectionBrowserMdiWindow() = default;

QSize ConnectionBrowserMdiWindow::sizeHint() const {
    return {800, 500};
}

void ConnectionBrowserMdiWindow::setTestCallback(TestConnectionCallback callback) {
    testCallback_ = std::move(callback);
}

void ConnectionBrowserMdiWindow::setMdiArea(QMdiArea* mdiArea, QMainWindow* mainWindow,
                                             QList<DetachableMdiSubWindow*>* allDetachableWindows) {
    mdiArea_ = mdiArea;
    mainWindow_ = mainWindow;
    allDetachableWindows_ = allDetachableWindows;
}

void ConnectionBrowserMdiWindow::setupUI() {
    using namespace ores::logging;

    layout_ = new QVBoxLayout(this);
    layout_->setContentsMargins(0, 0, 0, 0);
    layout_->setSpacing(0);

    // Create toolbar with icons following icon-guidelines
    toolBar_ = new QToolBar(this);
    toolBar_->setIconSize(QSize(20, 20));

    addAction_ = toolBar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor),
        tr("Add"));
    addAction_->setToolTip(tr("Add a new folder or connection"));

    toolBar_->addSeparator();

    editAction_ = toolBar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Edit, IconUtils::DefaultIconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected item"));

    deleteAction_ = toolBar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected item"));

    toolBar_->addSeparator();

    connectAction_ = toolBar_->addAction(
        IconUtils::createRecoloredIcon(Icon::PlugConnected, IconUtils::DefaultIconColor),
        tr("Connect"));
    connectAction_->setToolTip(tr("Connect to selected server"));

    toolBar_->addSeparator();

    refreshAction_ = toolBar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Refresh"));
    refreshAction_->setToolTip(tr("Refresh connection list"));

    toolBar_->addSeparator();

    changeMasterPasswordAction_ = toolBar_->addAction(
        IconUtils::createRecoloredIcon(Icon::KeyMultiple, IconUtils::DefaultIconColor),
        tr("Change"));
    changeMasterPasswordAction_->setToolTip(tr("Change master password"));

    purgeDatabaseAction_ = toolBar_->addAction(
        IconUtils::createRecoloredIcon(Icon::DeleteDismiss, IconUtils::DefaultIconColor),
        tr("Purge"));
    purgeDatabaseAction_->setToolTip(tr("Delete all connections and reset database"));

    layout_->addWidget(toolBar_);

    // Create splitter for tree and detail panel
    splitter_ = new QSplitter(Qt::Horizontal, this);

    // Create tree view
    treeView_ = new QTreeView(this);
    treeView_->setContextMenuPolicy(Qt::CustomContextMenu);
    treeView_->setSelectionMode(QAbstractItemView::SingleSelection);
    // Enable inline rename via F2 or slow double-click on selected item
    treeView_->setEditTriggers(QAbstractItemView::EditKeyPressed |
                                QAbstractItemView::SelectedClicked);
    treeView_->setExpandsOnDoubleClick(false); // We handle double-click ourselves
    treeView_->setUniformRowHeights(true);
    treeView_->setIconSize(QSize(20, 20)); // Larger icons for better visibility

    // Enable drag and drop for reorganizing folders and connections
    treeView_->setDragEnabled(true);
    treeView_->setAcceptDrops(true);
    treeView_->setDropIndicatorShown(true);
    treeView_->setDragDropMode(QAbstractItemView::InternalMove);

    // Create model and set on view
    model_ = std::make_unique<ConnectionTreeModel>(manager_, this);
    treeView_->setModel(model_.get());

    // Set custom delegate for rendering tag badges
    treeView_->setItemDelegate(new ConnectionItemDelegate(this));

    // Configure as a simple tree (hide detail columns)
    treeView_->setHeaderHidden(true);
    treeView_->header()->setStretchLastSection(true);
    treeView_->setColumnHidden(1, true); // Hide Host
    treeView_->setColumnHidden(2, true); // Hide Port
    treeView_->setColumnHidden(3, true); // Hide Username

    splitter_->addWidget(treeView_);

    // Create detail panel
    detailPanel_ = new ConnectionDetailPanel(manager_, this);
    splitter_->addWidget(detailPanel_);

    splitter_->setSizes({330, 270});
    splitter_->setStretchFactor(0, 0);
    splitter_->setStretchFactor(1, 1);

    layout_->addWidget(splitter_);

    // Connect signals
    connect(addAction_, &QAction::triggered,
            this, &ConnectionBrowserMdiWindow::openAddDialog);
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
    connect(treeView_, &QTreeView::expanded,
            this, [this](const QModelIndex& index) {
                model_->setFolderExpanded(index, true);
            });
    connect(treeView_, &QTreeView::collapsed,
            this, [this](const QModelIndex& index) {
                model_->setFolderExpanded(index, false);
            });

    connect(model_.get(), &ConnectionTreeModel::errorOccurred,
            this, &ConnectionBrowserMdiWindow::errorOccurred);

    // Start with tree expanded
    treeView_->expandAll();

    BOOST_LOG_SEV(lg(), info) << "Connection browser window initialized";
}

void ConnectionBrowserMdiWindow::updateActionStates() {
    QModelIndex current = treeView_->currentIndex();
    bool hasSelection = current.isValid();

    auto* node = model_->nodeFromIndex(current);
    bool isEnvironment = node && node->type == ConnectionTreeNode::Type::Environment;

    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    connectAction_->setEnabled(isEnvironment);
    addAction_->setEnabled(true);
}

void ConnectionBrowserMdiWindow::updateDetailPanel() {
    QModelIndex current = treeView_->currentIndex();

    if (!current.isValid()) {
        detailPanel_->showEmptyState();
        return;
    }

    auto* node = model_->nodeFromIndex(current);
    if (!node) {
        detailPanel_->showEmptyState();
        return;
    }

    if (node->type == ConnectionTreeNode::Type::Folder) {
        auto folder = model_->getFolderFromIndex(current);
        if (folder) {
            // Count items in this folder
            int itemCount = static_cast<int>(node->children.size());
            detailPanel_->showFolder(*folder, itemCount);
        } else {
            detailPanel_->showEmptyState();
        }
    } else if (node->type == ConnectionTreeNode::Type::Environment) {
        auto env = model_->getEnvironmentFromIndex(current);
        if (env) {
            detailPanel_->showEnvironment(*env);
        } else {
            detailPanel_->showEmptyState();
        }
    } else {
        detailPanel_->showEmptyState();
    }
}

void ConnectionBrowserMdiWindow::reload() {
    using namespace ores::logging;
    BOOST_LOG_SEV(lg(), debug) << "Reloading connection browser";

    model_->refresh();
    treeView_->expandAll();
    emit statusChanged(tr("Connections reloaded"));
}

void ConnectionBrowserMdiWindow::openAddDialog() {
    using namespace ores::logging;

    // Get current selection to determine initial folder
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

    auto* dialog = new AddItemDialog(manager_, nullptr);
    dialog->setCreateMode(true);
    dialog->setInitialFolder(folderId);
    dialog->setInitialParent(folderId);
    if (testCallback_) {
        dialog->setTestCallback(testCallback_);
    }

    // Connect signals for refresh
    connect(dialog, &AddItemDialog::folderSaved, this, [this](const boost::uuids::uuid&, const QString& name) {
        model_->refresh();
        treeView_->expandAll();
        emit statusChanged(tr("Folder created: %1").arg(name));
    });
    connect(dialog, &AddItemDialog::connectionSaved, this, [this](const boost::uuids::uuid&, const QString& name) {
        model_->refresh();
        treeView_->expandAll();
        emit statusChanged(tr("Connection created: %1").arg(name));
    });
    connect(dialog, &AddItemDialog::statusMessage, this, &ConnectionBrowserMdiWindow::statusChanged);
    connect(dialog, &AddItemDialog::errorMessage, this, &ConnectionBrowserMdiWindow::errorOccurred);

    // Create as MDI sub-window if MDI area is available
    if (mdiArea_) {
        auto* subWindow = new DetachableMdiSubWindow(mainWindow_);
        subWindow->setWidget(dialog);
        subWindow->setWindowTitle(tr("Add Item"));
        subWindow->setWindowIcon(IconUtils::createRecoloredIcon(
            Icon::Add, IconUtils::DefaultIconColor));
        subWindow->setAttribute(Qt::WA_DeleteOnClose);
        subWindow->resize(450, 500);

        mdiArea_->addSubWindow(subWindow);
        subWindow->show();

        if (allDetachableWindows_) {
            allDetachableWindows_->append(subWindow);
            connect(subWindow, &QObject::destroyed, this, [this, subWindow]() {
                if (allDetachableWindows_) {
                    allDetachableWindows_->removeOne(subWindow);
                }
            });
        }
    } else {
        // Fallback to regular window
        dialog->setWindowFlag(Qt::Window);
        dialog->setAttribute(Qt::WA_DeleteOnClose);
        dialog->setWindowTitle(tr("Add Item"));
        dialog->resize(450, 500);
        dialog->show();
        dialog->raise();
        dialog->activateWindow();
    }

    BOOST_LOG_SEV(lg(), debug) << "Opened add dialog";
}

void ConnectionBrowserMdiWindow::editSelected() {
    using namespace ores::logging;

    QModelIndex current = treeView_->currentIndex();
    if (!current.isValid())
        return;

    auto* node = model_->nodeFromIndex(current);
    if (!node)
        return;

    auto* dialog = new AddItemDialog(manager_, nullptr);
    dialog->setCreateMode(false);

    QString windowTitle;
    Icon iconType;

    if (node->type == ConnectionTreeNode::Type::Folder) {
        auto folder = model_->getFolderFromIndex(current);
        if (!folder) {
            emit errorOccurred(tr("Failed to load folder details"));
            delete dialog;
            return;
        }

        dialog->setFolder(*folder);
        windowTitle = tr("Edit Folder: %1").arg(QString::fromStdString(folder->name));
        iconType = Icon::Folder;

        connect(dialog, &AddItemDialog::folderSaved, this, [this](const boost::uuids::uuid&, const QString& name) {
            model_->refresh();
            treeView_->expandAll();
            emit statusChanged(tr("Folder updated: %1").arg(name));
        });
    } else if (node->type == ConnectionTreeNode::Type::Environment) {
        auto env = model_->getEnvironmentFromIndex(current);
        if (!env) {
            emit errorOccurred(tr("Failed to load connection details"));
            delete dialog;
            return;
        }

        dialog->setEnvironment(*env);
        windowTitle = tr("Edit Connection: %1").arg(QString::fromStdString(env->name));
        iconType = Icon::Server;
        if (testCallback_) {
            dialog->setTestCallback(testCallback_);
        }

        // Load existing tags
        try {
            auto tags = manager_->get_tags_for_environment(node->id);
            dialog->setTags(tags);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to load tags for environment: " << e.what();
        }

        connect(dialog, &AddItemDialog::connectionSaved, this, [this](const boost::uuids::uuid&, const QString& name) {
            model_->refresh();
            treeView_->expandAll();
            emit statusChanged(tr("Connection updated: %1").arg(name));
        });
    } else {
        delete dialog;
        return;
    }

    connect(dialog, &AddItemDialog::statusMessage, this, &ConnectionBrowserMdiWindow::statusChanged);
    connect(dialog, &AddItemDialog::errorMessage, this, &ConnectionBrowserMdiWindow::errorOccurred);

    // Create as MDI sub-window if MDI area is available
    if (mdiArea_) {
        auto* subWindow = new DetachableMdiSubWindow(mainWindow_);
        subWindow->setWidget(dialog);
        subWindow->setWindowTitle(windowTitle);
        subWindow->setWindowIcon(IconUtils::createRecoloredIcon(iconType, IconUtils::DefaultIconColor));
        subWindow->setAttribute(Qt::WA_DeleteOnClose);
        subWindow->resize(450, 500);

        mdiArea_->addSubWindow(subWindow);
        subWindow->show();

        if (allDetachableWindows_) {
            allDetachableWindows_->append(subWindow);
            connect(subWindow, &QObject::destroyed, this, [this, subWindow]() {
                if (allDetachableWindows_) {
                    allDetachableWindows_->removeOne(subWindow);
                }
            });
        }
    } else {
        // Fallback to regular window
        dialog->setWindowFlag(Qt::Window);
        dialog->setAttribute(Qt::WA_DeleteOnClose);
        dialog->setWindowTitle(windowTitle);
        dialog->resize(450, 500);
        dialog->show();
        dialog->raise();
        dialog->activateWindow();
    }

    BOOST_LOG_SEV(lg(), debug) << "Opened edit dialog";
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
    updateDetailPanel();
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

    // Always show add action
    menu.addAction(addAction_);

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
