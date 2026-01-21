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
#include "ores.qt/ConnectionTreeModel.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.connections/service/connection_manager.hpp"
#include <QMimeData>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/string_generator.hpp>

namespace ores::qt {

int ConnectionTreeNode::row() const {
    if (!parent)
        return 0;

    for (size_t i = 0; i < parent->children.size(); ++i) {
        if (parent->children[i].get() == this)
            return static_cast<int>(i);
    }
    return 0;
}

ConnectionTreeModel::ConnectionTreeModel(
    connections::service::connection_manager* manager,
    QObject* parent)
    : QAbstractItemModel(parent),
      manager_(manager),
      rootNode_(std::make_unique<ConnectionTreeNode>()) {

    // Use bright icons for visibility in dark theme
    folderIcon_ = IconUtils::createRecoloredIcon(
        Icon::Folder, IconUtils::DefaultIconColor);
    folderOpenIcon_ = IconUtils::createRecoloredIcon(
        Icon::FolderOpen, IconUtils::DefaultIconColor);
    serverIcon_ = IconUtils::createRecoloredIcon(
        Icon::ServerLink, IconUtils::DefaultIconColor);

    rootNode_->type = ConnectionTreeNode::Type::Root;
    rootNode_->name = "Root";

    buildTree();
}

ConnectionTreeModel::~ConnectionTreeModel() = default;

QModelIndex ConnectionTreeModel::index(int row, int column,
    const QModelIndex& parent) const {

    if (!hasIndex(row, column, parent))
        return QModelIndex();

    ConnectionTreeNode* parentNode;
    if (!parent.isValid())
        parentNode = rootNode_.get();
    else
        parentNode = static_cast<ConnectionTreeNode*>(parent.internalPointer());

    if (row >= 0 && static_cast<size_t>(row) < parentNode->children.size()) {
        ConnectionTreeNode* childNode = parentNode->children[row].get();
        return createIndex(row, column, childNode);
    }

    return QModelIndex();
}

QModelIndex ConnectionTreeModel::parent(const QModelIndex& index) const {
    if (!index.isValid())
        return QModelIndex();

    auto* childNode = static_cast<ConnectionTreeNode*>(index.internalPointer());
    ConnectionTreeNode* parentNode = childNode->parent;

    if (!parentNode || parentNode == rootNode_.get())
        return QModelIndex();

    return createIndex(parentNode->row(), 0, parentNode);
}

int ConnectionTreeModel::rowCount(const QModelIndex& parent) const {
    ConnectionTreeNode* parentNode;
    if (!parent.isValid())
        parentNode = rootNode_.get();
    else
        parentNode = static_cast<ConnectionTreeNode*>(parent.internalPointer());

    return static_cast<int>(parentNode->children.size());
}

int ConnectionTreeModel::columnCount(const QModelIndex& /*parent*/) const {
    return ColumnCount;
}

QVariant ConnectionTreeModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return QVariant();

    auto* node = static_cast<ConnectionTreeNode*>(index.internalPointer());

    switch (role) {
    case Qt::DisplayRole:
        switch (index.column()) {
        case Name:
            return node->name;
        case Host:
            if (node->type == ConnectionTreeNode::Type::Environment)
                return node->host;
            return QVariant();
        case Port:
            if (node->type == ConnectionTreeNode::Type::Environment && node->port > 0)
                return node->port;
            return QVariant();
        case Username:
            if (node->type == ConnectionTreeNode::Type::Environment)
                return node->username;
            return QVariant();
        default:
            return QVariant();
        }

    case Qt::DecorationRole:
        if (index.column() == Name) {
            if (node->type == ConnectionTreeNode::Type::Folder) {
                bool isExpanded = expandedFolders_.contains(node->id);
                return isExpanded ? folderOpenIcon_ : folderIcon_;
            } else if (node->type == ConnectionTreeNode::Type::Environment) {
                return serverIcon_;
            }
        }
        return QVariant();

    case Qt::ToolTipRole:
        if (node->type == ConnectionTreeNode::Type::Environment) {
            QString tooltip = QString("%1\n%2:%3")
                .arg(node->name)
                .arg(node->host)
                .arg(node->port);
            if (!node->description.isEmpty())
                tooltip += QString("\n\n%1").arg(node->description);
            return tooltip;
        }
        return node->name;

    case NodeTypeRole:
        return static_cast<int>(node->type);

    case UuidRole:
        return QString::fromStdString(boost::uuids::to_string(node->id));

    case IsEnvironmentRole:
        return node->type == ConnectionTreeNode::Type::Environment;

    case IsFolderRole:
        return node->type == ConnectionTreeNode::Type::Folder;

    case TagsRole:
        if (node->type == ConnectionTreeNode::Type::Environment) {
            try {
                auto tags = manager_->get_tags_for_environment(node->id);
                QStringList tagNames;
                for (const auto& tag : tags) {
                    tagNames.append(QString::fromStdString(tag.name));
                }
                return tagNames;
            } catch (const std::exception& e) {
                using namespace ores::logging;
                BOOST_LOG_SEV(lg(), error) << "Failed to get tags for environment: " << e.what();
                return QStringList();
            }
        }
        return QStringList();

    default:
        return QVariant();
    }
}

QVariant ConnectionTreeModel::headerData(int section, Qt::Orientation orientation,
    int role) const {

    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return QVariant();

    switch (section) {
    case Name:
        return tr("Name");
    case Host:
        return tr("Host");
    case Port:
        return tr("Port");
    case Username:
        return tr("Username");
    default:
        return QVariant();
    }
}

Qt::ItemFlags ConnectionTreeModel::flags(const QModelIndex& index) const {
    if (!index.isValid())
        return Qt::ItemIsDropEnabled; // Allow dropping on empty area (root)

    Qt::ItemFlags flags = Qt::ItemIsEnabled | Qt::ItemIsSelectable;

    auto* node = nodeFromIndex(index);
    if (node && node->type != ConnectionTreeNode::Type::Root) {
        // Items can be dragged
        flags |= Qt::ItemIsDragEnabled;

        // Folders can accept drops
        if (node->type == ConnectionTreeNode::Type::Folder) {
            flags |= Qt::ItemIsDropEnabled;
        }

        // Only the Name column is editable (for inline rename)
        if (index.column() == Name) {
            flags |= Qt::ItemIsEditable;
        }
    }

    return flags;
}

bool ConnectionTreeModel::setData(const QModelIndex& index, const QVariant& value,
    int role) {

    if (!index.isValid() || role != Qt::EditRole || index.column() != Name)
        return false;

    auto* node = nodeFromIndex(index);
    if (!node || node->type == ConnectionTreeNode::Type::Root)
        return false;

    QString newName = value.toString().trimmed();
    if (newName.isEmpty() || newName == node->name)
        return false;

    using namespace ores::logging;

    try {
        if (node->type == ConnectionTreeNode::Type::Folder) {
            auto folder = manager_->get_folder(node->id);
            if (folder) {
                folder->name = newName.toStdString();
                manager_->update_folder(*folder);
                node->name = newName;
                BOOST_LOG_SEV(lg(), info) << "Renamed folder to: " << newName.toStdString();
            }
        } else if (node->type == ConnectionTreeNode::Type::Environment) {
            auto env = manager_->get_environment(node->id);
            if (env) {
                env->name = newName.toStdString();
                manager_->update_environment(*env, std::nullopt);
                node->name = newName;
                BOOST_LOG_SEV(lg(), info) << "Renamed connection to: " << newName.toStdString();
            }
        }

        emit dataChanged(index, index, {Qt::DisplayRole, Qt::EditRole});
        return true;

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to rename: " << e.what();
        emit errorOccurred(QString("Failed to rename: %1").arg(e.what()));
        return false;
    }
}

Qt::DropActions ConnectionTreeModel::supportedDropActions() const {
    return Qt::MoveAction;
}

QStringList ConnectionTreeModel::mimeTypes() const {
    return {"application/x-ores-connection-item"};
}

QMimeData* ConnectionTreeModel::mimeData(const QModelIndexList& indexes) const {
    if (indexes.isEmpty())
        return nullptr;

    // Use first index (single selection mode)
    const QModelIndex& index = indexes.first();
    auto* node = nodeFromIndex(index);
    if (!node || node->type == ConnectionTreeNode::Type::Root)
        return nullptr;

    auto* mimeData = new QMimeData();
    QString data = QString("%1:%2")
        .arg(node->type == ConnectionTreeNode::Type::Folder ? "folder" : "env")
        .arg(QString::fromStdString(boost::uuids::to_string(node->id)));
    mimeData->setData("application/x-ores-connection-item", data.toUtf8());
    return mimeData;
}

bool ConnectionTreeModel::canDropMimeData(const QMimeData* data, Qt::DropAction action,
    int /*row*/, int /*column*/, const QModelIndex& parent) const {

    if (!data || action != Qt::MoveAction)
        return false;

    if (!data->hasFormat("application/x-ores-connection-item"))
        return false;

    // Parse dragged item
    QString itemData = QString::fromUtf8(data->data("application/x-ores-connection-item"));
    QStringList parts = itemData.split(':');
    if (parts.size() != 2)
        return false;

    QString itemType = parts[0];
    QString itemUuidStr = parts[1];

    boost::uuids::string_generator gen;
    boost::uuids::uuid draggedId;
    try {
        draggedId = gen(itemUuidStr.toStdString());
    } catch (const std::exception& e) {
        using namespace ores::logging;
        BOOST_LOG_SEV(lg(), error) << "Failed to parse UUID from drag data: " << e.what();
        return false;
    }

    // Get target folder (if dropping on a folder)
    std::optional<boost::uuids::uuid> targetFolderId;
    if (parent.isValid()) {
        auto* targetNode = nodeFromIndex(parent);
        if (!targetNode || targetNode->type != ConnectionTreeNode::Type::Folder)
            return false;
        targetFolderId = targetNode->id;

        // Prevent dropping a folder onto itself or its descendants
        if (itemType == "folder") {
            auto* checkNode = targetNode;
            while (checkNode) {
                if (checkNode->id == draggedId)
                    return false;
                checkNode = checkNode->parent;
            }
        }
    }

    return true;
}

bool ConnectionTreeModel::dropMimeData(const QMimeData* data, Qt::DropAction action,
    int /*row*/, int /*column*/, const QModelIndex& parent) {

    if (!canDropMimeData(data, action, 0, 0, parent))
        return false;

    using namespace ores::logging;

    QString itemData = QString::fromUtf8(data->data("application/x-ores-connection-item"));
    QStringList parts = itemData.split(':');
    QString itemType = parts[0];
    QString itemUuidStr = parts[1];

    boost::uuids::string_generator gen;
    boost::uuids::uuid itemId = gen(itemUuidStr.toStdString());

    // Determine target folder
    std::optional<boost::uuids::uuid> targetFolderId;
    if (parent.isValid()) {
        auto* targetNode = nodeFromIndex(parent);
        if (targetNode && targetNode->type == ConnectionTreeNode::Type::Folder) {
            targetFolderId = targetNode->id;
        }
    }

    try {
        if (itemType == "folder") {
            auto folder = manager_->get_folder(itemId);
            if (folder) {
                folder->parent_id = targetFolderId;
                manager_->update_folder(*folder);
                BOOST_LOG_SEV(lg(), info) << "Moved folder '"
                    << folder->name << "' to "
                    << (targetFolderId ? boost::uuids::to_string(*targetFolderId) : "root");
            }
        } else {
            auto env = manager_->get_environment(itemId);
            if (env) {
                env->folder_id = targetFolderId;
                manager_->update_environment(*env, std::nullopt);
                BOOST_LOG_SEV(lg(), info) << "Moved connection '"
                    << env->name << "' to "
                    << (targetFolderId ? boost::uuids::to_string(*targetFolderId) : "root");
            }
        }

        refresh();
        return true;

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to move item: " << e.what();
        emit errorOccurred(QString("Failed to move item: %1").arg(e.what()));
        return false;
    }
}

ConnectionTreeNode* ConnectionTreeModel::nodeFromIndex(const QModelIndex& index) const {
    if (!index.isValid())
        return nullptr;

    return static_cast<ConnectionTreeNode*>(index.internalPointer());
}

QModelIndex ConnectionTreeModel::indexFromNode(ConnectionTreeNode* node, int column) const {
    if (!node || node == rootNode_.get())
        return QModelIndex();

    return createIndex(node->row(), column, node);
}

QModelIndex ConnectionTreeModel::indexFromUuid(const boost::uuids::uuid& id) const {
    return findNodeIndex(rootNode_.get(), id);
}

QModelIndex ConnectionTreeModel::findNodeIndex(ConnectionTreeNode* searchNode,
    const boost::uuids::uuid& id) const {

    for (const auto& child : searchNode->children) {
        if (child->id == id)
            return indexFromNode(child.get());

        QModelIndex found = findNodeIndex(child.get(), id);
        if (found.isValid())
            return found;
    }
    return QModelIndex();
}

void ConnectionTreeModel::refresh() {
    beginResetModel();
    rootNode_->children.clear();
    expandedFolders_.clear();
    buildTree();
    endResetModel();
    emit dataRefreshed();
}

void ConnectionTreeModel::setFolderExpanded(const QModelIndex& index, bool expanded) {
    auto* node = nodeFromIndex(index);
    if (!node || node->type != ConnectionTreeNode::Type::Folder)
        return;

    if (expanded)
        expandedFolders_.insert(node->id);
    else
        expandedFolders_.erase(node->id);

    // Notify the view that the decoration has changed
    emit dataChanged(index, index, {Qt::DecorationRole});
}

void ConnectionTreeModel::buildTree() {
    using namespace ores::logging;

    try {
        // Build folders first (they can contain other folders and environments)
        buildFolderNodes(rootNode_.get(), std::nullopt);

        // Build root-level environments (no folder)
        buildEnvironmentNodes(rootNode_.get(), std::nullopt);

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to build tree: " << e.what();
        emit errorOccurred(QString("Failed to load connections: %1").arg(e.what()));
    }
}

void ConnectionTreeModel::buildFolderNodes(ConnectionTreeNode* parentNode,
    const std::optional<boost::uuids::uuid>& parentId) {

    using namespace ores::logging;

    std::vector<connections::domain::folder> folders;
    if (parentId)
        folders = manager_->get_child_folders(*parentId);
    else
        folders = manager_->get_root_folders();

    BOOST_LOG_SEV(lg(), debug) << "Building " << folders.size()
                               << " folder nodes for parent: "
                               << (parentId ? boost::uuids::to_string(*parentId) : "root");

    for (const auto& folder : folders) {
        auto node = std::make_unique<ConnectionTreeNode>();
        node->type = ConnectionTreeNode::Type::Folder;
        node->id = folder.id;
        node->name = QString::fromStdString(folder.name);
        node->parent_id = folder.parent_id;
        node->parent = parentNode;

        ConnectionTreeNode* folderNode = node.get();
        parentNode->children.push_back(std::move(node));

        // Recursively build child folders
        buildFolderNodes(folderNode, folder.id);

        // Build environments in this folder
        buildEnvironmentNodes(folderNode, folder.id);
    }
}

void ConnectionTreeModel::buildEnvironmentNodes(ConnectionTreeNode* parentNode,
    const std::optional<boost::uuids::uuid>& folderId) {

    using namespace ores::logging;

    auto environments = manager_->get_environments_in_folder(folderId);

    BOOST_LOG_SEV(lg(), debug) << "Building " << environments.size()
                               << " environment nodes for folder: "
                               << (folderId ? boost::uuids::to_string(*folderId) : "root");

    for (const auto& env : environments) {
        auto node = std::make_unique<ConnectionTreeNode>();
        node->type = ConnectionTreeNode::Type::Environment;
        node->id = env.id;
        node->name = QString::fromStdString(env.name);
        node->parent_id = env.folder_id;
        node->host = QString::fromStdString(env.host);
        node->port = env.port;
        node->username = QString::fromStdString(env.username);
        node->description = QString::fromStdString(env.description);
        node->parent = parentNode;

        parentNode->children.push_back(std::move(node));
    }
}

std::optional<connections::domain::folder> ConnectionTreeModel::getFolderFromIndex(
    const QModelIndex& index) const {

    auto* node = nodeFromIndex(index);
    if (!node || node->type != ConnectionTreeNode::Type::Folder)
        return std::nullopt;

    return manager_->get_folder(node->id);
}

std::optional<connections::domain::server_environment> ConnectionTreeModel::getEnvironmentFromIndex(
    const QModelIndex& index) const {

    auto* node = nodeFromIndex(index);
    if (!node || node->type != ConnectionTreeNode::Type::Environment)
        return std::nullopt;

    return manager_->get_environment(node->id);
}

}
