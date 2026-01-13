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
#include <boost/uuid/uuid_io.hpp>

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
    const QColor iconColor(220, 220, 220);
    folderIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_folder_20_regular.svg", iconColor);
    folderOpenIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_folder_open_20_regular.svg", iconColor);
    serverIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_server_link_20_regular.svg", iconColor);

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
            if (node->type == ConnectionTreeNode::Type::Folder)
                return folderIcon_;
            else if (node->type == ConnectionTreeNode::Type::Environment)
                return serverIcon_;
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
        return Qt::NoItemFlags;

    return Qt::ItemIsEnabled | Qt::ItemIsSelectable;
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
    buildTree();
    endResetModel();
    emit dataRefreshed();
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
