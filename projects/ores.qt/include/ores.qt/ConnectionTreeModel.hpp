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
#ifndef ORES_QT_CONNECTION_TREE_MODEL_HPP
#define ORES_QT_CONNECTION_TREE_MODEL_HPP

#include <QAbstractItemModel>
#include <QIcon>
#include <memory>
#include <vector>
#include <optional>
#include <unordered_set>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_hash.hpp>
#include "ores.connections/domain/folder.hpp"
#include "ores.connections/domain/server_environment.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::connections::service {
class connection_manager;
}

namespace ores::qt {

/**
 * @brief Tree node representing either a folder or server environment.
 */
struct ConnectionTreeNode {
    enum class Type { Root, Folder, Environment };

    Type type{Type::Root};
    boost::uuids::uuid id{};
    QString name;
    std::optional<boost::uuids::uuid> parent_id;

    // Environment-specific data
    QString host;
    int port{0};
    QString username;
    QString description;

    // Tree structure
    ConnectionTreeNode* parent{nullptr};
    std::vector<std::unique_ptr<ConnectionTreeNode>> children;

    ConnectionTreeNode() = default;
    ~ConnectionTreeNode() = default;

    ConnectionTreeNode(const ConnectionTreeNode&) = delete;
    ConnectionTreeNode& operator=(const ConnectionTreeNode&) = delete;
    ConnectionTreeNode(ConnectionTreeNode&&) = default;
    ConnectionTreeNode& operator=(ConnectionTreeNode&&) = default;

    int row() const;
};

/**
 * @brief Tree model for displaying folders and server environments.
 *
 * Provides a hierarchical view of connection folders and environments:
 * - Root folders at top level
 * - Child folders nested under parents
 * - Server environments within folders or at root
 */
class ConnectionTreeModel : public QAbstractItemModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.connection_tree_model";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum Column {
        Name = 0,
        Host,
        Port,
        Username,
        ColumnCount
    };

    enum Role {
        NodeTypeRole = Qt::UserRole + 1,
        UuidRole,
        IsEnvironmentRole,
        IsFolderRole
    };

    explicit ConnectionTreeModel(
        connections::service::connection_manager* manager,
        QObject* parent = nullptr);
    ~ConnectionTreeModel() override;

    // QAbstractItemModel interface
    QModelIndex index(int row, int column,
        const QModelIndex& parent = QModelIndex()) const override;
    QModelIndex parent(const QModelIndex& index) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;
    Qt::ItemFlags flags(const QModelIndex& index) const override;
    bool setData(const QModelIndex& index, const QVariant& value,
        int role = Qt::EditRole) override;

    // Data access
    ConnectionTreeNode* nodeFromIndex(const QModelIndex& index) const;
    QModelIndex indexFromUuid(const boost::uuids::uuid& id) const;

    // Data operations
    void refresh();

    // Expansion state (for folder icons)
    void setFolderExpanded(const QModelIndex& index, bool expanded);

    // Get domain objects from selection
    std::optional<connections::domain::folder> getFolderFromIndex(
        const QModelIndex& index) const;
    std::optional<connections::domain::server_environment> getEnvironmentFromIndex(
        const QModelIndex& index) const;

signals:
    void dataRefreshed();
    void errorOccurred(const QString& message);

private:
    void buildTree();
    void buildFolderNodes(ConnectionTreeNode* parentNode,
        const std::optional<boost::uuids::uuid>& parentId);
    void buildEnvironmentNodes(ConnectionTreeNode* parentNode,
        const std::optional<boost::uuids::uuid>& folderId);
    QModelIndex indexFromNode(ConnectionTreeNode* node, int column = 0) const;
    QModelIndex findNodeIndex(ConnectionTreeNode* searchNode,
        const boost::uuids::uuid& id) const;

    connections::service::connection_manager* manager_;
    std::unique_ptr<ConnectionTreeNode> rootNode_;
    QIcon folderIcon_;
    QIcon folderOpenIcon_;
    QIcon serverIcon_;
    std::unordered_set<boost::uuids::uuid> expandedFolders_;
};

}

#endif
