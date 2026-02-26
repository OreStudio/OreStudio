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
#ifndef ORES_QT_ORG_EXPLORER_TREE_MODEL_HPP
#define ORES_QT_ORG_EXPLORER_TREE_MODEL_HPP

#include <memory>
#include <vector>
#include <optional>
#include <unordered_map>
#include <QString>
#include <QAbstractItemModel>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.refdata/domain/business_unit.hpp"
#include "ores.refdata/domain/book.hpp"

namespace ores::qt {

/**
 * @brief A single node in the organisational hierarchy tree.
 *
 * Nodes are owned by the model. Raw pointers are stored in
 * QModelIndex::internalPointer().
 *
 * The tree represents: Party → BusinessUnit (recursive) → Book
 */
struct OrgTreeNode {
    enum class Kind { Party, BusinessUnit, Book };

    Kind kind;
    QString party_name;                            // valid when kind == Party
    refdata::domain::business_unit unit;           // valid when kind == BusinessUnit
    refdata::domain::book book;                    // valid when kind == Book
    OrgTreeNode* parent = nullptr;
    std::vector<std::unique_ptr<OrgTreeNode>> children;
    int row_in_parent = 0;
};

/**
 * @brief Tree model for the organisational hierarchy.
 *
 * Displays: Party root → BusinessUnit nodes (recursive via parent_business_unit_id)
 * → Book leaves (via owner_unit_id).
 *
 * Books with no owner_unit_id are placed under an "Unassigned" synthetic node
 * at the party level.
 */
class OrgExplorerTreeModel final : public QAbstractItemModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.org_explorer_tree_model";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit OrgExplorerTreeModel(QObject* parent = nullptr);
    ~OrgExplorerTreeModel() override = default;

    /**
     * @brief Rebuild the tree from raw business unit and book data.
     *
     * Creates a single party root node named party_name, then adds business
     * units recursively and books as leaves under their owning unit.
     * Books with no owner_unit_id are grouped under an "Unassigned" node.
     */
    void load(const QString& party_name,
              std::vector<refdata::domain::business_unit> units,
              std::vector<refdata::domain::book> books);

    /**
     * @brief Returns the node for a given index, or nullptr if invalid.
     */
    OrgTreeNode* node_from_index(const QModelIndex& index) const;

    // QAbstractItemModel interface
    QModelIndex index(int row, int col,
        const QModelIndex& parent = QModelIndex()) const override;
    QModelIndex parent(const QModelIndex& index) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;

private:
    void build_unit_subtree(OrgTreeNode* parent_node,
        const std::vector<refdata::domain::business_unit>& units,
        const std::vector<refdata::domain::book>& books,
        const std::optional<boost::uuids::uuid>& parent_unit_id);

    std::unique_ptr<OrgTreeNode> root_;
};

}

#endif
