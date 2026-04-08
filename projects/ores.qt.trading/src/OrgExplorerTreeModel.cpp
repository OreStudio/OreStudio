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
#include "ores.qt/OrgExplorerTreeModel.hpp"

#include <functional>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"

namespace ores::qt {

using namespace ores::logging;

OrgExplorerTreeModel::OrgExplorerTreeModel(QObject* parent)
    : QAbstractItemModel(parent) {}

void OrgExplorerTreeModel::load(
    const QString& party_name,
    std::vector<refdata::domain::business_unit> units,
    std::vector<refdata::domain::book> books) {

    beginResetModel();
    trade_counts_.clear();

    root_ = std::make_unique<OrgTreeNode>();
    root_->kind = OrgTreeNode::Kind::Party;
    root_->party_name = party_name;
    root_->parent = nullptr;
    root_->row_in_parent = 0;

    // Build unit subtree recursively, starting with top-level units
    build_unit_subtree(root_.get(), units, books, std::nullopt);

    // Collect books with no owner unit into an "Unassigned" synthetic node
    std::vector<refdata::domain::book> unassigned;
    for (const auto& b : books) {
        if (!b.owner_unit_id.has_value())
            unassigned.push_back(b);
    }

    if (!unassigned.empty()) {
        auto unassigned_node = std::make_unique<OrgTreeNode>();
        unassigned_node->kind = OrgTreeNode::Kind::BusinessUnit;
        unassigned_node->unit.unit_name = "(Unassigned)";
        unassigned_node->parent = root_.get();
        unassigned_node->row_in_parent = static_cast<int>(root_->children.size());

        for (const auto& b : unassigned) {
            auto book_node = std::make_unique<OrgTreeNode>();
            book_node->kind = OrgTreeNode::Kind::Book;
            book_node->book = b;
            book_node->parent = unassigned_node.get();
            book_node->row_in_parent =
                static_cast<int>(unassigned_node->children.size());
            unassigned_node->children.push_back(std::move(book_node));
        }

        root_->children.push_back(std::move(unassigned_node));
    }

    endResetModel();

    BOOST_LOG_SEV(lg(), debug) << "Org tree loaded with party root: "
                               << party_name.toStdString()
                               << ", " << root_->children.size()
                               << " top-level unit nodes.";
}

void OrgExplorerTreeModel::build_unit_subtree(
    OrgTreeNode* parent_node,
    const std::vector<refdata::domain::business_unit>& units,
    const std::vector<refdata::domain::book>& books,
    const std::optional<boost::uuids::uuid>& parent_unit_id) {

    if (!parent_node)
        return;

    int row = static_cast<int>(parent_node->children.size());

    for (const auto& u : units) {
        // Match this unit to the given parent_unit_id
        const bool is_root = !parent_unit_id.has_value() &&
                             !u.parent_business_unit_id.has_value();
        const bool is_child = parent_unit_id.has_value() &&
                              u.parent_business_unit_id.has_value() &&
                              *u.parent_business_unit_id == *parent_unit_id;

        if (!is_root && !is_child)
            continue;

        auto node = std::make_unique<OrgTreeNode>();
        node->kind = OrgTreeNode::Kind::BusinessUnit;
        node->unit = u;
        node->parent = parent_node;
        node->row_in_parent = row++;

        // Recurse: add child units
        build_unit_subtree(node.get(), units, books,
            std::optional<boost::uuids::uuid>{u.id});

        // Add books owned by this unit
        for (const auto& b : books) {
            if (b.owner_unit_id.has_value() && *b.owner_unit_id == u.id) {
                auto book_node = std::make_unique<OrgTreeNode>();
                book_node->kind = OrgTreeNode::Kind::Book;
                book_node->book = b;
                book_node->parent = node.get();
                book_node->row_in_parent =
                    static_cast<int>(node->children.size());
                node->children.push_back(std::move(book_node));
            }
        }

        parent_node->children.push_back(std::move(node));
    }
}

OrgTreeNodeFilter OrgExplorerTreeModel::selected_filter(
    const QModelIndex& index) const {
    const auto* node = node_from_index(index);
    if (!node)
        return {};

    if (node->kind == OrgTreeNode::Kind::Party)
        return {.book_id = std::nullopt, .business_unit_id = std::nullopt};

    if (node->kind == OrgTreeNode::Kind::Book)
        return {.book_id = node->book.id, .business_unit_id = std::nullopt};

    // BusinessUnit: the synthetic "(Unassigned)" node has a nil UUID because it
    // has no corresponding database row.  Passing a nil business_unit_id to the
    // backend produces zero results, which is inconsistent with the trade counts
    // shown on the node.  Fall back to "no filter" so all trades are visible,
    // matching the behaviour of clicking the Party root.
    if (node->unit.id.is_nil())
        return {.book_id = std::nullopt, .business_unit_id = std::nullopt};

    return {.book_id = std::nullopt, .business_unit_id = node->unit.id};
}

std::uint32_t OrgExplorerTreeModel::subtree_count(
    const OrgTreeNode* node) const {
    if (!node)
        return 0;
    if (node->kind == OrgTreeNode::Kind::Book) {
        const auto it = trade_counts_.find(
            boost::uuids::to_string(node->book.id));
        return it != trade_counts_.end() ? it->second : 0;
    }
    std::uint32_t total = 0;
    for (const auto& child : node->children)
        total += subtree_count(child.get());
    return total;
}

void OrgExplorerTreeModel::set_trade_count(
    const boost::uuids::uuid& book_id, std::uint32_t count) {
    trade_counts_[boost::uuids::to_string(book_id)] = count;
    auto idx = find_book_index(book_id);
    if (!idx.isValid())
        return;
    // Notify the book itself and every ancestor up to (and including) the root
    while (idx.isValid()) {
        emit dataChanged(idx, idx, {Qt::DisplayRole});
        idx = idx.parent();
    }
}

QModelIndex OrgExplorerTreeModel::find_book_index(
    const boost::uuids::uuid& id) const {
    std::function<QModelIndex(const QModelIndex&)> search =
        [&](const QModelIndex& parent) -> QModelIndex {
        for (int r = 0; r < rowCount(parent); ++r) {
            auto idx = index(r, 0, parent);
            const auto* node = node_from_index(idx);
            if (node && node->kind == OrgTreeNode::Kind::Book
                    && node->book.id == id)
                return idx;
            if (node && !node->children.empty()) {
                auto found = search(idx);
                if (found.isValid())
                    return found;
            }
        }
        return {};
    };
    return search({});
}

OrgTreeNode* OrgExplorerTreeModel::node_from_index(
    const QModelIndex& index) const {
    if (!index.isValid())
        return nullptr;
    return static_cast<OrgTreeNode*>(index.internalPointer());
}

QModelIndex OrgExplorerTreeModel::index(
    int row, int col, const QModelIndex& parent) const {
    if (row < 0 || col != 0)
        return {};

    if (!parent.isValid()) {
        // Top-level: only the party root node
        if (!root_ || row != 0)
            return {};
        return createIndex(0, 0, root_.get());
    }

    const auto* parent_node = node_from_index(parent);
    if (!parent_node || row >= static_cast<int>(parent_node->children.size()))
        return {};

    return createIndex(row, col, parent_node->children[row].get());
}

QModelIndex OrgExplorerTreeModel::parent(const QModelIndex& index) const {
    const auto* node = node_from_index(index);
    if (!node || !node->parent)
        return {};

    return createIndex(node->parent->row_in_parent, 0, node->parent);
}

int OrgExplorerTreeModel::rowCount(const QModelIndex& parent) const {
    if (!parent.isValid())
        return root_ ? 1 : 0;

    const auto* node = node_from_index(parent);
    if (!node)
        return 0;
    return static_cast<int>(node->children.size());
}

int OrgExplorerTreeModel::columnCount(const QModelIndex& /*parent*/) const {
    return 1;
}

QVariant OrgExplorerTreeModel::data(
    const QModelIndex& index, int role) const {
    const auto* node = node_from_index(index);
    if (!node)
        return {};

    if (role == Qt::DisplayRole) {
        auto append_count = [](QString name, std::uint32_t n) -> QString {
            if (n > 0)
                name += QStringLiteral(" (%1)").arg(n);
            return name;
        };
        if (node->kind == OrgTreeNode::Kind::Party)
            return append_count(node->party_name, subtree_count(node));
        if (node->kind == OrgTreeNode::Kind::BusinessUnit)
            return append_count(
                QString::fromStdString(node->unit.unit_name),
                subtree_count(node));
        // Book: look up count directly
        const auto it = trade_counts_.find(
            boost::uuids::to_string(node->book.id));
        const auto n = it != trade_counts_.end() ? it->second : 0;
        return append_count(QString::fromStdString(node->book.name), n);
    }

    if (role == Qt::DecorationRole) {
        if (node->kind == OrgTreeNode::Kind::Party)
            return IconUtils::createRecoloredIcon(
                Icon::Organization, IconUtils::DefaultIconColor);
        if (node->kind == OrgTreeNode::Kind::BusinessUnit)
            return IconUtils::createRecoloredIcon(
                Icon::PeopleTeam, IconUtils::DefaultIconColor);
        // Book
        if (node->book.is_trading_book == 1)
            return IconUtils::createRecoloredIcon(
                Icon::BookOpenFilled, IconUtils::DefaultIconColor);
        return IconUtils::createRecoloredIcon(
            Icon::BookOpen, IconUtils::DefaultIconColor);
    }

    if (role == Qt::UserRole)
        return static_cast<int>(node->kind);

    if (role == Qt::UserRole + 1) {
        if (node->kind == OrgTreeNode::Kind::Party)
            return node->party_name;
        if (node->kind == OrgTreeNode::Kind::BusinessUnit)
            return QString::fromStdString(
                boost::uuids::to_string(node->unit.id));
        return QString::fromStdString(
            boost::uuids::to_string(node->book.id));
    }

    return {};
}

}
