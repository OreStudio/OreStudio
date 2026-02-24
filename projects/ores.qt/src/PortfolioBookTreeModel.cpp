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
#include "ores.qt/PortfolioBookTreeModel.hpp"

#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"

namespace ores::qt {

using namespace ores::logging;

PortfolioBookTreeModel::PortfolioBookTreeModel(QObject* parent)
    : QAbstractItemModel(parent) {}

void PortfolioBookTreeModel::load(
    std::vector<refdata::domain::portfolio> portfolios,
    std::vector<refdata::domain::book> books) {

    beginResetModel();
    roots_.clear();

    // Build tree starting from root portfolios (no parent).
    // Virtual portfolios are included as structural parent nodes; they are
    // distinguished visually by the Briefcase (outline) icon vs BriefcaseFilled
    // for real portfolios. Selecting a virtual portfolio in the tree triggers
    // the recursive-CTE trade filter which traverses the full subtree.
    build_subtree(nullptr, portfolios, books, std::nullopt);

    endResetModel();
    BOOST_LOG_SEV(lg(), debug) << "Tree loaded: " << roots_.size()
                               << " root nodes.";
}

void PortfolioBookTreeModel::build_subtree(
    PortfolioTreeNode* parent_node,
    const std::vector<refdata::domain::portfolio>& portfolios,
    const std::vector<refdata::domain::book>& books,
    const std::optional<boost::uuids::uuid>& parent_id) {

    auto& container = parent_node ? parent_node->children : roots_;
    int row = 0;

    for (const auto& p : portfolios) {
        // Match this portfolio to the given parent_id
        const bool is_root = !parent_id.has_value() &&
                             !p.parent_portfolio_id.has_value();
        const bool is_child = parent_id.has_value() &&
                              p.parent_portfolio_id.has_value() &&
                              *p.parent_portfolio_id == *parent_id;

        if (!is_root && !is_child)
            continue;

        auto node = std::make_unique<PortfolioTreeNode>();
        node->kind = PortfolioTreeNode::Kind::Portfolio;
        node->portfolio = p;
        node->parent = parent_node;
        node->row_in_parent = row++;

        // Recurse: add child portfolios
        build_subtree(node.get(), portfolios, books,
            std::optional<boost::uuids::uuid>{p.id});

        // Add books under this portfolio
        for (const auto& b : books) {
            if (b.parent_portfolio_id == p.id) {
                auto book_node = std::make_unique<PortfolioTreeNode>();
                book_node->kind = PortfolioTreeNode::Kind::Book;
                book_node->book = b;
                book_node->parent = node.get();
                book_node->row_in_parent =
                    static_cast<int>(node->children.size());
                node->children.push_back(std::move(book_node));
            }
        }

        container.push_back(std::move(node));
    }
}

TreeNodeFilter
PortfolioBookTreeModel::selected_filter(const QModelIndex& index) const {
    const auto* node = node_from_index(index);
    if (!node)
        return {};

    if (node->kind == PortfolioTreeNode::Kind::Book)
        return {.book_id = node->book.id, .portfolio_id = std::nullopt};

    return {.book_id = std::nullopt, .portfolio_id = node->portfolio.id};
}

PortfolioTreeNode*
PortfolioBookTreeModel::node_from_index(const QModelIndex& index) const {
    if (!index.isValid())
        return nullptr;
    return static_cast<PortfolioTreeNode*>(index.internalPointer());
}

QModelIndex PortfolioBookTreeModel::index(
    int row, int col, const QModelIndex& parent) const {
    if (row < 0 || col != 0)
        return {};

    if (!parent.isValid()) {
        if (row >= static_cast<int>(roots_.size()))
            return {};
        return createIndex(row, col, roots_[row].get());
    }

    const auto* parent_node = node_from_index(parent);
    if (!parent_node || row >= static_cast<int>(parent_node->children.size()))
        return {};

    return createIndex(row, col, parent_node->children[row].get());
}

QModelIndex PortfolioBookTreeModel::parent(const QModelIndex& index) const {
    const auto* node = node_from_index(index);
    if (!node || !node->parent)
        return {};

    const auto* grandparent = node->parent->parent;
    if (!grandparent) {
        // parent is a root node
        return createIndex(node->parent->row_in_parent, 0, node->parent);
    }
    return createIndex(node->parent->row_in_parent, 0, node->parent);
}

int PortfolioBookTreeModel::rowCount(const QModelIndex& parent) const {
    if (!parent.isValid())
        return static_cast<int>(roots_.size());

    const auto* node = node_from_index(parent);
    if (!node)
        return 0;
    return static_cast<int>(node->children.size());
}

int PortfolioBookTreeModel::columnCount(const QModelIndex& /*parent*/) const {
    return 1;
}

QVariant PortfolioBookTreeModel::data(
    const QModelIndex& index, int role) const {
    const auto* node = node_from_index(index);
    if (!node)
        return {};

    if (role == Qt::DisplayRole) {
        if (node->kind == PortfolioTreeNode::Kind::Portfolio)
            return QString::fromStdString(node->portfolio.name);
        return QString::fromStdString(node->book.name);
    }

    if (role == Qt::DecorationRole) {
        if (node->kind == PortfolioTreeNode::Kind::Portfolio) {
            // Virtual portfolios use outline icon; real portfolios use filled icon
            const auto icon = node->portfolio.is_virtual == 1
                ? Icon::Briefcase : Icon::BriefcaseFilled;
            return IconUtils::createRecoloredIcon(icon, IconUtils::DefaultIconColor);
        }
        // Book
        if (node->book.is_trading_book == 1) {
            return IconUtils::createRecoloredIcon(
                Icon::BookOpenFilled, IconUtils::DefaultIconColor);
        }
        return IconUtils::createRecoloredIcon(
            Icon::BookOpen, IconUtils::DefaultIconColor);
    }

    if (role == Qt::UserRole) {
        return static_cast<int>(node->kind);
    }

    if (role == Qt::UserRole + 1) {
        if (node->kind == PortfolioTreeNode::Kind::Portfolio)
            return QString::fromStdString(boost::uuids::to_string(node->portfolio.id));
        return QString::fromStdString(boost::uuids::to_string(node->book.id));
    }

    return {};
}

}
