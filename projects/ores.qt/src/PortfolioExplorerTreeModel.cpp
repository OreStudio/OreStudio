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
#include "ores.qt/PortfolioExplorerTreeModel.hpp"

#include <functional>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"

namespace ores::qt {

using namespace ores::logging;

PortfolioExplorerTreeModel::PortfolioExplorerTreeModel(QObject* parent)
    : QAbstractItemModel(parent) {}

void PortfolioExplorerTreeModel::load(
    const QString& party_name,
    std::vector<refdata::domain::portfolio> portfolios,
    std::vector<refdata::domain::book> books) {

    beginResetModel();
    trade_counts_.clear();

    root_ = std::make_unique<PortfolioTreeNode>();
    root_->kind = PortfolioTreeNode::Kind::Party;
    root_->party_name = party_name;
    root_->parent = nullptr;
    root_->row_in_parent = 0;

    build_subtree(root_.get(), portfolios, books, std::nullopt);

    endResetModel();
    BOOST_LOG_SEV(lg(), debug) << "Tree loaded with party root: "
                               << party_name.toStdString()
                               << ", " << root_->children.size()
                               << " top-level portfolio nodes.";
}

void PortfolioExplorerTreeModel::build_subtree(
    PortfolioTreeNode* parent_node,
    const std::vector<refdata::domain::portfolio>& portfolios,
    const std::vector<refdata::domain::book>& books,
    const std::optional<boost::uuids::uuid>& parent_id) {

    if (!parent_node)
        return;

    auto& container = parent_node->children;
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
PortfolioExplorerTreeModel::selected_filter(const QModelIndex& index) const {
    const auto* node = node_from_index(index);
    if (!node)
        return {};

    if (node->kind == PortfolioTreeNode::Kind::Party)
        return {.book_id = std::nullopt, .portfolio_id = std::nullopt};

    if (node->kind == PortfolioTreeNode::Kind::Book)
        return {.book_id = node->book.id, .portfolio_id = std::nullopt};

    return {.book_id = std::nullopt, .portfolio_id = node->portfolio.id};
}

std::uint32_t PortfolioExplorerTreeModel::subtree_count(
    const PortfolioTreeNode* node) const {
    if (!node)
        return 0;
    if (node->kind == PortfolioTreeNode::Kind::Book) {
        const auto it = trade_counts_.find(
            boost::uuids::to_string(node->book.id));
        return it != trade_counts_.end() ? it->second : 0;
    }
    std::uint32_t total = 0;
    for (const auto& child : node->children)
        total += subtree_count(child.get());
    return total;
}

void PortfolioExplorerTreeModel::set_trade_count(
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

QModelIndex PortfolioExplorerTreeModel::find_book_index(
    const boost::uuids::uuid& id) const {
    std::function<QModelIndex(const QModelIndex&)> search =
        [&](const QModelIndex& parent) -> QModelIndex {
        for (int r = 0; r < rowCount(parent); ++r) {
            auto idx = index(r, 0, parent);
            const auto* node = node_from_index(idx);
            if (node && node->kind == PortfolioTreeNode::Kind::Book
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

PortfolioTreeNode*
PortfolioExplorerTreeModel::node_from_index(const QModelIndex& index) const {
    if (!index.isValid())
        return nullptr;
    return static_cast<PortfolioTreeNode*>(index.internalPointer());
}

QModelIndex PortfolioExplorerTreeModel::index(
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

QModelIndex PortfolioExplorerTreeModel::parent(const QModelIndex& index) const {
    const auto* node = node_from_index(index);
    if (!node || !node->parent)
        return {};

    return createIndex(node->parent->row_in_parent, 0, node->parent);
}

int PortfolioExplorerTreeModel::rowCount(const QModelIndex& parent) const {
    if (!parent.isValid())
        return root_ ? 1 : 0;

    const auto* node = node_from_index(parent);
    if (!node)
        return 0;
    return static_cast<int>(node->children.size());
}

int PortfolioExplorerTreeModel::columnCount(const QModelIndex& /*parent*/) const {
    return 1;
}

QVariant PortfolioExplorerTreeModel::data(
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
        if (node->kind == PortfolioTreeNode::Kind::Party)
            return append_count(node->party_name, subtree_count(node));
        if (node->kind == PortfolioTreeNode::Kind::Portfolio)
            return append_count(
                QString::fromStdString(node->portfolio.name),
                subtree_count(node));
        // Book: look up count directly
        const auto it = trade_counts_.find(
            boost::uuids::to_string(node->book.id));
        const auto n = it != trade_counts_.end() ? it->second : 0;
        return append_count(QString::fromStdString(node->book.name), n);
    }

    if (role == Qt::DecorationRole) {
        if (node->kind == PortfolioTreeNode::Kind::Party)
            return IconUtils::createRecoloredIcon(
                Icon::Organization, IconUtils::DefaultIconColor);
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
        if (node->kind == PortfolioTreeNode::Kind::Party)
            return node->party_name;
        if (node->kind == PortfolioTreeNode::Kind::Portfolio)
            return QString::fromStdString(boost::uuids::to_string(node->portfolio.id));
        return QString::fromStdString(boost::uuids::to_string(node->book.id));
    }

    return {};
}

}
