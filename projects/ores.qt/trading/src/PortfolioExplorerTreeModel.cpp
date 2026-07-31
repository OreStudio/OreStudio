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
#include "ores.qt/IconUtils.hpp"
#include "ores.refdata.api/domain/regulatory_book_type_constants.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <functional>

namespace ores::qt {

using namespace ores::logging;

PortfolioExplorerTreeModel::PortfolioExplorerTreeModel(QObject* parent)
    : QAbstractItemModel(parent) {}

void PortfolioExplorerTreeModel::load(const std::vector<refdata::domain::party>& parties,
                                      std::vector<refdata::domain::portfolio> portfolios,
                                      std::vector<refdata::domain::book> books) {

    beginResetModel();
    trade_counts_.clear();

    // Invisible container: its children are the per-party root nodes actually
    // shown at the top level (see index()/parent()/rowCount() for the
    // invalid-parent special-casing this implies).
    root_ = std::make_unique<PortfolioTreeNode>();
    root_->parent = nullptr;

    std::unordered_map<std::string, refdata::domain::party> party_by_id;
    for (const auto& p : parties)
        party_by_id[boost::uuids::to_string(p.id)] = p;

    std::unordered_map<std::string, std::vector<refdata::domain::portfolio>> portfolios_by_party;
    for (const auto& p : portfolios)
        portfolios_by_party[boost::uuids::to_string(p.party_id)].push_back(p);

    std::unordered_map<std::string, std::vector<refdata::domain::book>> books_by_party;
    for (const auto& b : books)
        books_by_party[boost::uuids::to_string(b.party_id)].push_back(b);

    // The working set of party ids to show: every party with visible
    // portfolios/books, plus every party fetched outright (so an empty
    // holding company still appears as the shell containing its child
    // parties). A portfolio/book whose party isn't in `parties` (a
    // visibility gap) still gets a bare fallback node rather than silently
    // disappearing.
    std::vector<std::string> party_ids;
    auto ensure_party = [&](const std::string& party_id) {
        if (std::find(party_ids.begin(), party_ids.end(), party_id) == party_ids.end())
            party_ids.push_back(party_id);
    };
    for (const auto& [pid, _] : party_by_id)
        ensure_party(pid);
    for (const auto& [pid, _] : portfolios_by_party)
        ensure_party(pid);
    for (const auto& [pid, _] : books_by_party)
        ensure_party(pid);

    auto name_of = [&](const std::string& pid) {
        const auto it = party_by_id.find(pid);
        return it != party_by_id.end() ? QString::fromStdString(it->second.full_name) :
                                         QString::fromStdString(pid);
    };
    // A party's effective parent is its parent_party_id, but only when that
    // parent is itself part of the working set -- otherwise (parent outside
    // session visibility, or no parent at all) the party becomes a top-level
    // root.
    auto effective_parent_of = [&](const std::string& pid) -> std::optional<std::string> {
        const auto it = party_by_id.find(pid);
        if (it == party_by_id.end() || !it->second.parent_party_id.has_value())
            return std::nullopt;
        const auto parent_str = boost::uuids::to_string(*it->second.parent_party_id);
        if (std::find(party_ids.begin(), party_ids.end(), parent_str) == party_ids.end())
            return std::nullopt;
        return parent_str;
    };

    std::function<void(PortfolioTreeNode*, const std::optional<std::string>&)> build_parties =
        [&](PortfolioTreeNode* parent_node, const std::optional<std::string>& parent_id) {
            std::vector<std::string> children;
            for (const auto& pid : party_ids) {
                if (effective_parent_of(pid) == parent_id)
                    children.push_back(pid);
            }
            std::sort(children.begin(), children.end(), [&](const auto& a, const auto& b) {
                return name_of(a) < name_of(b);
            });

            for (const auto& pid : children) {
                auto party_node = std::make_unique<PortfolioTreeNode>();
                party_node->kind = PortfolioTreeNode::Kind::Party;
                party_node->party_name = name_of(pid);
                party_node->parent = parent_node;
                party_node->row_in_parent = static_cast<int>(parent_node->children.size());

                const auto portfolios_it = portfolios_by_party.find(pid);
                const auto books_it = books_by_party.find(pid);
                static const std::vector<refdata::domain::portfolio> no_portfolios;
                static const std::vector<refdata::domain::book> no_books;
                const auto& party_portfolios = portfolios_it != portfolios_by_party.end() ?
                                                   portfolios_it->second :
                                                   no_portfolios;
                const auto& party_books =
                    books_it != books_by_party.end() ? books_it->second : no_books;

                build_subtree(party_node.get(), party_portfolios, party_books, std::nullopt);

                // Recurse into this party's own child parties before moving
                // party_node into its parent's children (build_parties needs
                // the still-owning raw pointer).
                build_parties(party_node.get(), pid);

                parent_node->children.push_back(std::move(party_node));
            }
        };

    build_parties(root_.get(), std::nullopt);

    endResetModel();
    BOOST_LOG_SEV(lg(), debug) << "Tree loaded with " << root_->children.size()
                               << " top-level party node(s).";
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
        const bool is_root = !parent_id.has_value() && !p.parent_portfolio_id.has_value();
        const bool is_child = parent_id.has_value() && p.parent_portfolio_id.has_value() &&
                              *p.parent_portfolio_id == *parent_id;

        if (!is_root && !is_child)
            continue;

        auto node = std::make_unique<PortfolioTreeNode>();
        node->kind = PortfolioTreeNode::Kind::Portfolio;
        node->portfolio = p;
        node->parent = parent_node;
        node->row_in_parent = row++;

        // Recurse: add child portfolios
        build_subtree(node.get(), portfolios, books, std::optional<boost::uuids::uuid>{p.id});

        // Add books under this portfolio
        for (const auto& b : books) {
            if (b.parent_portfolio_id == p.id) {
                auto book_node = std::make_unique<PortfolioTreeNode>();
                book_node->kind = PortfolioTreeNode::Kind::Book;
                book_node->book = b;
                book_node->parent = node.get();
                book_node->row_in_parent = static_cast<int>(node->children.size());
                node->children.push_back(std::move(book_node));
            }
        }

        container.push_back(std::move(node));
    }
}

TreeNodeFilter PortfolioExplorerTreeModel::selected_filter(const QModelIndex& index) const {
    const auto* node = node_from_index(index);
    if (!node)
        return {};

    if (node->kind == PortfolioTreeNode::Kind::Party)
        return {.book_id = std::nullopt, .portfolio_id = std::nullopt};

    if (node->kind == PortfolioTreeNode::Kind::Book)
        return {.book_id = node->book.id, .portfolio_id = std::nullopt};

    return {.book_id = std::nullopt, .portfolio_id = node->portfolio.id};
}

std::uint32_t PortfolioExplorerTreeModel::subtree_count(const PortfolioTreeNode* node) const {
    if (!node)
        return 0;
    if (node->kind == PortfolioTreeNode::Kind::Book) {
        const auto it = trade_counts_.find(boost::uuids::to_string(node->book.id));
        return it != trade_counts_.end() ? it->second : 0;
    }
    std::uint32_t total = 0;
    for (const auto& child : node->children)
        total += subtree_count(child.get());
    return total;
}

void PortfolioExplorerTreeModel::set_trade_count(const boost::uuids::uuid& book_id,
                                                 std::uint32_t count) {
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

QModelIndex PortfolioExplorerTreeModel::find_book_index(const boost::uuids::uuid& id) const {
    std::function<QModelIndex(const QModelIndex&)> search =
        [&](const QModelIndex& parent) -> QModelIndex {
        for (int r = 0; r < rowCount(parent); ++r) {
            auto idx = index(r, 0, parent);
            const auto* node = node_from_index(idx);
            if (node && node->kind == PortfolioTreeNode::Kind::Book && node->book.id == id)
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

PortfolioTreeNode* PortfolioExplorerTreeModel::node_from_index(const QModelIndex& index) const {
    if (!index.isValid())
        return nullptr;
    return static_cast<PortfolioTreeNode*>(index.internalPointer());
}

QModelIndex PortfolioExplorerTreeModel::index(int row, int col, const QModelIndex& parent) const {
    if (row < 0 || col != 0)
        return {};

    if (!parent.isValid()) {
        // Top-level: one row per party root node
        if (!root_ || row >= static_cast<int>(root_->children.size()))
            return {};
        return createIndex(row, 0, root_->children[row].get());
    }

    const auto* parent_node = node_from_index(parent);
    if (!parent_node || row >= static_cast<int>(parent_node->children.size()))
        return {};

    return createIndex(row, col, parent_node->children[row].get());
}

QModelIndex PortfolioExplorerTreeModel::parent(const QModelIndex& index) const {
    const auto* node = node_from_index(index);
    if (!node || !node->parent || node->parent == root_.get())
        return {};

    return createIndex(node->parent->row_in_parent, 0, node->parent);
}

int PortfolioExplorerTreeModel::rowCount(const QModelIndex& parent) const {
    if (!parent.isValid())
        return root_ ? static_cast<int>(root_->children.size()) : 0;

    const auto* node = node_from_index(parent);
    if (!node)
        return 0;
    return static_cast<int>(node->children.size());
}

int PortfolioExplorerTreeModel::columnCount(const QModelIndex& /*parent*/) const {
    return 1;
}

QVariant PortfolioExplorerTreeModel::data(const QModelIndex& index, int role) const {
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
            return append_count(QString::fromStdString(node->portfolio.name), subtree_count(node));
        // Book: look up count directly
        const auto it = trade_counts_.find(boost::uuids::to_string(node->book.id));
        const auto n = it != trade_counts_.end() ? it->second : 0;
        return append_count(QString::fromStdString(node->book.name), n);
    }

    if (role == Qt::DecorationRole) {
        if (node->kind == PortfolioTreeNode::Kind::Party)
            return IconUtils::createRecoloredIcon(Icon::Organization, IconUtils::DefaultIconColor);
        if (node->kind == PortfolioTreeNode::Kind::Portfolio) {
            // Virtual portfolios use outline icon; real portfolios use filled icon
            const auto icon =
                node->portfolio.is_virtual == 1 ? Icon::Briefcase : Icon::BriefcaseFilled;
            return IconUtils::createRecoloredIcon(icon, IconUtils::DefaultIconColor);
        }
        // Book
        if (ores::refdata::domain::regulatory_book_type_constants::is_trading_book(node->book)) {
            return IconUtils::createRecoloredIcon(Icon::BookOpenFilled,
                                                  IconUtils::DefaultIconColor);
        }
        return IconUtils::createRecoloredIcon(Icon::BookOpen, IconUtils::DefaultIconColor);
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
