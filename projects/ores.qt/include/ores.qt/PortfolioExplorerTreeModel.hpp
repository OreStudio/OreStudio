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
#ifndef ORES_QT_PORTFOLIO_EXPLORER_TREE_MODEL_HPP
#define ORES_QT_PORTFOLIO_EXPLORER_TREE_MODEL_HPP

#include <memory>
#include <vector>
#include <optional>
#include <unordered_map>
#include <QString>
#include <QAbstractItemModel>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.refdata/domain/portfolio.hpp"
#include "ores.refdata/domain/book.hpp"

namespace ores::qt {

/**
 * @brief A single node in the portfolio/book tree.
 *
 * Nodes are owned by the model. Raw pointers are stored in
 * QModelIndex::internalPointer().
 */
struct PortfolioTreeNode {
    enum class Kind { Party, Portfolio, Book };

    Kind kind;
    QString party_name;                        // valid when kind == Party
    refdata::domain::portfolio portfolio;      // valid when kind == Portfolio
    refdata::domain::book book;                // valid when kind == Book
    PortfolioTreeNode* parent = nullptr;
    std::vector<std::unique_ptr<PortfolioTreeNode>> children;
    int row_in_parent = 0;
};

/**
 * @brief Filter result from a tree node selection.
 */
struct TreeNodeFilter {
    std::optional<boost::uuids::uuid> book_id;
    std::optional<boost::uuids::uuid> portfolio_id;
};

/**
 * @brief Tree model for the portfolio/book hierarchy.
 *
 * The top-level item is always the owning party node. Real portfolios are
 * inner nodes and books are leaves. Virtual portfolios (is_virtual == 1) are
 * included as structural parents distinguished by the outline Briefcase icon.
 */
class PortfolioExplorerTreeModel final : public QAbstractItemModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.portfolio_explorer_tree_model";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit PortfolioExplorerTreeModel(QObject* parent = nullptr);
    ~PortfolioExplorerTreeModel() override = default;

    /**
     * @brief Rebuild the tree from raw portfolio and book data.
     *
     * Creates a single party root node named party_name, then adds portfolios
     * and books beneath it. Books are placed as leaves under their
     * parent_portfolio_id.
     */
    void load(const QString& party_name,
              std::vector<refdata::domain::portfolio> portfolios,
              std::vector<refdata::domain::book> books);

    /**
     * @brief Returns the filter for the selected index.
     *
     * For a book node: sets book_id.
     * For a portfolio node: sets portfolio_id.
     * For a party node: returns nullopt pair (show all trades).
     * Returns nullopt pair if index is invalid.
     */
    TreeNodeFilter selected_filter(const QModelIndex& index) const;

    /**
     * @brief Update the trade count for a book and refresh its display.
     *
     * Called after background count fetches complete. Stores the count and
     * emits dataChanged so the book label updates to show "(N)".
     */
    void set_trade_count(const boost::uuids::uuid& book_id, std::uint32_t count);

    // QAbstractItemModel interface
    QModelIndex index(int row, int col,
        const QModelIndex& parent = QModelIndex()) const override;
    QModelIndex parent(const QModelIndex& index) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;

    /**
     * @brief Returns the node for a given index, or nullptr if invalid.
     */
    PortfolioTreeNode* node_from_index(const QModelIndex& index) const;

private:
    void build_subtree(PortfolioTreeNode* parent_node,
        const std::vector<refdata::domain::portfolio>& portfolios,
        const std::vector<refdata::domain::book>& books,
        const std::optional<boost::uuids::uuid>& parent_id);

    QModelIndex find_book_index(const boost::uuids::uuid& id) const;
    std::uint32_t subtree_count(const PortfolioTreeNode* node) const;

    std::unique_ptr<PortfolioTreeNode> root_;
    std::unordered_map<std::string, std::uint32_t> trade_counts_;
};

}

#endif
