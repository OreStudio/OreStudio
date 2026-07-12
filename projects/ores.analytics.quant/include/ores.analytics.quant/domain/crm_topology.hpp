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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_CRM_TOPOLOGY_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_CRM_TOPOLOGY_HPP

#include "ores.analytics.quant/domain/ccy_pair.hpp"
#include "ores.analytics.quant/domain/currency_id.hpp"
#include <algorithm>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace ores::analytics::quant::domain {

/**
 * @brief The CRM's driver/derived spanning tree: immutable once built.
 *
 * Built exactly once by @c topology_builder and never mutated again for the
 * lifetime of a runtime engine -- see the parent task's design notes for why
 * structure (rare, this class) and rate values (continuous, a later task)
 * are deliberately kept separate.
 *
 * Storage is undirected (a currency's path to the pivot is just an edge
 * list); which side of each edge is the driver is carried on the edge
 * itself via the @c ccy_pair_input the edge was built from, not by the
 * direction it is stored in.
 */
class crm_topology {
public:
    crm_topology(currency_id pivot,
                 std::vector<currency_id> parent,
                 std::vector<std::optional<ccy_pair>> edge_to_parent,
                 std::unordered_map<std::string, currency_id> currency_index,
                 std::vector<std::string> currency_code)
        : pivot_(pivot)
        , parent_(std::move(parent))
        , edge_to_parent_(std::move(edge_to_parent))
        , currency_index_(std::move(currency_index))
        , currency_code_(std::move(currency_code)) {}

    [[nodiscard]] std::size_t vertex_count() const noexcept {
        return currency_code_.size();
    }

    [[nodiscard]] currency_id pivot() const noexcept {
        return pivot_;
    }

    /// The parent of @p id in the spanning tree; equals @p id itself iff
    /// @p id is the pivot. Used by the rate engine to walk the tree without
    /// re-deriving it.
    [[nodiscard]] currency_id parent(currency_id id) const {
        return parent_.at(id.index());
    }

    /// The edge from @p id to its parent, or @c std::nullopt for the pivot.
    [[nodiscard]] const std::optional<ccy_pair>& edge_to_parent(currency_id id) const {
        return edge_to_parent_.at(id.index());
    }

    [[nodiscard]] std::optional<currency_id> currency_id_for(const std::string& code) const {
        const auto it = currency_index_.find(code);
        if (it == currency_index_.end())
            return std::nullopt;
        return it->second;
    }

    [[nodiscard]] const std::string& code_of(currency_id id) const {
        return currency_code_.at(id.index());
    }

    /// The edges from @p id up to the pivot, pivot-first. Empty for the
    /// pivot itself.
    [[nodiscard]] std::vector<ccy_pair> path_to_pivot(currency_id id) const {
        std::vector<ccy_pair> path;
        currency_id current = id;
        while (current != pivot_) {
            const auto& edge = edge_to_parent_.at(current.index());
            path.push_back(*edge);
            current = parent_.at(current.index());
        }
        std::reverse(path.begin(), path.end());
        return path;
    }

private:
    currency_id pivot_;
    std::vector<currency_id> parent_;
    std::vector<std::optional<ccy_pair>> edge_to_parent_;
    std::unordered_map<std::string, currency_id> currency_index_;
    std::vector<std::string> currency_code_;
};

} // namespace ores::analytics::quant::domain

#endif
