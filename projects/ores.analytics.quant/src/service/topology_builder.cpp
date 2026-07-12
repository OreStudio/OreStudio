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
#include "ores.analytics.quant/service/topology_builder.hpp"
#include "ores.analytics.quant/domain/topology_build_error.hpp"
#include <boost/pending/disjoint_sets.hpp>
#include <algorithm>
#include <queue>
#include <unordered_map>
#include <unordered_set>
#include <utility>

namespace ores::analytics::quant::service {

namespace {

using domain::ccy_pair;
using domain::ccy_pair_input;
using domain::crm_topology;
using domain::currency_id;
using domain::topology_error;
using domain::topology_error_kind;

/// Resolves currency codes to compact 0-based indices, assigning a new one
/// the first time a code is seen. Order of first appearance in the input.
class currency_index_builder {
public:
    currency_id resolve(const std::string& code) {
        auto [it, inserted] = index_.try_emplace(
            code, currency_id(static_cast<std::uint16_t>(codes_.size())));
        if (inserted) codes_.push_back(code);
        return it->second;
    }

    [[nodiscard]] std::unordered_map<std::string, currency_id> index() && {
        return std::move(index_);
    }
    [[nodiscard]] std::vector<std::string> codes() && { return std::move(codes_); }
    [[nodiscard]] std::size_t size() const noexcept { return codes_.size(); }

private:
    std::unordered_map<std::string, currency_id> index_;
    std::vector<std::string> codes_;
};

} // namespace

domain::crm_topology topology_builder::build(
    const std::vector<ccy_pair_input>& pairs, const std::string& pivot_code,
    const std::vector<std::string>& required_majors) {
    currency_index_builder resolver;
    // The pivot itself must be a known vertex even if it never appears as a
    // lone token in a pair, so it is resolved unconditionally alongside
    // everything else -- an empty/garbage pivot_code becomes an isolated
    // vertex with no edges, which the BFS below still visits (it is its own
    // starting point), so pivot validity isn't the thing to check here; see
    // required_majors/disconnected_currency below for the real diagnostics.
    std::vector<ccy_pair> resolved_pairs;
    resolved_pairs.reserve(pairs.size());
    for (const auto& input : pairs) {
        resolved_pairs.push_back(ccy_pair{
            resolver.resolve(input.base_code), resolver.resolve(input.quote_code),
            input.is_driver});
    }
    resolver.resolve(pivot_code);
    for (const auto& major : required_majors) resolver.resolve(major);

    const std::size_t vertex_count = resolver.size();
    auto currency_index = std::move(resolver).index();
    auto currency_codes = std::move(resolver).codes();

    std::vector<topology_error> errors;

    // Incremental cycle detection: boost::disjoint_sets rejects (as a
    // cycle_conflict) any edge that would connect two currencies already
    // in the same set -- i.e. a second path between them. This guarantees
    // the resulting graph has at most one path between any two currencies
    // by construction; an ambiguous config is caught here, never resolved
    // silently.
    std::vector<std::size_t> rank(vertex_count);
    std::vector<std::size_t> parent_rep(vertex_count);
    boost::disjoint_sets<std::size_t*, std::size_t*> disjoint_set(
        rank.data(), parent_rep.data());
    for (std::size_t v = 0; v < vertex_count; ++v) disjoint_set.make_set(v);

    std::unordered_set<std::uint64_t> seen_edges;
    auto edge_key = [](currency_id a, currency_id b) -> std::uint64_t {
        const auto lo = std::min(a.index(), b.index());
        const auto hi = std::max(a.index(), b.index());
        return (static_cast<std::uint64_t>(lo) << 32) | hi;
    };

    // Adjacency built alongside union-find so we can BFS from the pivot
    // once validation succeeds, without a second Boost graph pass.
    std::vector<std::vector<std::pair<currency_id, ccy_pair>>> adjacency(vertex_count);

    for (std::size_t i = 0; i < resolved_pairs.size(); ++i) {
        const auto& pair = resolved_pairs[i];
        const auto& input = pairs[i];
        const auto key = edge_key(pair.base, pair.quote);
        if (!seen_edges.insert(key).second) {
            errors.push_back(topology_error{
                topology_error_kind::duplicate_edge, input.base_code, input.quote_code});
            continue;
        }

        const auto base_rep = disjoint_set.find_set(pair.base.index());
        const auto quote_rep = disjoint_set.find_set(pair.quote.index());
        if (base_rep == quote_rep) {
            errors.push_back(topology_error{
                topology_error_kind::cycle_conflict, input.base_code, input.quote_code});
            continue;
        }
        disjoint_set.union_set(base_rep, quote_rep);
        adjacency[pair.base.index()].emplace_back(pair.quote, pair);
        adjacency[pair.quote.index()].emplace_back(pair.base, pair);
    }

    // pivot_code was resolved unconditionally above, so it is always
    // present here -- this can never throw std::out_of_range.
    const currency_id pivot = currency_index.at(pivot_code);

    // BFS from the pivot: assigns parent[]/edge_to_parent[] and, as a side
    // effect, tells us exactly which currencies are unreachable.
    std::vector<currency_id> parent(vertex_count, pivot);
    std::vector<std::optional<ccy_pair>> edge_to_parent(vertex_count);
    std::vector<bool> visited(vertex_count, false);
    std::queue<currency_id> frontier;
    frontier.push(pivot);
    visited[pivot.index()] = true;
    while (!frontier.empty()) {
        const auto current = frontier.front();
        frontier.pop();
        for (const auto& [neighbour, edge] : adjacency[current.index()]) {
            if (visited[neighbour.index()]) continue;
            visited[neighbour.index()] = true;
            parent[neighbour.index()] = current;
            edge_to_parent[neighbour.index()] = edge;
            frontier.push(neighbour);
        }
    }

    // Required majors get the more specific missing_major diagnosis; the
    // general disconnected_currency sweep below skips them so each
    // unreachable currency is reported exactly once.
    std::unordered_set<std::uint16_t> major_ids;
    for (const auto& major : required_majors) {
        const auto id = currency_index.at(major);
        major_ids.insert(id.index());
        if (!visited[id.index()]) {
            errors.push_back(topology_error{
                topology_error_kind::missing_major, major, ""});
        }
    }
    for (std::size_t v = 0; v < vertex_count; ++v) {
        if (!visited[v] && !major_ids.contains(static_cast<std::uint16_t>(v))) {
            errors.push_back(topology_error{
                topology_error_kind::disconnected_currency, currency_codes[v], ""});
        }
    }

    if (!errors.empty()) throw domain::topology_build_error(std::move(errors));

    return crm_topology(pivot, std::move(parent), std::move(edge_to_parent),
        std::move(currency_index), std::move(currency_codes));
}

} // namespace ores::analytics::quant::service
