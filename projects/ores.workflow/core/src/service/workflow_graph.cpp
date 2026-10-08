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
#include "ores.workflow.core/service/workflow_graph.hpp"
#include <boost/graph/topological_sort.hpp>

namespace ores::workflow::service {

std::vector<workflow_node> nodes_of(const std::vector<workflow_step_def>& steps) {
    std::vector<workflow_node> nodes;
    nodes.reserve(steps.size());
    for (const auto& s : steps)
        nodes.push_back({s.name, s.consumes});
    return nodes;
}

std::vector<workflow_node> nodes_of(const std::vector<materialised_step>& steps) {
    std::vector<workflow_node> nodes;
    nodes.reserve(steps.size());
    for (const auto& s : steps)
        nodes.push_back({s.name, s.consumes});
    return nodes;
}

workflow_graph::workflow_graph(std::vector<workflow_node> nodes) {
    for (const auto& node : nodes) {
        if (by_name_.contains(node.name)) {
            incoherent_ = "The chain holds two steps called '" + node.name +
                          "', so a step that reads it cannot say which one it reads.";
            return;
        }
        by_name_.emplace(node.name, boost::add_vertex(node.name, graph_));
    }

    // The engine advances along the chain in the order it was declared, so a
    // step must read the steps that come before it and no others. That is what
    // makes the declaration order a topological one. The graph derives the
    // order itself and would happily run a chain declared the other way round;
    // the engine would not, and a chain it cannot run is refused here rather
    // than left to fail at the step that finds its input missing. Dispatching
    // from the graph's own order instead of the declaration is what lifts this.
    std::unordered_map<std::string, std::size_t> position;
    for (std::size_t i = 0; i < nodes.size(); ++i)
        position.emplace(nodes[i].name, i);

    for (const auto& node : nodes) {
        const auto consumer = by_name_.at(node.name);
        for (const auto& input : node.consumes) {
            const auto producer = by_name_.find(input);
            if (producer == by_name_.end()) {
                incoherent_ = "The chain builds step '" + node.name + "' to read '" + input +
                              "', which no step produces.";
                return;
            }
            if (position.at(input) >= position.at(node.name)) {
                incoherent_ = "The chain builds step '" + node.name + "' to read '" + input +
                              "', which the engine reaches only after it, so the step would "
                              "run before its input.";
                return;
            }
            boost::add_edge(producer->second, consumer, graph_);
        }
    }

    // Defence rather than reachable: the order rule above already makes every
    // chain acyclic. A cycle here would mean that rule had stopped holding.
    std::vector<graph_t::vertex_descriptor> sorted;
    try {
        boost::topological_sort(graph_, std::back_inserter(sorted));
    } catch (const boost::not_a_dag&) {
        incoherent_ = "The chain's steps read each other, so none of them can run first.";
        return;
    }

    // topological_sort answers with each vertex before the ones it points at,
    // which is the reverse of the order this needs.
    order_.reserve(sorted.size());
    for (auto it = sorted.rbegin(); it != sorted.rend(); ++it)
        order_.push_back(graph_[*it]);
}

std::vector<std::string> workflow_graph::ready(const std::set<std::string>& satisfied,
                                               const std::set<std::string>& dispatched) const {
    std::vector<std::string> ready_now;
    if (incoherent_)
        return ready_now;

    for (const auto& name : order_) {
        if (dispatched.contains(name))
            continue;
        const auto node = by_name_.at(name);

        bool waiting = false;
        for (auto [in, end] = boost::in_edges(node, graph_); in != end; ++in) {
            if (!satisfied.contains(graph_[boost::source(*in, graph_)])) {
                waiting = true;
                break;
            }
        }
        if (!waiting)
            ready_now.push_back(name);
    }
    return ready_now;
}

}
