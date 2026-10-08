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
#ifndef ORES_WORKFLOW_CORE_SERVICE_WORKFLOW_GRAPH_HPP
#define ORES_WORKFLOW_CORE_SERVICE_WORKFLOW_GRAPH_HPP

#include "ores.workflow.api/service/workflow_definition.hpp"
#include "ores.workflow.core/export.hpp"
#include <boost/graph/adjacency_list.hpp>
#include <cstddef>
#include <optional>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

/**
 * @file workflow_graph.hpp
 * @brief A run's steps and the dependencies between them.
 *
 * The chain was a line and an ordinal said what came next. That stops being
 * true the moment a step gathers per book: an ordinal says which step is next,
 * not what a step is waiting for, and a step whose result is written by many
 * producers has no ordinal that describes when it may run.
 */
namespace ores::workflow::service {

/**
 * @brief One step of a run as its dependencies describe it.
 */
struct workflow_node {
    /** The step's identity, which is how a consumer names it. */
    std::string name;
    /** The steps whose results this one reads. */
    std::vector<std::string> consumes;
};

/**
 * @brief The nodes a definition's freshly built steps describe.
 */
[[nodiscard]] ORES_WORKFLOW_CORE_EXPORT std::vector<workflow_node>
nodes_of(const std::vector<workflow_step_def>& steps);

/**
 * @brief A run's steps as a directed graph.
 *
 * Vertices are steps and an edge runs from a step to each step that reads its
 * result. The graph is built once from what the run states and then answers
 * two questions: whether the chain is coherent at all, and which steps may be
 * dispatched given what has answered so far.
 *
 * A step is named once. A chain that names two steps the same way cannot say
 * which one a consumer reads, and a fan-out answers it by naming each producer
 * for what it produces — the batch key — so the names stay distinct.
 */
class ORES_WORKFLOW_CORE_EXPORT workflow_graph {
public:
    explicit workflow_graph(std::vector<workflow_node> nodes);

    /**
     * @brief Why the chain cannot be run, or nothing when it can.
     *
     * A consumer of a name no step produces, a name that appears twice, and a
     * cycle are all definitions that cannot be executed rather than states a
     * run can reach, so they are reported before anything is dispatched.
     */
    [[nodiscard]] const std::optional<std::string>& incoherent() const {
        return incoherent_;
    }

    /**
     * @brief The steps that may run now.
     *
     * A step may run when every step it reads has answered and it has not been
     * dispatched already. The result is ordered as order() is, so two callers
     * that see the same state choose the same step.
     *
     * @param satisfied   the names of the steps that have answered.
     * @param dispatched  the names of the steps already dispatched, whether or
     *                    not they have answered.
     */
    [[nodiscard]] std::vector<std::string> ready(const std::set<std::string>& satisfied,
                                                 const std::set<std::string>& dispatched) const;

    /**
     * @brief Every step, each of them after the steps it reads.
     *
     * Compensation walks a run's satisfied prefix backwards, so it reads this
     * order in reverse.
     */
    [[nodiscard]] const std::vector<std::string>& order() const { return order_; }

    /** How many steps the run has. */
    [[nodiscard]] std::size_t size() const { return order_.size(); }

private:
    using graph_t = boost::adjacency_list<boost::vecS, boost::vecS, boost::bidirectionalS, std::string>;

    graph_t graph_;
    /** The vertex a step's name belongs to. */
    std::unordered_map<std::string, graph_t::vertex_descriptor> by_name_;
    /** The vertices in an order where each comes after the steps it reads. */
    std::vector<std::string> order_;
    /** Why the chain cannot be run, empty when it can. */
    std::optional<std::string> incoherent_;
};

}

#endif
