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
#ifndef ORES_MARKETDATA_CORE_SERVICE_CURVE_BUILD_PLANNER_HPP
#define ORES_MARKETDATA_CORE_SERVICE_CURVE_BUILD_PLANNER_HPP

#include "ores.marketdata.core/export.hpp"
#include <expected>
#include <span>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief One artefact the producer builds, and the artefacts it declares as
 * inputs.
 *
 * @p id names the artefact as the set names it. @p inputs are the artefacts it
 * reads, and nothing else: an edge that is not declared here is not in the
 * plan, so an artefact that reads something without saying so is planned as
 * though it read nothing.
 */
struct ORES_MARKETDATA_CORE_EXPORT curve_build_node final {
    std::string id;
    std::vector<std::string> inputs;
};

/**
 * @brief One edge of the build graph: the node, and the input it declares.
 *
 * An edge whose @p input_id is not in the set is kept rather than dropped,
 * because it is the reason its node cannot build.
 */
struct ORES_MARKETDATA_CORE_EXPORT curve_build_edge final {
    std::string node_id;
    std::string input_id;
};

/**
 * @brief The levels a set of nodes builds in, and the nodes no level can hold.
 *
 * Level zero holds the nodes with no input inside the set, and each later level
 * holds the nodes whose inputs are all in earlier levels. Nodes of one level
 * share no edge, so their order does not matter and they may be built in
 * parallel; the end of a level is the only barrier.
 */
struct ORES_MARKETDATA_CORE_EXPORT curve_build_plan final {
    std::vector<std::vector<std::string>> levels;

    /**
     * @brief The nodes a level cannot hold, each with the one input that keeps
     * it out: an input the set does not hold, or one that is itself kept out.
     *
     * A node that cannot build is not an input the others can read, so a node
     * whose input is listed here is listed too.
     */
    std::vector<curve_build_edge> unbuildable;
};

/**
 * @brief Why a plan was refused rather than returned.
 */
enum class curve_build_refusal_kind {
    /// The declared edges form a cycle, so no level order builds them.
    cycle,
    /// The set names one id more than once, so the plan cannot say which node
    /// it orders.
    repeated_node
};

/**
 * @brief A refused plan, and the nodes it is about.
 */
struct ORES_MARKETDATA_CORE_EXPORT curve_build_refusal final {
    curve_build_refusal_kind kind = curve_build_refusal_kind::cycle;

    /// The nodes no level could take (a cycle), or the ids the set repeated.
    std::vector<std::string> node_ids;
};

/**
 * @brief Orders a set of artefacts by the inputs they declare.
 *
 * Pure: the plan reads the nodes and their edges and nothing else, so order,
 * missing inputs and cycles are decided without a database or a service. It
 * states what may be attempted first; whether an input is live at an instant is
 * the builder's question, not the plan's.
 *
 * A plan refused with @c curve_build_refusal_kind::cycle must not be built from:
 * the cycle is a configuration fault, and retrying the same edges cannot clear
 * it.
 *
 * @param nodes The artefacts to order, in the order the set names them. Each
 * id must be unique.
 */
ORES_MARKETDATA_CORE_EXPORT
std::expected<curve_build_plan, curve_build_refusal>
plan_curve_builds(std::span<const curve_build_node> nodes);

}

#endif
