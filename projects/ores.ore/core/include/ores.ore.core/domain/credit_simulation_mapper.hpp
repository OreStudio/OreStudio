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
#ifndef ORES_ORE_CORE_DOMAIN_CREDIT_SIMULATION_MAPPER_HPP
#define ORES_ORE_CORE_DOMAIN_CREDIT_SIMULATION_MAPPER_HPP

#include "ores.analytics.api/domain/credit_simulation_config.hpp"
#include "ores.analytics.api/domain/credit_simulation_entity_config.hpp"
#include "ores.analytics.api/domain/credit_simulation_matrix_cell_config.hpp"
#include "ores.analytics.api/domain/credit_simulation_matrix_config.hpp"
#include "ores.analytics.api/domain/credit_simulation_matrix_state_config.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/export.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::domain {

/**
 * @brief One ORE CreditSimulation document, mapped to the analytics entities.
 *
 * The five entity lists are the tables a document decomposes into: the root
 * configuration with its Risk block, the entities that migrate, the named
 * matrices with their bounds, the states each matrix spans and the matrix
 * cells. The netting set ids have no column of their own and ride beside the
 * entities.
 */
struct mapped_credit_simulation {
    analytics::domain::credit_simulation_config config;
    std::vector<analytics::domain::credit_simulation_entity_config> entities;
    std::vector<analytics::domain::credit_simulation_matrix_config> matrices;
    std::vector<analytics::domain::credit_simulation_matrix_state_config> states;
    std::vector<analytics::domain::credit_simulation_matrix_cell_config> cells;
    std::string netting_set_ids;
};

/**
 * @brief Maps between an ORE CreditSimulation XML document and the analytics
 * credit simulation entities.
 *
 * Import turns each matrix's Data grid into a matrix row, one state row per
 * label the grid's comment names, and one cell row per entry, keyed to the
 * matrix and the (from_state, to_state) pair; an entity refers to its matrix
 * by id, never by text. Export reassembles the grid from the state and cell
 * rows, so the document comes back cell for cell and label for label.
 */
class ORES_ORE_CORE_EXPORT credit_simulation_mapper {
private:
    inline static std::string_view logger_name = "ores.ore.domain.credit_simulation_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    static bool parse_bool(domain::bool_ v);
    static domain::bool_ make_bool(bool v);

public:
    /**
     * @brief Maps an ORE CreditSimulation document to the analytics entities.
     *
     * Every matrix becomes a matrix row plus a state row per label and a cell
     * row per grid entry, and every entity points at the matrix id its
     * TransitionMatrix name resolves to.
     */
    static mapped_credit_simulation map(const creditsimulation& v);

    /**
     * @brief Reconstructs an ORE CreditSimulation document from mapped entities.
     *
     * Each matrix's grid is reassembled from its state and cell rows.
     */
    static creditsimulation reverse(const mapped_credit_simulation& v);
};

}

#endif
