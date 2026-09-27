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
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::domain {

/**
 * @brief The state labels one transition matrix's Data element declares.
 *
 * ORE names a matrix's states through the optional @c t0 and @c t1 attributes
 * of the @c Data element, beside the grid of probabilities. The analytics
 * matrix table carries only the matrix's name, so the labels are held here
 * while a document is in flight and written back on export. A caller that
 * persists the mapped matrices cannot recover them, exactly as with
 * @c mapped_fx::advance_calendars.
 */
struct mapped_matrix_states {
    boost::uuids::uuid matrix_id;
    std::string t0;
    std::string t1;
};

/**
 * @brief One ORE CreditSimulation document, mapped to the analytics entities.
 *
 * The four entity lists are the tables a document decomposes into: the root
 * configuration with its Risk block, the entities that migrate, the named
 * matrices and the matrix cells. Two values on the ORE document have no
 * column of their own and ride beside the entities: the netting set ids and
 * each matrix's state labels.
 */
struct mapped_credit_simulation {
    analytics::domain::credit_simulation_config config;
    std::vector<analytics::domain::credit_simulation_entity_config> entities;
    std::vector<analytics::domain::credit_simulation_matrix_config> matrices;
    std::vector<analytics::domain::credit_simulation_matrix_cell_config> cells;
    std::vector<mapped_matrix_states> matrix_states;
    std::string netting_set_ids;
};

/**
 * @brief Maps between an ORE CreditSimulation XML document and the analytics
 * credit simulation entities.
 *
 * Import turns each matrix's Data grid into one cell row per entry, keyed to
 * the matrix and the (from_state, to_state) pair; a matrix's name becomes its
 * identity within the document and an entity refers to it by that id, never
 * by text. Export reassembles each matrix's grid from its rows, ordered by
 * from_state and then to_state, so the document comes back cell for cell.
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

    static std::string format_number(double value);
    static std::vector<std::vector<double>>
    assemble_grid(const std::vector<analytics::domain::credit_simulation_matrix_cell_config>& cells);

public:
    /**
     * @brief Maps an ORE CreditSimulation document to the analytics entities.
     *
     * Every matrix becomes a matrix row plus one cell row per grid entry, and
     * every entity points at the matrix id its TransitionMatrix name resolves
     * to.
     */
    static mapped_credit_simulation map(const creditsimulation& v);

    /**
     * @brief Reconstructs an ORE CreditSimulation document from mapped entities.
     *
     * Each matrix's grid is reassembled from its cell rows and the state
     * labels recorded at import are restored.
     */
    static creditsimulation reverse(const mapped_credit_simulation& v);

    /**
     * @brief Parses a Data element's whitespace or comma separated square grid.
     *
     * Tokens that are not numbers are skipped, so a comment the decoder left in
     * the character data cannot derail the grid. A grid that is not square is
     * an error.
     */
    static std::vector<std::vector<double>> parse_grid(const std::string& text);

    /**
     * @brief Formats a square grid as the text of a Data element.
     *
     * Numbers use the shortest representation that parses back to the same
     * value, so a round trip is exact.
     */
    static std::string format_grid(const std::vector<std::vector<double>>& grid);
};

}

#endif
