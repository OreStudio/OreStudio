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
#ifndef ORES_ORE_CORE_DOMAIN_RUN_DOCUMENT_MAPPER_HPP
#define ORES_ORE_CORE_DOMAIN_RUN_DOCUMENT_MAPPER_HPP

#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/export.hpp"
#include "ores.reporting.api/domain/report_analytic.hpp"
#include "ores.reporting.api/domain/report_market_binding.hpp"
#include "ores.reporting.api/domain/report_run_setup.hpp"
#include "ores.reporting.api/domain/run_document.hpp"
#include <string>
#include <vector>

namespace ores::ore::domain {

/**
 * @brief Maps between an ORE run document and the reporting run entities.
 *
 * The Setup block is the first half: its forty parameters are named strings in
 * ORE's schema and typed columns in report_run_setup. The mapper reads each
 * named parameter into its column and writes the columns back as the named
 * parameters ORE expects, so a run document's Setup block survives a trip
 * through the entity.
 *
 * Values are carried as ORE spells them. The flags are not all booleans -- the
 * fixing and cashflow flags are Y or N, the model flags are true or false, and
 * accrualDate may be the literal ASOF -- so the columns are text and the
 * mapping is a rename, not a conversion.
 */
class ORES_ORE_CORE_EXPORT run_document_mapper {
public:
    /**
     * @brief Maps an ORE run document's Setup block to the setup entity.
     *
     * A parameter the entity has no column for is refused rather than dropped,
     * and a value that cannot be read as the column's type is refused too: a
     * document that loses a parameter on the way in would otherwise lose it
     * silently. The corpus uses forty names and the entity carries all forty,
     * so nothing shipped is refused.
     *
     * A name that appears twice is read from its last occurrence. Three shipped
     * documents repeat continueOnError and in all three the values agree.
     */
    static reporting::domain::report_run_setup map_setup(const ore& v);

    /**
     * @brief Reconstructs an ORE Setup block from the setup entity.
     *
     * The parameters are written in the mapper's own fixed order, which is not
     * necessarily the order the document wrote them in: the entity is a row of
     * columns and has no order of its own. ORE reads the parameters by name, so
     * the order is lossy on purpose and the round trip compares names and
     * values rather than positions.
     */
    static parameterListType reverse_setup(const reporting::domain::report_run_setup& v);

    /**
     * @brief Maps a run document's ordered analytic list to the entities.
     *
     * The order is the order the document wrote, counting from one, and it is
     * what ORE runs the analytics in. Every element carries an active flag,
     * which becomes a column rather than a parameter. Each parameter carries
     * its own position within its analytic, so a later writer can fill the
     * column the schema asks for.
     */
    static std::vector<ores::reporting::domain::run_analytic> map_analytics(const ore& v);

    /**
     * @brief Reconstructs a run document's ordered analytic list.
     *
     * The active flag is written back as the first parameter, which is where
     * ORE writes it, and the rest follow in order.
     */
    static analyticsType
    reverse_analytics(const std::vector<ores::reporting::domain::run_analytic>& v);

    /**
     * @brief Maps a run document's named market bindings to the entities.
     *
     * Each binding is a market role and the configuration set the run asks it
     * for, in the order the document wrote them.
     */
    static std::vector<reporting::domain::report_market_binding> map_market_bindings(const ore& v);

    /**
     * @brief Reconstructs a run document's named market bindings.
     */
    static parameterListType
    reverse_market_bindings(const std::vector<reporting::domain::report_market_binding>& v);
};

}

#endif
