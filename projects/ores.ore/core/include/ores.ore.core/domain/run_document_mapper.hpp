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
#include "ores.reporting.api/domain/report_run_setup.hpp"
#include <string>

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
     * A parameter the entity has no column for is ignored rather than guessed
     * at; the corpus uses forty names and the entity carries all forty.
     */
    static reporting::domain::report_run_setup map_setup(const ore& v);

    /**
     * @brief Reconstructs an ORE Setup block from the setup entity.
     *
     * The parameters are written in a fixed order, because ORE reads them by
     * name and the entity has no order of its own.
     */
    static parameterListType reverse_setup(const reporting::domain::report_run_setup& v);
};

}

#endif
