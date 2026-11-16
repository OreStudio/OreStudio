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
#ifndef ORES_ORE_CORE_DOMAIN_SCRIPT_LIBRARY_PROJECTION_HPP
#define ORES_ORE_CORE_DOMAIN_SCRIPT_LIBRARY_PROJECTION_HPP

#include "ores.ore.core/export.hpp"
#include "ores.trading.api/domain/script.hpp"
#include "ores.trading.api/domain/script_calibration.hpp"
#include "ores.trading.api/domain/script_calibration_strike.hpp"
#include "ores.trading.api/domain/script_coarsening.hpp"
#include "ores.trading.api/domain/script_result.hpp"
#include "ores.trading.api/domain/script_variant.hpp"
#include <string>
#include <vector>

namespace ores::ore::domain {

/**
 * @brief The rows the script library is held in, as the store reads them.
 */
struct script_library_rows final {
    std::vector<trading::domain::script> scripts;
    std::vector<trading::domain::script_variant> variants;
    std::vector<trading::domain::script_result> results;
    std::vector<trading::domain::script_calibration> calibrations;
    std::vector<trading::domain::script_calibration_strike> calibration_strikes;
    std::vector<trading::domain::script_coarsening> coarsenings;
};

/**
 * @brief Writes the scriptlibrary.xml document ORE reads from the rows.
 *
 * Scripts are written in name order, a script's variants with the default
 * first, and each child in its sequence. A variant whose purpose is the
 * default states no purpose attribute, as ORE's library does not.
 */
ORES_ORE_CORE_EXPORT std::string write_script_library(const script_library_rows& rows);

}

#endif
