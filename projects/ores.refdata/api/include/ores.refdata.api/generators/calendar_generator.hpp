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
#ifndef ORES_REFDATA_API_GENERATORS_CALENDAR_GENERATOR_HPP
#define ORES_REFDATA_API_GENERATORS_CALENDAR_GENERATOR_HPP

#include "ores.refdata.api/domain/calendar.hpp"
#include "ores.refdata.api/export.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include <vector>

namespace ores::refdata::generators {

/**
 * @brief Generates a synthetic calendar.
 */
ORES_REFDATA_API_EXPORT domain::calendar
generate_synthetic_calendar(utility::generation::generation_context& ctx);

/**
 * @brief Generates N synthetic calendars.
 */
ORES_REFDATA_API_EXPORT std::vector<domain::calendar>
generate_synthetic_calendars(std::size_t n, utility::generation::generation_context& ctx);

/**
 * @brief Generates the full QuantLib calendar set as real reference data.
 *
 * Transcribed from QuantLib's calendar headers (ql/time/calendars/):
 * one row per concrete calendar token, including sub-market variants
 * (e.g. UnitedStates.NYSE, UnitedStates.GovernmentBond) as distinct
 * rows rather than a joined variant field. This is real reference
 * data, not fictional test data — every code matches ORE's XML
 * <Calendar> vocabulary verbatim.
 */
ORES_REFDATA_API_EXPORT std::vector<domain::calendar>
generate_quantlib_calendars(utility::generation::generation_context& ctx);
}

#endif
