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
#ifndef ORES_MARKETDATA_CORE_ORESMD_PILLAR_QUOTE_HPP
#define ORES_MARKETDATA_CORE_ORESMD_PILLAR_QUOTE_HPP

#include "ores.marketdata.core/export.hpp"
#include <chrono>
#include <string>

namespace ores::marketdata::core {

/**
 * @brief The term of the datum a curve pillar's quote is: the coordinate that
 * tells one rung of a ladder from the next.
 *
 * A pillar names the series it reads, and its end tenor names the term within
 * that series, so a template that names both ends of its pillar reads the same
 * quote on every day it is read: a 1M pillar ends at 1M. A pillar the template
 * names no period end for -- a meeting-dated pillar, or one whose end is a date
 * -- takes the days its end date lies after its start, which is the only case
 * the dates decide the term.
 *
 * The reader resolves both dates through the config's tenor convention and
 * passes them here, so the term is the pillar's end tenor resolved through the
 * conventions and not a fresh spelling of it.
 *
 * @throws std::invalid_argument if a spot pillar's end date does not lie after
 *         its start date.
 */
ORES_MARKETDATA_CORE_EXPORT std::string
pillar_term_text(const std::string& start_tenor_code,
                 const std::string& end_tenor_code,
                 std::chrono::year_month_day start_date,
                 std::chrono::year_month_day end_date);

}

#endif
