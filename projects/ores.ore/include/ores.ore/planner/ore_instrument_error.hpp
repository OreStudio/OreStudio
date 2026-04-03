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
#ifndef ORES_ORE_PLANNER_ORE_INSTRUMENT_ERROR_HPP
#define ORES_ORE_PLANNER_ORE_INSTRUMENT_ERROR_HPP

#include <string>

namespace ores::ore::planner {

/**
 * @brief Records a single instrument save failure during an ORE import.
 */
struct ore_instrument_error {
    /**
     * @brief External trade ID of the trade whose instrument could not be saved.
     */
    std::string trade_external_id;

    /**
     * @brief Error message returned by the server, or a transport error description.
     */
    std::string message;
};

}

#endif
