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
#ifndef ORES_SYNTHETIC_API_MESSAGING_SIMULATE_PATHS_COMMON_HPP
#define ORES_SYNTHETIC_API_MESSAGING_SIMULATE_PATHS_COMMON_HPP

namespace ores::synthetic::messaging {

/**
 * @brief Batch bounds every simulate_paths request shares: the service clamps
 * to them and a UI spinner offers them as its maxima.
 *
 * One pair of numbers for FX spot and IR curve alike, so the two envelopes
 * cannot drift apart.
 */
inline constexpr int max_simulate_num_ticks = 5000;
inline constexpr int max_simulate_num_paths = 50;

}

#endif
