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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_REFDATA_API_TESTS_TEST_HELPERS_HPP_
#define ORES_REFDATA_API_TESTS_TEST_HELPERS_HPP_

#include "ores.refdata.api/domain/tenor_resolution.hpp"

using ores::refdata::domain::tenor;

// The 1F..8F tenors, transcribed from refdata_tenors_populate.sql (task
// D2): kind SPECIAL, unit NONE, multiplier the meeting ordinal.
inline tenor make_fomc_tenor(int n) {
    tenor t;
    t.code = std::to_string(n) + "F";
    t.kind = "SPECIAL";
    t.unit = "NONE";
    t.multiplier = n;
    return t;
}

#endif
