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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */
#ifndef ORES_ORE_CORE_DOMAIN_CALENDAR_NAMES_HPP
#define ORES_ORE_CORE_DOMAIN_CALENDAR_NAMES_HPP

#include "ores.ore.core/export.hpp"
#include <string>

namespace ores::ore::domain {

/**
 * @brief The name ORE states for a calendar, given the name the convention uses.
 *
 * QuantLib and ORE name the same calendar differently for the codes below, and
 * ORE drops a convention whose calendar it cannot convert, taking the instrument
 * that uses it with it. A code the two vocabularies spell the same is returned
 * unchanged.
 */
ORES_ORE_CORE_EXPORT std::string ore_calendar_name(const std::string& code);

}

#endif
