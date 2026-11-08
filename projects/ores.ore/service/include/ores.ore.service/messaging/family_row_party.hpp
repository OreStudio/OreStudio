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
#ifndef ORES_ORE_SERVICE_MESSAGING_FAMILY_ROW_PARTY_HPP
#define ORES_ORE_SERVICE_MESSAGING_FAMILY_ROW_PARTY_HPP

#include "ores.utility/codec/json_document.hpp"
#include <rfl/Generic.hpp>
#include <string>

namespace ores::ore::service::messaging {

/**
 * @brief The party a convention family row belongs to, as a list response
 * carries it.
 *
 * A flat type states party_id among its own fields. A type that groups its
 * identity, such as the central convention and the parts keyed to it, states
 * it inside its identity group. Empty when the row names no party.
 */
std::string party_of_family_row(const ores::utility::codec::json_value& row);

}

#endif
