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
#ifndef ORES_QT_PARSE_BOND_IMPL_HPP
#define ORES_QT_PARSE_BOND_IMPL_HPP

// Internal header: declares the bond-instrument parse entry point implemented
// in parse_bond_instruments.cpp. Kept separate so that the rfl::json::read
// instantiation for the assembled bond container lives in its own translation
// unit, avoiding MSVC C1202 (template dependency graph too complex) which fires
// when those instantiations share a TU with the flat/FX/equity instrument types.

#include <string>

namespace ores::qt {
struct IInstrumentFormPopulator;
}

namespace ores::qt::internal {

// Parses the assembled bond container (the slim header, its issue row and the
// product's engaged fact row) out of the response payload. Returns false and
// logs on parse failure.
bool parse_bond_instrument(const std::string& raw, ores::qt::IInstrumentFormPopulator& pop);

}

#endif
