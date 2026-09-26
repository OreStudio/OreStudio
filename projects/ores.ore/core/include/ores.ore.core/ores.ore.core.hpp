/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_ORE_CORE_HPP
#define ORES_ORE_CORE_HPP

/**
 * @brief ORE document engine
 *
 * Moves ORE documents in and out of the ORES domain model through:
 * - The ORE XML bindings for the whole vocabulary, generated from the
 *   vendored schemas by xsdcpp
 * - The instrument mappers between those bindings and the trading and
 *   reference data domain types
 * - The importer, the exporter and the directory round trip
 * - Readers for ORE's market data text formats and its engine log
 * - The import planner, the hierarchy builder and the directory scanner
 */
namespace ores::ore {}

#endif
