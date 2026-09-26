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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_UTILITY_ORES_UTILITY_HPP
#define ORES_UTILITY_ORES_UTILITY_HPP

/**
 * @brief Foundation utilities used across all ORE Studio components.
 *
 * The component holds the small, dependency-light pieces more than one
 * component needs. Its facets:
 *
 * - @b compression: gzip over raw bytes, the tree's one gzip implementation.
 * - @b convert: the Base64 codec.
 * - @b crypto: the SHA-256 digest.
 * - @b domain: the entity-agnostic hierarchy row and the domain request
 *   outcome enumeration.
 * - @b faker: the internet and datetime extensions to faker-cxx.
 * - @b generation: the seeded generation engine, its context and environment,
 *   and the tree builder.
 * - @b program_options: the options every application shares and the
 *   ORES_<DOMAIN>_<OPTION> mapping.
 * - @b rfl: the reflect-cpp reflectors and the time point parser.
 * - @b serialization: the wire error enumeration.
 * - @b streaming: the std::vector stream operator and the uuid and tenant_id
 *   parsers.
 * - @b string: the counterparty short-code generator.
 * - @b uuid: version 7 identifiers and the tenant identity type.
 * - @b version: the CMake-stamped product version.
 *
 * It depends on =ores.platform=, which also supplies reflect-cpp, and on
 * nothing else in the tree, so every other component may use it without
 * risking a dependency cycle.
 */
namespace ores::utility {}

#endif
