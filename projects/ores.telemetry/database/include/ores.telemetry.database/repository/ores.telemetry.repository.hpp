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
#ifndef ORES_TELEMETRY_REPOSITORY_HPP
#define ORES_TELEMETRY_REPOSITORY_HPP

/**
 * @brief The repository namespace of the database part.
 *
 * It holds the entity types that map to the component's four tables, the
 * mappers that convert between an entity and its domain type, and the
 * telemetry_repository that reads and writes them. The repository supports
 * batch inserts and the time-range queries the log list is built on.
 */
namespace ores::telemetry::database::repository {}

#endif
