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
#ifndef ORES_TELEMETRY_DATABASE_HPP
#define ORES_TELEMETRY_DATABASE_HPP

/**
 * @brief PostgreSQL persistence for the telemetry records.
 *
 * Stores the log entries, the service heartbeats and the NATS server and
 * stream samples in the component's own tables, and reads them back for the
 * query handlers.
 *
 * Its sub-namespaces:
 * - @b repository: the entities, mappers and the repository that read and
 *   write the four tables.
 * - @b log: the sink that writes a log record straight to the database,
 *   beside the Boost.Log front end the core part owns.
 */
namespace ores::telemetry::database {}

#endif
