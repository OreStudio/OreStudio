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
#ifndef ORES_DATABASE_REPOSITORY_DB_TYPES_HPP
#define ORES_DATABASE_REPOSITORY_DB_TYPES_HPP

#include <chrono>

namespace ores::database::repository {

/**
 * @brief Canonical database timestamp type: the instant a timestamptz column
 * holds.
 *
 * The row an entity holds carries the instant itself, not its text. The layer
 * reads and binds it in the binary format, which counts microseconds from
 * 2000-01-01 UTC, and the mapper copies it to the domain member with no
 * conversion. All sessions are forced to UTC by every transaction, so an
 * instant has one meaning whatever the host machine's timezone.
 *
 * The database's infinity, which stands for the bitemporal never, is
 * std::chrono::system_clock::time_point::max(): the binary codec reads it as
 * 9999-12-31 23:59:59 and writes it back, and pq::text does the same in the
 * text format a hand-written query uses.
 */
using db_timestamp = std::chrono::system_clock::time_point;

}

#endif
