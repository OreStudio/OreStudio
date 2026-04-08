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

#include "sqlgen/Timestamp.hpp"

namespace ores::database::repository {

/**
 * @brief Canonical database timestamp type.
 *
 * All timestamptz columns map to this type. The format matches PostgreSQL's
 * output when the session timezone is UTC: "YYYY-MM-DD HH:MM:SS".
 * DB sessions are forced to UTC in tenant_aware_pool::acquire() so this
 * format is always correct regardless of the host machine timezone.
 */
using db_timestamp = sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">;

}

#endif
