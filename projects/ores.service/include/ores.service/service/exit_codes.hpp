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
#ifndef ORES_SERVICE_SERVICE_EXIT_CODES_HPP
#define ORES_SERVICE_SERVICE_EXIT_CODES_HPP

namespace ores::service::service {

/**
 * @brief Well-known process exit codes for ORE Studio services.
 *
 * Services must exit with these values so the controller can diagnose
 * failures without parsing log files. Add new codes here as new failure
 * categories are discovered; never reuse an existing value.
 */
enum class exit_code : int {
    ok = 0,                     ///< Clean shutdown (normal or graceful signal)
    general_error = 1,          ///< Unclassified runtime error
    config_error = 2,           ///< Missing / invalid configuration
    db_connection_failed = 3,   ///< Could not acquire a PostgreSQL connection
    nats_connection_failed = 4, ///< Could not connect to NATS
    startup_timeout = 5,        ///< Timed out waiting for a dependency
    auth_error = 6,             ///< Authentication or authorisation failure
};

}

#endif
