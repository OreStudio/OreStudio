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
#ifndef ORES_DATABASE_DOMAIN_EXCEPTIONS_HPP
#define ORES_DATABASE_DOMAIN_EXCEPTIONS_HPP

#include <stdexcept>
#include <string>

namespace ores::database {

/**
 * @brief Thrown when a database connection cannot be established.
 *
 * Distinct from std::runtime_error so callers (service main functions)
 * can catch it specifically and return exit_code::db_connection_failed
 * rather than the generic exit_code::general_error.
 */
class db_connection_exception : public std::runtime_error {
public:
    explicit db_connection_exception(const std::string& msg)
        : std::runtime_error(msg) {}
};

}

#endif
