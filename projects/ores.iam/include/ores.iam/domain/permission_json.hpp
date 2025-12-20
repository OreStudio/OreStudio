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
#ifndef ORES_ACCOUNTS_DOMAIN_PERMISSION_JSON_HPP
#define ORES_ACCOUNTS_DOMAIN_PERMISSION_JSON_HPP

#include <string>
#include <vector>
#include "ores.iam/domain/permission.hpp"

namespace ores::iam::domain {

/**
 * @brief Dumps the permission object to a stream in JSON format.
 */
/**@{*/
std::string convert_to_json(const permission& v);
std::string convert_to_json(const std::vector<permission>& v);
/**@}*/

}

#endif
