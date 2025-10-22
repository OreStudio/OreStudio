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
#include <ostream>
#include <boost/uuid/uuid_io.hpp>
#include "ores.accounts/repository/account_entity.hpp"

namespace ores::accounts::repository {

std::ostream& operator<<(std::ostream& s, const account_entity& v) {
    s << "account_entity{id=" << v.id.value()
      << ", version=" << v.version
      << ", username=" << v.username
      << ", email=" << v.email
      << ", is_admin=" << v.is_admin
      << ", modified_by=" << v.modified_by
      << ", valid_from=" << v.valid_from.str()
      << ", valid_to=" << v.valid_to.str()
      << "}";
    return(s);
}

}
