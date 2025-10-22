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
#include "ores.accounts/repository/logins_entity.hpp"

namespace ores::accounts::repository {

std::ostream& operator<<(std::ostream& s, const logins_entity& v) {
    s << "logins_entity{account_id=" << v.account_id.value()
      << ", last_ip=" << v.last_ip
      << ", last_attempt_ip=" << v.last_attempt_ip
      << ", failed_logins=" << v.failed_logins
      << ", locked=" << v.locked
      << ", last_login=" << v.last_login.str()
      << ", online=" << v.online
      << "}";
    return(s);
}

}
