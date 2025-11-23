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
#include "ores.accounts/domain/login_info_table.hpp"

#include <fort.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <iomanip>
#include <sstream>

namespace ores::accounts::domain {

std::string convert_to_table(const std::vector<login_info>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header << "Account ID" << "Last IP" << "Last Attempt IP"
          << "Failed" << "Locked" << "Online" << "Last Login" << fort::endr;

    for (const auto& li : v) {
        std::string locked_status = li.locked ? "Y" : "N";
        std::string online_status = li.online ? "Y" : "N";

        // Format the timestamp
        auto time = std::chrono::system_clock::to_time_t(li.last_login);
        std::stringstream ss;
        ss << std::put_time(std::localtime(&time), "%Y-%m-%d %H:%M:%S");

        table << boost::uuids::to_string(li.account_id)
              << li.last_ip.to_string()
              << li.last_attempt_ip.to_string()
              << li.failed_logins
              << locked_status
              << online_status
              << ss.str()
              << fort::endr;
    }
    return table.to_string();
}

}
